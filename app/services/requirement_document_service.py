import asyncio
from sqlalchemy.orm import Session, selectinload
from sqlalchemy import desc, select
from typing import List, Optional, Dict, Any
import json
from openai import APIError
import re
import subprocess
import tiktoken

from app.models.project_model import Project
from app.models.procedure_model import Procedure
from app.models.m204_file_model import M204File
# M204Field is not directly used in queries here as it was removed

from app.models.m204_variable_model import M204Variable
# from app.models.image_statement_model import ImageStatement # Removed
from app.models.procedure_call_model import ProcedureCall
from app.models.dd_statement_model import DDStatement
from app.models.requirement_document_model import RequirementDocument
from app.models.input_source_model import InputSource

from app.schemas.requirement_document_schema import (
    RequirementGenerationOptionsSchema,
    RequirementDocumentCreateSchema,
    RequirementDocumentResponseSchema
)
from app.schemas.m204_analysis_schema import ( # For serializing nested data
    M204ProcedureResponseSchema,
    M204FileResponseSchema,
    M204VariableResponseSchema,
    M204ProcedureCallResponseSchema
)
from app.schemas.generic_analysis_schema import DDStatementResponseSchema

from app.config.llm_config import llm_config
from app.utils.logger import log
from fastapi import HTTPException, status
from datetime import datetime
import os

MAX_CONCURRENT_LLM_CALLS = 5   #for generating sections
MAX_MERMAID_FIX_ATTEMPTS = 3 # Max attempts to fix a diagram

_current_script_dir = os.path.dirname(os.path.abspath(__file__))
# Go up two levels to get to the 'server' directory, which is our project root
_server_root_dir = os.path.abspath(os.path.join(_current_script_dir, '..', '..'))
# Construct the full path to the validator script
MERMAID_VALIDATOR_PATH = os.path.join(_server_root_dir, "mermaid_validator.js")



def count_tokens(text: str) -> int:
    enc = tiktoken.get_encoding("cl100k_base")
    return len(enc.encode(text))

def _run_mermaid_validator_sync(mermaid_code: str) -> subprocess.CompletedProcess:
    """Synchronous helper to run the validator in a separate thread."""
    return subprocess.run(
        ['node', MERMAID_VALIDATOR_PATH],
        input=mermaid_code.encode('utf-8'),
        capture_output=True,
        check=False, # Don't raise exception on non-zero exit code
        cwd=_server_root_dir
    )

# --- Helper function to serialize SQLAlchemy models to dicts for LLM prompt ---
def _serialize_model_instance(instance: Any, schema_class: Optional[Any] = None) -> Dict[str, Any]:
    """
    Serializes a SQLAlchemy model instance to a dictionary.
    If a Pydantic schema is provided, it uses that for serialization.
    """
    if schema_class:
        try:
            return schema_class.model_validate(instance).model_dump(exclude_none=True)
        except Exception as e:
            log.warning(f"Failed to serialize instance with schema {schema_class.__name__}: {e}. Falling back to basic serialization.")

    data = {}
    for column in instance.__table__.columns:
        value = getattr(instance, column.name)
        if isinstance(value, datetime):
            data[column.name] = value.isoformat()
        else:
            data[column.name] = value
    return data

async def _structure_m204_loop_logic(loop_content_string: str) -> Optional[Dict[str, Any]]:
    """
    Uses an LLM to parse a raw M204 code string into a structured JSON "Operation Tree".
    This is a pre-processing step to simplify the final document generation.
    """
    if not loop_content_string or not llm_config._llm:
        return None

    log.info("Attempting to structure complex M204 loop logic into JSON...")

    prompt = f"""
You are an expert M204 code parser. Your task is to analyze the provided M204 code snippet and convert its logical flow into a structured JSON object.

The JSON object must be an array of 'operations'. Each operation can be one of two types: `action` or `conditional`.

- An `action` represents a single command like `CALL`, `PRINT`, `FIND`, or a variable assignment. It must have the fields: `type: "action"`, `statement: "The original code line"`, and `description: "A plain-language summary of the action"`.
- A `conditional` represents an `IF/ELSE IF/ELSE` block. It must have the fields: `type: "conditional"` and `branches`. The `branches` field is an array where each object represents one branch (`IF`, `ELSE IF`, or `ELSE`).
- Each `branch` object must have a `condition: "The condition string, e.g., 'IF %STATUS EQ 'A'' or simply 'ELSE'"` and an `operations` field, which is a nested array following these same rules recursively.

Your response must contain *only* the raw JSON object and nothing else. Do not wrap it in markdown, and do not add any explanations.

M204 Code to Parse:
```m204
{loop_content_string}
```
"""
    try:
        completion_response = await llm_config._llm.acomplete(prompt=prompt)
        if completion_response and completion_response.text:
            # The response should be a JSON string, so we parse it.
            json_text = completion_response.text.strip()
            # Handle cases where the LLM might still wrap the output in markdown
            if json_text.startswith("```json"):
                json_text = json_text[7:]
            if json_text.endswith("```"):
                json_text = json_text[:-3]
            
            structured_logic = json.loads(json_text)
            log.info("Successfully structured M204 loop logic into JSON.")
            return structured_logic
        else:
            log.warning("LLM returned no content for M204 loop structuring.")
            return None
    except json.JSONDecodeError as e_json:
        log.error(f"Failed to decode JSON from LLM response for loop structuring: {e_json}. Response text: '{completion_response.text[:500]}'")
        return None
    except Exception as e:
        log.error(f"An unexpected error occurred during M204 loop structuring: {e}", exc_info=True)
        return None

async def _fetch_project_data_for_llm(db: Session, project_id: int, options: RequirementGenerationOptionsSchema) -> Dict[str, Any]:
    """
    Fetches all relevant data for a project and pre-processes complex parts for the LLM prompt.
    """
    project_data: Dict[str, Any] = {}

    # Project Info
    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")
    project_data["project_info"] = _serialize_model_instance(project)

    # Always fetch input sources to check for summaries and main loop content
    input_sources = db.query(InputSource).filter(InputSource.project_id == project_id).all()
    source_summaries = []
    main_loop_content = None # Variable to hold the main loop content
    main_loop_source_filename = None # To identify which file the loop came from

    for src in input_sources:
        # Unconditionally check for and capture the main processing loop content from any M204 source
        log.debug(f" details about src: {src}")
        if src.source_type == 'm204' and src.main_processing_loop_content:
            log.debug("checking for m204 main processing loop")
            if not main_loop_content: # Take the first one found
                main_loop_content = src.main_processing_loop_content
                main_loop_source_filename = src.original_filename or f"InputSource ID {src.input_source_id}"
            else:
                log.warning(f"Multiple main_processing_loop_content found for project {project_id}. Using the first one from source '{main_loop_source_filename}'.")

        # Conditionally gather summaries if the option is selected
        if options.include_project_overview:
            summary_item = {
                "original_filename": src.original_filename,
                "source_type": src.source_type,
                "jcl_detailed_description": getattr(src, 'jcl_detailed_description', None), # Use getattr for safety
                "m204_detailed_description": getattr(src, 'm204_detailed_description', None) # Use getattr for safety
            }
            # Only add if there's a relevant description
            if summary_item["jcl_detailed_description"] or summary_item["m204_detailed_description"]:
                source_summaries.append(summary_item)

    # Add summaries to project_data if they were gathered
    if source_summaries:
        project_data["source_file_llm_summaries"] = source_summaries
    
    # Process and add the main loop content if found, regardless of other options
    if main_loop_content:
        log.info(f"Found main processing loop content from '{main_loop_source_filename}', structuring for LLM prompt.")
        structured_logic = await _structure_m204_loop_logic(main_loop_content)
        project_data["main_processing_loop"] = {
            "source_filename": main_loop_source_filename,
            "raw_content": main_loop_content,
            "structured_logic": structured_logic
        }

    # M204 Procedures
    if options.include_procedures:
        procedures_query = db.query(Procedure).filter(Procedure.project_id == project_id)
        if options.include_procedure_variables:
            procedures_query = procedures_query.options(
                selectinload(Procedure.variables_in_procedure)
            )
        procedures = procedures_query.all()
        project_data["procedures"] = [M204ProcedureResponseSchema.model_validate(p).model_dump(exclude_none=True) for p in procedures]

    # M204 Files
    if options.include_files:
        m204_files_query = db.query(M204File).filter(M204File.project_id == project_id)
        m204_files_results = m204_files_query.all()
        
        # Convert to dictionaries. The loop processing is now removed from here.
        m204_files_data = [M204FileResponseSchema.model_validate(f).model_dump(exclude_none=True) for f in m204_files_results]

        project_data["m204_files"] = m204_files_data

    # Global/Public M204 Variables
    if options.include_global_variables:
        global_vars = db.query(M204Variable).filter(
            M204Variable.project_id == project_id,
            M204Variable.procedure_id.is_(None) # Identifies global/public variables
        ).all()
        project_data["global_variables"] = [M204VariableResponseSchema.model_validate(v).model_dump(exclude_none=True) for v in global_vars]

    # JCL DD Statements
    if options.include_jcl_dd_statements:
        dd_statements = db.query(DDStatement).join(InputSource, DDStatement.input_source_id == InputSource.input_source_id)\
            .filter(InputSource.project_id == project_id).all()
        project_data["dd_statements"] = [DDStatementResponseSchema.model_validate(dd).model_dump(exclude_none=True) for dd in dd_statements]

    # Procedure Calls
    if options.include_procedure_calls:
        procedure_calls = db.query(ProcedureCall).filter(ProcedureCall.project_id == project_id).all()
        project_data["procedure_calls"] = [M204ProcedureCallResponseSchema.model_validate(pc).model_dump(exclude_none=True) for pc in procedure_calls]

    return project_data

def _construct_llm_prompt_content(project_data: Dict[str, Any]) -> str:
    """
    Constructs the detailed data dump part of the LLM prompt.
    Formats the fetched data into a readable structure for the LLM.
    This function now receives a targeted subset of data for a specific section.
    """
    content_parts = []

    # Source File Summaries
    if "source_file_llm_summaries" in project_data and project_data["source_file_llm_summaries"]:
        content_parts.append("Source File LLM-Generated Summaries:")
        for summary_info in project_data["source_file_llm_summaries"]:
            filename = summary_info.get('original_filename', 'N/A')
            src_type = summary_info.get('source_type', 'N/A')
            content_parts.append(f"  - File: {filename} (Type: {src_type})")
            if summary_info.get('jcl_detailed_description'):
                content_parts.append(f"    JCL Summary: {summary_info['jcl_detailed_description']}")
            if summary_info.get('m204_detailed_description'):
                content_parts.append(f"    M204 Summary: {summary_info['m204_detailed_description']}")
        content_parts.append("\n")

    # Main Processing Loop
    if "main_processing_loop" in project_data and project_data["main_processing_loop"]:
        loop_data = project_data["main_processing_loop"]
        source_file = loop_data.get('source_filename', 'an M204 source file')
        content_parts.append(f"Main Processing Loop (from {source_file}):")
        structured_loop = loop_data.get("structured_logic")
        if structured_loop:
            log.debug("Including structured_loop_logic in prompt.")
            content_parts.append("  Structured Logic (JSON):")
            content_parts.append("  ```json")
            content_parts.append(json.dumps(structured_loop, indent=2))
            content_parts.append("  ```")
        elif loop_data.get("raw_content"):
            log.debug("Falling back to raw main_processing_loop_content in prompt.")
            content_parts.append("  Raw M204 Content:")
            content_parts.append("  ```m204")
            content_parts.append(loop_data.get('raw_content'))
            content_parts.append("  ```")
        content_parts.append("\n")

    # M204 Procedures
    if "procedures" in project_data and project_data["procedures"]:
        content_parts.append("M204 Procedures Data:")
        for proc in project_data["procedures"]:
            proc_details = [
                f"  - Procedure Name: {proc.get('m204_proc_name', 'N/A')}",
                f"    Type: {proc.get('m204_proc_type', 'N/A')}",
                f"    Parameters String: {proc.get('m204_parameters_string', 'None')}",
                f"    Target COBOL Program: {proc.get('target_cobol_function_name', 'N/A')}",
            ]
            if proc.get('summary'):
                proc_details.append(f"    Summary: {proc.get('summary', 'No summary available.')}")
            proc_details.append(f"    Content Snippet (first 200 chars): {proc.get('procedure_content', 'N/A')[:200]}...")
            if proc.get("variables_in_procedure"):
                proc_details.append("    Variables Defined in Procedure:")
                for var in proc["variables_in_procedure"]:
                    proc_details.append(f"      - Variable Name: {var.get('variable_name')}, Type: {var.get('variable_type')}, Scope: {var.get('scope')}, COBOL Mapped Name: {var.get('cobol_mapped_variable_name', 'N/A')}")
            content_parts.extend(proc_details)
        content_parts.append("\n")

    # M204 Files
    if "m204_files" in project_data and project_data["m204_files"]:
        content_parts.append("M204 File Definitions Data:")
        for f_data in project_data["m204_files"]:
            log.debug(f"f_data: {f_data}")
            file_details = [
                f"  - File Name: {f_data.get('m204_file_name', 'N/A')}",
                f"    M204 Attributes: {f_data.get('m204_attributes', 'N/A')}",
                f"    Is Database File: {f_data.get('is_db_file', 'Unknown')}",
                f"    Target VSAM Dataset Name: {f_data.get('target_vsam_dataset_name', 'N/A')}",
                f"    Target VSAM Type: {f_data.get('target_vsam_type', 'N/A')}"
            ]
            file_def_json = f_data.get("file_definition_json")
            has_printed_fields_for_this_file = False 
            if file_def_json and isinstance(file_def_json, dict):
                file_type = file_def_json.get("file_type")
                db_fields_data = None
                db_header_text = ""
                if file_type == "db_file":
                    db_fields_data = file_def_json.get("fields")
                    db_header_text = "    Fields (from DB definition):"
                elif file_type == "mixed":
                    db_part = file_def_json.get("db_file_definition")
                    if isinstance(db_part, dict):
                        db_fields_data = db_part.get("fields")
                        db_header_text = "    Fields (from DB part of mixed definition):"
                if isinstance(db_fields_data, dict) and db_fields_data:
                    file_details.append(db_header_text)
                    has_printed_fields_for_this_file = True
                    for field_name, field_attributes in db_fields_data.items():
                        attrs_str = str(field_attributes) if not isinstance(field_attributes, str) else field_attributes
                        file_details.append(f"      - Field Name: {field_name}, Attributes: {attrs_str}")
                image_definitions_list = None
                image_header_prefix_text = ""
                if file_type == "flat_file":
                    image_definitions_list = file_def_json.get("image_definitions")
                    image_header_prefix_text = "    Fields (from flat file IMAGE"
                elif file_type == "mixed":
                    flat_part = file_def_json.get("flat_file_definition")
                    if isinstance(flat_part, dict):
                        image_definitions_list = flat_part.get("image_definitions")
                    image_header_prefix_text = "    Fields (from flat file part of mixed IMAGE"
                if isinstance(image_definitions_list, list):
                    for image_def in image_definitions_list:
                        if isinstance(image_def, dict):
                            image_name = image_def.get('image_name', 'Unnamed')
                            fields_in_image = image_def.get("fields")
                            if isinstance(fields_in_image, list) and fields_in_image:
                                file_details.append(f"{image_header_prefix_text} '{image_name}'):") 
                                has_printed_fields_for_this_file = True
                                for field_item in fields_in_image:
                                    if isinstance(field_item, dict):
                                        file_details.append(f"      - Field Name: {field_item.get('name', 'N/A')}, Attributes: {field_item.get('attributes', 'N/A')}")
                                    else:
                                        log.warning(f"Item in IMAGE fields list is not a dict for file {f_data.get('m204_file_name')}, image '{image_name}': {type(field_item)}. Data: {str(field_item)[:100]}")
                                        file_details.append(f"      - Field Data (raw): {str(field_item)}")
                if not has_printed_fields_for_this_file and "fields" in file_def_json:
                    generic_fields = file_def_json.get("fields")
                    if isinstance(generic_fields, list) and generic_fields: 
                        file_details.append("    Fields (from JSON definition - list):")
                        for field_item in generic_fields:
                            if isinstance(field_item, dict):
                                file_details.append(f"      - Field Name: {field_item.get('name', 'N/A')}, Attributes: {field_item.get('attributes', 'N/A')}")
                            else:
                                log.warning(f"Item in generic 'fields' list is not a dict for file {f_data.get('m204_file_name')}: {type(field_item)}. Data: {str(field_item)[:100]}")
                                file_details.append(f"      - Field Data (raw): {str(field_item)}")
                    elif isinstance(generic_fields, dict) and generic_fields: 
                        file_details.append("    Fields (from JSON definition - dictionary):")
                        for field_name, field_attributes in generic_fields.items():
                            attrs_str = str(field_attributes) if not isinstance(field_attributes, str) else field_attributes
                            file_details.append(f"      - Field Name: {field_name}, Attributes: {attrs_str}")
                    elif generic_fields: 
                        log.warning(f"Generic 'fields' in file_definition_json is of unexpected type {type(generic_fields)} for file {f_data.get('m204_file_name')}: {str(generic_fields)[:100]}")
                        file_details.append(f"    Fields Data (raw, unexpected type): {str(generic_fields)}")
            content_parts.extend(file_details)
        content_parts.append("\n")

    # Global Variables
    if "global_variables" in project_data and project_data["global_variables"]:
        content_parts.append("Global/Public M204 Variables Data:")
        for var in project_data["global_variables"]:
            attributes_json = json.dumps(var.get('attributes')) if var.get('attributes') else 'None'
            content_parts.append(f"  - Variable Name: {var.get('variable_name')}, Type: {var.get('variable_type')}, Scope: {var.get('scope')}, COBOL Mapped Name: {var.get('cobol_mapped_variable_name', 'N/A')}, Attributes: {attributes_json}")
        content_parts.append("\n")

    # JCL DD Statements
    if "dd_statements" in project_data and project_data["dd_statements"]:
        content_parts.append("JCL DD Statements Data:")
        for dd in project_data["dd_statements"]:
            content_parts.append(f"  - DD Name: {dd.get('dd_name')}, DSN: {dd.get('dsn', 'N/A')}, Disposition: {dd.get('disposition', 'N/A')}, Job Name: {dd.get('job_name', 'N/A')}, Step Name: {dd.get('step_name', 'N/A')}")
        content_parts.append("\n")

    # Procedure Calls
    if "procedure_calls" in project_data and project_data["procedure_calls"]:
        content_parts.append("Procedure Call Relationships Data:")
        # Create a quick lookup for procedure names from their IDs
        proc_name_map = {p['proc_id']: p.get('m204_proc_name', 'N/A') for p in project_data.get("procedures", [])}
        
        for call in project_data["procedure_calls"]:
            calling_proc_id = call.get('calling_procedure_id')
            calling_proc_name = proc_name_map.get(calling_proc_id, f"ID {calling_proc_id}")
            if calling_proc_name == f"ID {calling_proc_id}":
                 log.debug(f"Calling procedure ID {calling_proc_id} not found in pre-loaded procedure data for call ID {call.get('procedure_call_id')}")

            content_parts.append(f"  - Calling Procedure Name: {calling_proc_name}, Called Procedure Name: {call.get('called_procedure_name')}, Line Number: {call.get('line_number')}, Is External: {call.get('is_external')}")
        content_parts.append("\n")

    return "\n".join(content_parts)


def json_to_mermaid(data: dict) -> str:
    """
    Convert a flowchart JSON (with 'nodes' and 'edges') into Mermaid code.
    Adds validation and error handling for missing fields and unknown node types.
    """
    lines = ["graph TD"]
    delimiters = {
        "start": lambda node_id, node_label: f"{node_id}(({node_label}))",
        "end":   lambda node_id, node_label: f"{node_id}(({node_label}))",
        "action":   lambda node_id, node_label: f"{node_id}[{node_label}]",
        "decision": lambda node_id, node_label: f"{node_id}{{{node_label}}}",
        "io":       lambda node_id, node_label: f"{node_id}[/{node_label}/]",
    }

    # Validate nodes
    for node in data.get("nodes", []):
        node_id = node.get("id")
        node_label = node.get("label")
        node_type = node.get("type")
        if not node_id or not node_label or not node_type:
            # Log and skip invalid node
            log.warning(f"Skipping node with missing fields: {node}")
            continue
        if node_type not in delimiters:
            log.warning(f"Unknown node type '{node_type}' for node '{node_id}'. Skipping node.")
            continue
        lines.append("  " + delimiters[node_type](node_id, node_label))

    # Validate edges
    for edge in data.get("edges", []):
        src_id = edge.get("from")
        dst_id = edge.get("to")
        cond = edge.get("condition")
        if not src_id or not dst_id:
            log.warning(f"Skipping edge with missing 'from' or 'to': {edge}")
            continue
        arrow = f"{src_id} -->"
        if cond is not None:
            arrow += f"|{cond}|"
        arrow += f" {dst_id}"
        lines.append("  " + arrow)

    return "\n".join(lines)

def _get_sections_config() -> List[Dict[str, Any]]:
    """
    Defines the structure, instructions, and data dependencies for each section.
    For diagram sections, instructs the LLM to emit only JSON (not Mermaid).
    """
    all_data_keys = {
        "project_info", "source_file_llm_summaries", "main_processing_loop",
        "procedures", "m204_files", "global_variables", "dd_statements", "procedure_calls"
    }
    full_context_data = list(all_data_keys)

    # --- JSON diagram prompt ---
    diagram_json_prompt = """
Generate a JSON structure that describes the flowchart for this section.
Use exactly this schema:

{
  "nodes": [
    {
      "id":   "unique_identifier",          
      "type": "start|action|decision|io|end",
      "label":"Your node label"
    },
    …
  ],
  "edges": [
    {
      "from":      "source_node_id",
      "to":        "target_node_id",
      "condition": "Yes|No|null"
    },
    …
  ]
}

Requirements:
- Do NOT emit any Mermaid code—only JSON.
- Every node used in edges must appear in nodes[].
- Decisions (“type”: “decision”) will have edges whose “condition” is “Yes” or “No”.
- Other edges should set “condition” to null.
- **For procedures, always use the procedure name (not numeric ID) as the node label.**
- Follow this example (you don’t need to copy the example nodes/edges, just the schema):

Example snippet:
{
  "nodes":[
    { "id":"A","type":"start","label":"Start Loop" },
    { "id":"B","type":"decision","label":"Is X>0?" }
  ],
  "edges":[
    { "from":"A","to":"B","condition":null }
  ]
}
"""

    return [
        {
            "id": "program_overview",
            "title": "## 1. Program Overview",
            "required_data": ["source_file_llm_summaries", "m204_files"],
            "instructions": """
(Provide a high-level technical overview of the entire program’s purpose and architecture based on the 
‘m204_detailed_description’ fields from all InputSource records. Focus on the program as a whole. 
Do not dive into the details of any single module or business context.)

After the overview, include:
- A bullet list of all files involved in the program, as found in the M204 file definitions (list file names).
- A separate bullet list of all files flagged as Model 204 database files (where is_db_file = True).
"""
        },
        {
            "id": "conceptual_data_model",
            "title": "### 1.1. Conceptual Data Model Diagram",
            "required_data": ["m204_files", "dd_statements", "procedures", "procedure_calls", "source_file_llm_summaries"],
            "instructions": diagram_json_prompt + """
Describe the high-level conceptual data flow and control flow for the system, including key processes (JCL jobs, M204 procedures), major data stores (M204 files, VSAM datasets), and their relationships. Use the provided system data. If insufficient, emit an empty JSON object.
"""
        },
        {
            "id": "main_processing_loop",
            "title": "## 2. Main Processing Loop",
            "required_data": ["main_processing_loop"],
            "instructions": """
The input provides the main processing loop as a block of M204 code (not as structured JSON). Please:

- Begin with a concise summary paragraph describing the overall purpose and function of the main processing loop, based on your analysis of the code.
- Then, analyze the M204 code directly, outlining the sequence of operations and major conditional logic in a step-by-step, hierarchical bullet-point format.
    * For each significant operation (e.g., FIND, CALL, PRINT, variable assignment), state the code line and provide a plain-language explanation.
    * For each conditional (e.g., IF, ELSE IF, ELSE), clearly state the condition, then describe the logic within each branch, using indentation to show nesting and sequence.
    * Dont include statements like print(***) or anything like that, the goal is to capture every rule or logic that is available
- Explicitly identify the primary inputs, outputs, and any key decision points or repeated operations.
"""
        },
        {
            "id": "main_processing_loop_diagram",
            "title": "### 2.1. Main Processing Loop Flowchart",
            "required_data": ["main_processing_loop"],
            "instructions": diagram_json_prompt + """
Describe the flowchart for the main processing loop, using the structured logic if available. If only raw code is provided, analyze and emit the JSON structure for the flowchart. If insufficient, emit an empty JSON object.
"""
        },
        {
            "id": "m204_procedures",
            "title": "## 3. M204 Procedures",
            "required_data": ["procedures", "source_file_llm_summaries"],
            "instructions": """
(Based on 'M204 Procedures Data' and relevant 'Source File LLM-Generated Summaries' for M204 files:
- For each procedure, write a descriptive paragraph. This paragraph should cover its name, type, and its target COBOL program name for generation (if specified).
- If a procedure summary is available in the input, incorporate it into the descriptive paragraph.
- Detail parameters and any variables defined within the procedure using bullet points *underneath* the main descriptive paragraph for that procedure.
- For example:
  "The procedure **SAMPLE_PROC** is an **ONLINE** type procedure, found in the M204 source file `SOURCEA.M204`. It is designated to be modernized into the COBOL program **SAMPLE-PROG-FUNC**. The primary function of this procedure appears to be real-time data modification based on user input.
  * Parameters: `ACCOUNT_NUMBER, UPDATE_DATA`
  * Variables defined in procedure:
    * `%OLD_VALUE` (Type: `STRING`, Scope: `LOCAL`, COBOL Mapped Name: `WS-OLD-VAL`)
    * `%NEW_VALUE` (Type: `STRING`, Scope: `LOCAL`, COBOL Mapped Name: `WS-NEW-VAL`)"
- Conclude this section with a brief summary paragraph discussing any observed overall purpose or common themes across the procedures, informed by individual procedure details and the broader M204 source file summaries.)
"""
        },
        {
            "id": "m204_file_definitions",
            "title": "## 4. M204 File Definitions",
            "required_data": ["m204_files", "source_file_llm_summaries"],
            "instructions": """
Based on the 'M204 File Definitions and Main Processing Loops Data' and any relevant 'Source File LLM-Generated Summaries' for M204 files:

1. For each M204 file, write a descriptive paragraph covering:
- File name in **bold**.
- The exact DEFINE statement or M204 attributes (e.g. `(attribute_list)`).
- Whether it is flagged as a database file under Model 204.
- Its target VSAM dataset name in **bold** and type (e.g. `KSDS`, `RRDS`), or `N/A` if none.
- A brief note on its purpose or role, drawing on any available LLM-generated summary.

2. If `file_definition_json` includes field definitions, list them as bullet points under “Fields:”:
* `FIELD_NAME` (Attributes: `attr1, attr2, …`)
Otherwise, state:
“Detailed field structure not available in the provided data for this file.”

3. After all files, conclude with a summary paragraph that:
- Groups the files into general categories (e.g., master/reference data, transactional staging, system/utility datasets, control/audit logs).
- Highlights the types of data each category manages, inferred from file names, attributes, and source summaries.
"""
        },
        {
            "id": "m204_file_vsam_jcl_diagram",
            "title": "### 4.1. M204 File and Associated VSAM/JCL Diagram",
            "required_data": ["m204_files", "dd_statements"],
            "instructions": diagram_json_prompt + """
Describe the relationships between M204 files, VSAM datasets, and JCL DD statements. Emit only the JSON structure for the diagram. If insufficient, emit an empty JSON object.
"""
        },
        {
            "id": "global_variables",
            "title": "## 5. Global/Public M204 Variables",
            "required_data": ["global_variables"],
            "instructions": """
(Based on 'Global/Public M204 Variables Data':
- Begin with an introductory paragraph about the role of global/public variables in the system.
- Present all global/public variables in a Markdown table with columns: Variable Name, Type, Scope, COBOL Mapped Name, Attributes.
- Follow the table with a paragraph further explaining the potential collective purpose or usage patterns of these global variables based on their characteristics.)
"""
        },
        {
            "id": "jcl_dd_statements",
            "title": "## 6. JCL DD Statements",
            "required_data": ["dd_statements", "source_file_llm_summaries"],
            "instructions": """
        (Based on 'JCL DD Statements Data' and relevant 'Source File LLM-Generated Summaries' for JCL files:
        - Start with a paragraph describing the role of JCL DD statements in interfacing the M204 application with datasets. Refer to the LLM-generated summaries for the JCL files to provide context on the overall purpose of the JCLs containing these DD statements.
        - Present all DD statements in a Markdown table with columns: DD Name, DSN, Disposition, Job Name, Step Name.
        - Conclude with a paragraph highlighting any DD statements that seem particularly critical (e.g., for primary input/output files, connections to other systems), considering their role within the summarized JCL job flow.)
        """
        },
        {
            "id": "jcl_job_step_flow_diagram",
            "title": "### 6.1. JCL Job/Step Flow Diagram",
            "required_data": ["dd_statements"],
            "instructions": diagram_json_prompt + """
Describe the JCL job and step flow, grouping DD statements under steps and steps under jobs. Emit only the JSON structure for the diagram. If insufficient, emit an empty JSON object.
"""
        },
        {
            "id": "procedure_call_flow",
            "title": "## 7. Procedure Call Flow",
            "required_data": ["procedure_calls", "procedures"],
            "instructions": """
(Based on 'Procedure Call Relationships Data':
- Describe the procedure call flow using narrative paragraphs to explain major sequences or interactions.
- Supplement with bullet points to list specific call relationships, indicating the calling procedure, the called procedure, the line number of the call, and whether it's an external call.
- Focus on making the control flow and dependencies clear. Try to identify and describe any main processing sequences, critical execution paths, or highly interconnected modules.)
"""
        },
        {
            "id": "procedure_call_diagram",
            "title": "### 7.1. Procedure Call Diagram",
            "required_data": ["procedure_calls"],
            "instructions": diagram_json_prompt + """
Describe the direct call relationships between procedures.

**Important:** Always use the procedure names (not numeric IDs) for all nodes in the diagram, including the root/start node and all called procedures. Do not use any numeric IDs anywhere in the diagram.

Emit only the JSON structure for the diagram. If insufficient, emit an empty JSON object.
"""
        },
        {
            "id": "external_interfaces",
            "title": "## 8. External Interfaces",
            "required_data": ["dd_statements", "m204_files", "procedure_calls"],
            "instructions": """
(Infer from 'JCL DD Statements Data' (DSNs), 'M204 File Definitions Data' (attributes suggesting external links), and 'Procedure Call Relationships Data' (`is_external` flag).
- Describe in paragraph form any identified external systems, datasets, or interfaces that the M204 application interacts with. Explain the nature of these interactions if discernible.)
"""
        },
        {
            "id": "non_functional_requirements",
            "title": "## 9. Non-Functional Requirements",
            "required_data": full_context_data,
            "instructions": """
(Review all provided data for hints towards non-functional requirements.
- Describe any identified NFRs (e.g., performance considerations from file attributes, security aspects from field encryption, operational constraints) in paragraph form.
"""
        },
        {
            "id": "other_observations",
            "title": "## 10. Other Observations / Summary",
            "required_data": full_context_data,
            "instructions": """
(Provide a concluding summary of the system in narrative paragraphs. Highlight any overarching patterns, complexities, or notable observations that don't fit neatly into other sections. Address points from the 'Additional Instructions/Custom Section from User' here if not covered elsewhere.)
"""
        },
    ]



async def _validate_mermaid_code(mermaid_code: str, section_title: str = "") -> tuple[bool, Optional[str], Optional[int]]:
    """
    Validates Mermaid code using an external Node.js script.
    Returns (is_valid, error_message, error_line_number).
    This version uses asyncio.to_thread to be compatible with Windows.
    """
    if not mermaid_code.strip():
        return False, "Mermaid code is empty.", 0

    try:
        process = await asyncio.to_thread(_run_mermaid_validator_sync, mermaid_code)
        if process.returncode == 0:
            log.info(f"[Mermaid Validation][{section_title}] Mermaid validation successful.")
            return True, None, None
        else:
            error_output = process.stderr.decode('utf-8').strip()
            line_num_output = process.stdout.decode('utf-8').strip()
            error_line = int(line_num_output) if line_num_output.isdigit() else 0
            log.warning(f"[Mermaid Validation][{section_title}] Mermaid validation failed at line {error_line}. Error: {error_output}")
            return False, error_output, error_line
    except FileNotFoundError:
        log.error(f"[Mermaid Validation][{section_title}] Mermaid validator script not found at '{MERMAID_VALIDATOR_PATH}'. Please ensure Node.js is installed and the script is in the correct path.")
        return True, "Validator script not found, skipping validation.", None
    except Exception as e:
        log.error(f"[Mermaid Validation][{section_title}] An exception occurred during Mermaid validation: {e}", exc_info=True)
        return True, f"An exception occurred during validation: {e}, skipping validation.", None

async def _attempt_to_fix_mermaid_code(invalid_code: str, error_message: str, error_line: int, section_title: str = "") -> Optional[str]:
    """
    Uses the LLM to attempt to fix invalid Mermaid code based on a specific error.
    """
    if not llm_config._llm:
        return None

    log.info(f"[Mermaid Fix][{section_title}] Attempting to fix invalid Mermaid code with LLM. Error at line {error_line}.")

    lines = invalid_code.split('\n')
    start = max(0, error_line - 4)
    end = min(len(lines), error_line + 3)
    context_snippet = "\n".join(f"{i+1}: {line}" for i, line in enumerate(lines[start:end]))

    prompt = f"""
You are an expert in Mermaid.js syntax. The following Mermaid code is invalid.
Your task is to fix it based on the provided error message and context, without changing the meaning or structure of the diagram.
Your response must contain *only* the complete, corrected, raw Mermaid code inside a ```mermaid ... ``` block and nothing else.

**Error Message:**
{error_message}

**Error occurred near line {error_line}. Here is a snippet of the code around the error line (with line numbers):**
```
{context_snippet}
```

**Full Invalid Mermaid Code:**
```mermaid
{invalid_code}
```

Please provide the full, corrected Mermaid code.
"""
    try:
        completion_response = await llm_config._llm.acomplete(prompt=prompt)
        if completion_response and completion_response.text:
            fixed_code_match = re.search(r"```mermaid\n(.*?)\n```", completion_response.text, re.DOTALL)
            if fixed_code_match:
                fixed_code = fixed_code_match.group(1).strip()
                log.info(f"[Mermaid Fix][{section_title}] LLM provided a potential fix for the Mermaid code.")
                return fixed_code
            else:
                log.warning(f"[Mermaid Fix][{section_title}] LLM response for Mermaid fix did not contain a valid code block.")
                return None
    except Exception as e:
        log.error(f"[Mermaid Fix][{section_title}] An error occurred while asking LLM to fix Mermaid code: {e}", exc_info=True)
    return None

async def _generate_llm_section(
    section_id: str,
    section_title: str,
    section_instructions: str,
    formatted_data_for_prompt: str,
    custom_prompt_section: Optional[str]
) -> str:
    """
    Generates content for a single section of the requirements document using the LLM.
    For diagram sections, expects JSON, converts to Mermaid, and validates.
    """
    # If there are no instructions and no data, it's likely a title-only section.
    if not section_instructions.strip() and not formatted_data_for_prompt.strip():
        return section_title

    # List of diagram section IDs (update if you add more diagram sections)
    diagram_section_ids = {
        "conceptual_data_model",
        "main_processing_loop_diagram",
        "m204_file_vsam_jcl_diagram",
        "jcl_job_step_flow_diagram",
        "procedure_call_diagram"
    }

    prompt_for_section = f"""
You are a senior technical analyst and writer. Your task is to generate the content for *only* the following section of a Software Requirements Specification (SRS) document in Markdown format for an existing M204-based system.
The section content should be based *solely* on the structured data provided below. Do not invent information not present in the provided data.
Output *only* the Markdown for this specific section, starting with its heading.
Emphasize the use of **paragraphs for description and bullet points for lists of attributes or related items**. Tables can be used sparingly.

**Mermaid Diagram Generation Rules (if applicable):**
- You are a Mermaid.js expert.
- The instructions for this section will specify the required diagram type (e.g., `graph TD`, `graph LR`).
- Use clear, concise, and short labels for nodes and relationships to minimize syntax errors.
- Ensure all Mermaid syntax is correct and enclosed in a ```mermaid ... ``` fenced code block.
- If data is insufficient for a meaningful diagram, state this clearly under the section heading instead of outputting malformed code.

**Input Data (Summary of system components and relationships):**
--- BEGIN SYSTEM DATA ---
{formatted_data_for_prompt if formatted_data_for_prompt.strip() else "No specific data provided for this section."}
--- END SYSTEM DATA ---

{f"**Additional Instructions/Custom Section from User (consider these when generating content for this section):**\n{custom_prompt_section}\n" if custom_prompt_section else ""}

**Instructions for the section to generate:**
Your output must start *exactly* with the following heading line:
{section_title}

And then provide the content as described below:
{section_instructions}

When discussing JCL, M204 Procedures, or M204 Files (if relevant to this section), refer to any 'Source File LLM-Generated Summaries' provided in the input data to give broader context.
Do not add any preamble or explanation before the section's heading.
Ensure the entire output for this section is valid Markdown.
"""
    log.info(f"Generating section: '{section_title}' (ID: {section_id})")

    try:
        completion_response = await llm_config._llm.acomplete(prompt=prompt_for_section)
        if completion_response and completion_response.text:
            section_content = completion_response.text.strip()
            log.debug(f"Raw LLM response for section '{section_id}':\n{section_content}")

            # --- Diagram Section Handling ---
            if section_id in diagram_section_ids:
                # Try to extract JSON from the response
                json_text = section_content
                # Remove markdown fences if present
                if json_text.startswith("```json"):
                    json_text = json_text[7:]
                if json_text.endswith("```"):
                    json_text = json_text[:-3]
                json_text = json_text.strip()
                # Sometimes LLM may wrap in code block or add explanation, try to sanitize
                # Remove any heading/title lines
                if json_text.startswith(section_title):
                    json_text = json_text[len(section_title):].strip()
                # Remove any explanation before JSON
                json_start = json_text.find("{")
                if json_start > 0:
                    json_text = json_text[json_start:]
                # Parse JSON
                try:
                    diagram_json = json.loads(json_text)
                except Exception as e:
                    log.error(f"Failed to parse JSON for diagram section '{section_id}': {e}. Raw text: '{json_text[:500]}'")
                    return f"{section_title}\n\nDiagram JSON could not be parsed. Raw response:\n```\n{section_content}\n```"
                # Convert to Mermaid
                try:
                    mermaid_code = json_to_mermaid(diagram_json)
                except Exception as e:
                    log.error(f"Failed to convert diagram JSON to Mermaid for section '{section_id}': {e}. JSON: {json.dumps(diagram_json)[:500]}")
                    return f"{section_title}\n\nDiagram JSON could not be converted to Mermaid. JSON:\n```\n{json.dumps(diagram_json, indent=2)}\n```"
                # Validate Mermaid
                last_error_msg = "Diagram validation failed."
                for attempt in range(MAX_MERMAID_FIX_ATTEMPTS):
                    is_valid, error_msg, error_line = await _validate_mermaid_code(mermaid_code, section_title)
                    if is_valid:
                        log.info(f"[Mermaid Validation][{section_title}] Mermaid diagram is valid after {attempt} fix attempts.")
                        break
                    last_error_msg = error_msg or "Unknown validation error."
                    log.warning(f"[Mermaid Validation][{section_title}] Mermaid fix attempt {attempt + 1}/{MAX_MERMAID_FIX_ATTEMPTS} failed. Error: {last_error_msg}")
                    if attempt < MAX_MERMAID_FIX_ATTEMPTS - 1:
                        fixed_code = await _attempt_to_fix_mermaid_code(mermaid_code, last_error_msg, error_line or 0, section_title)
                        if fixed_code:
                            mermaid_code = fixed_code
                        else:
                            log.error(f"[Mermaid Fix][{section_title}] LLM could not provide a fix. Aborting fix attempts.")
                            break
                else:
                    log.error(f"[Mermaid Validation][{section_title}] Failed to validate Mermaid diagram after {MAX_MERMAID_FIX_ATTEMPTS} attempts.")

                # Compose Markdown output
                markdown = f"{section_title}\n\n"
                if not diagram_json.get("nodes") or not diagram_json.get("edges"):
                    markdown += "No meaningful diagram data was provided for this section."
                else:
                    markdown += f"```mermaid\n{mermaid_code}\n```"
                return markdown

            # --- Non-diagram Section Handling ---
            # START SANITIZATION AND MERMAID FIXING (legacy, for non-diagram sections)
            if section_content.startswith("```markdown"):
                section_content = section_content[len("```markdown"):].lstrip()
            if section_content.endswith("```"):
                section_content = section_content[:-len("```")].rstrip()
            # Fix for unclosed mermaid blocks before validation
            if "```mermaid" in section_content and section_content.count("```mermaid") > section_content.count("```\n", section_content.find("```mermaid")):
                parts = section_content.split("```mermaid")
                processed_content = parts[0]
                for i in range(1, len(parts)):
                    processed_content += "```mermaid"
                    if "```" not in parts[i]:
                        log.warning(f"Found unclosed mermaid block in section '{section_title}'. Appending closing fence before validation.")
                        processed_content += parts[i].rstrip() + "\n```"
                    else:
                        processed_content += parts[i]
                section_content = processed_content
            # MERMAID VALIDATION AND SELF-CORRECTION LOOP
            mermaid_match = re.search(r"```mermaid\n(.*?)\n```", section_content, re.DOTALL)
            if mermaid_match:
                original_mermaid_block = mermaid_match.group(0)
                mermaid_code = mermaid_match.group(1).strip()
                last_error_msg = "Diagram validation failed."
                for attempt in range(MAX_MERMAID_FIX_ATTEMPTS):
                    is_valid, error_msg, error_line = await _validate_mermaid_code(mermaid_code, section_title)
                    if is_valid:
                        log.info(f"[Mermaid Validation][{section_title}] Mermaid diagram is valid after {attempt} fix attempts.")
                        if attempt > 0:
                            new_mermaid_block = f"```mermaid\n{mermaid_code}\n```"
                            section_content = section_content.replace(original_mermaid_block, new_mermaid_block)
                        break
                    last_error_msg = error_msg or "Unknown validation error."
                    log.warning(f"[Mermaid Validation][{section_title}] Mermaid fix attempt {attempt + 1}/{MAX_MERMAID_FIX_ATTEMPTS} failed. Error: {last_error_msg}")
                    if attempt < MAX_MERMAID_FIX_ATTEMPTS - 1:
                        fixed_code = await _attempt_to_fix_mermaid_code(mermaid_code, last_error_msg, error_line or 0, section_title)
                        if fixed_code:
                            mermaid_code = fixed_code
                        else:
                            log.error(f"[Mermaid Fix][{section_title}] LLM could not provide a fix. Aborting fix attempts.")
                            break
                else:
                    log.error(f"[Mermaid Validation][{section_title}] Failed to validate Mermaid diagram after {MAX_MERMAID_FIX_ATTEMPTS} attempts.")
                    section_content = section_content.replace(original_mermaid_block, f"```mermaid\n{mermaid_code}\n```")

            if not section_content.lstrip().startswith(section_title):
                log.warning(f"LLM output for section '{section_id}' did not start with the expected title. Expected: '{section_title}'. Got: '{section_content[:200]}'. Prepending title.")
                section_content = f"{section_title}\n\n{section_content.lstrip()}"
            else:
                lines = section_content.splitlines()
                if len(lines) > 1 and lines[0].strip() == section_title.strip():
                    if not section_content.startswith(f"{section_title}\n\n"):
                        if lines[1].strip() and not (lines[1].strip().startswith(("*", "-", "```"))):
                            section_content = f"{section_title}\n\n{section_content[len(lines[0]):].lstrip()}"
            
            token_count = count_tokens(section_content)
            log.info(f"Successfully generated content for section '{section_id}'. Tokens: {token_count}")
            return section_content
        else:
            log.warning(f"LLM returned empty or no text content for section '{section_id}'.")
            return f"{section_title}\n\nNo content was generated by the LLM for this section."
    except APIError as e_api:
        log.error(f"Azure OpenAI API error for section '{section_id}': {e_api}. Status: {e_api.status_code}, Body: {e_api.body}", exc_info=True)
        log.debug(f"Prompt length for failed section '{section_id}': {len(prompt_for_section)}")
        return f"{section_title}\n\nError generating content for this section due to an API error: {str(e_api)}"
    except Exception as e_llm_section:
        log.error(f"LLM completion error for section '{section_id}': {e_llm_section}", exc_info=True)
        log.debug(f"Prompt length for failed section '{section_id}': {len(prompt_for_section)}")
        return f"{section_title}\n\nError generating content for this section: {str(e_llm_section)}"

async def _generate_individual_procedure_requirements(procedures: list) -> str:
    if not procedures:
        return "No M204 procedures (subroutines) are defined for this file/project."
    results = []
    for proc in procedures:
        proc_name = proc.get('m204_proc_name', 'N/A')
        proc_type = proc.get('m204_proc_type', 'N/A')
        proc_summary = proc.get('summary', '')
        proc_code = proc.get('procedure_content', '')
        proc_params = proc.get('m204_parameters_string', '')
        proc_vars = proc.get('variables_in_procedure', [])

        prompt = f"""
You are a senior technical analyst. For the following M204 procedure (subroutine):

- Write a descriptive paragraph summarizing its purpose, type, and any summary provided.
- Then, analyze the code and capture all rules, logic, and decision points in a hierarchical bullet-point format.
    * For each significant operation (e.g., FIND, CALL, PRINT, variable assignment), state the code line and provide a plain-language explanation.
    * For each conditional (e.g., IF, ELSE IF, ELSE), clearly state the condition, then describe the logic within each branch, using indentation to show nesting and sequence.
    * Include relevant code snippets from the source for clarity.
- Add bullet points for parameters and variables.

Procedure Name: {proc_name}
Type: {proc_type}
Summary: {proc_summary}

Procedure Code:
```m204
{proc_code}
```

Parameters: {proc_params}
Variables:
{json.dumps(proc_vars, indent=2)}
"""
        completion_response = await llm_config._llm.acomplete(prompt=prompt)
        if completion_response and completion_response.text:
            results.append(
                f"### {proc_name}\n\n"
                f"**Summary:**\n\n"
                f"{proc_summary if proc_summary else 'No summary available.'}\n\n"
                f"**Detailed Logic Flow and Bullet Points:**\n\n"
                f"{completion_response.text.strip()}\n\n"
                f"**Parameters and Variables:**\n\n"
                f"- Parameters: {proc_params if proc_params else 'None'}\n"
                f"- Variables:\n"
                + "\n".join(
                    f"  * {v.get('variable_name', 'N/A')} (Type: {v.get('variable_type', 'N/A')}, Scope: {v.get('scope', 'N/A')}, COBOL Mapped Name: {v.get('cobol_mapped_variable_name', 'N/A')})"
                    for v in proc_vars
                )
            )
        else:
            results.append(f"### {proc_name}\n\nNo content generated.")

    section_content = "\n\n".join(results)
    return section_content



async def generate_and_save_project_requirements_document(
    db: Session,
    project_id: int,
    options: RequirementGenerationOptionsSchema,
    custom_prompt_section: Optional[str] = None
) -> RequirementDocumentResponseSchema:
    """
    Generates a comprehensive Software Requirements Specification (SRS) document
    in Markdown format for a given project using an LLM.
    Each major section is generated via an LLM call with a tailored data payload.
    """
    log.info(f"Starting requirements document generation for project ID: {project_id} with options: {options.model_dump_json(exclude_none=True)}")
    log.info(f"Concurrency for LLM calls is limited to {MAX_CONCURRENT_LLM_CALLS}.")

    if not llm_config._llm:
        log.error("LLM is not configured. Cannot generate requirements document.")
        raise HTTPException(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, detail="LLM service is not configured or available.")

    try:
        project_data_for_llm = await _fetch_project_data_for_llm(db, project_id, options)
        log.info(f"Fetched and pre-processed project data for LLM. Keys: {list(project_data_for_llm.keys())}")
    except HTTPException as he:
        raise he
    except Exception as e_fetch:
        log.error(f"Failed to fetch project data for LLM (Project ID: {project_id}): {e_fetch}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to fetch project data: {str(e_fetch)}")

    project_name = project_data_for_llm.get("project_info", {}).get("project_name", f"Project {project_id}")
    document_title = f"Requirements Document for {project_name}"
    
    sections_config = _get_sections_config()
    markdown_parts = [f"# {document_title}\n"]

    semaphore = asyncio.Semaphore(MAX_CONCURRENT_LLM_CALLS)

    async def generate_with_semaphore(section_conf: Dict[str, Any]) -> str:
        async with semaphore:
            section_id = section_conf["id"]
            section_title = section_conf["title"]
            section_instructions = section_conf["instructions"]
            required_data_keys = section_conf.get("required_data", [])

            targeted_project_data = {
                key: project_data_for_llm[key] 
                for key in required_data_keys 
                if key in project_data_for_llm
            }
            formatted_data_for_prompt = _construct_llm_prompt_content(targeted_project_data)

            # Ensure top-level heading for procedures section
            if section_id == "m204_procedures" and "procedures" in project_data_for_llm:
                individual_content = await _generate_individual_procedure_requirements(
                    project_data_for_llm["procedures"]
                )
                # Always prepend the top-level heading
                return f"## 3. M204 Procedures\n\n{individual_content}"
            
            if section_id == "procedure_call_flow":
                if not project_data_for_llm.get("procedures"):
                    return f"{section_title}\n\nNo procedure calls are defined because there are no procedures in this file/project."

            else:
                return await _generate_llm_section(
                    section_id=section_id,
                    section_title=section_title,
                    section_instructions=section_instructions,
                    formatted_data_for_prompt=formatted_data_for_prompt,
                    custom_prompt_section=custom_prompt_section
                )

    generation_tasks = [generate_with_semaphore(conf) for conf in sections_config]

    try:
        log.info(f"Executing {len(generation_tasks)} section generation tasks with a concurrency limit of {MAX_CONCURRENT_LLM_CALLS}.")
        generated_sections_content = await asyncio.gather(*generation_tasks)
         # Log which sections are None
        for idx, part in enumerate(generated_sections_content):
            if part is None:
                section_id = sections_config[idx].get("id", "unknown")
                section_title = sections_config[idx].get("title", "unknown")
                log.warning(f"Section '{section_id}' ('{section_title}') returned None and will be skipped in the final document.")
        markdown_parts.extend(generated_sections_content)
        log.info("All parallel section generation tasks completed.")
    except Exception as e_gather:
        log.error(f"Error during parallel generation of sections: {e_gather}", exc_info=True)
        markdown_parts.append(f"\n\n## Error During Document Assembly\n\nAn error occurred while assembling the document sections: {str(e_gather)}")

    markdown_content = "\n\n".join([part for part in markdown_parts if part is not None])
    log.info(f"All sections processed. Total Markdown content length: {len(markdown_content)}")

    if not markdown_content.strip() or len(markdown_content.strip()) < len(f"# {document_title}") + 50:
        log.warning(f"Generated document content is very minimal or empty for project {project_id}. Content: '{markdown_content[:200]}'")
        if not markdown_content.strip():
             markdown_content = f"# {document_title}\n\nNo content was generated by the LLM."

    doc_create_data = RequirementDocumentCreateSchema(
        project_id=project_id,
        document_title=document_title,
        markdown_content=markdown_content,
        generation_options_json=options.model_dump()
    )

    db_document = RequirementDocument(**doc_create_data.model_dump())
    db.add(db_document)
    try:
        db.commit()
        db.refresh(db_document)
        log.info(f"Saved new requirements document ID {db_document.requirement_document_id} for project {project_id}")
    except Exception as e_db:
        db.rollback()
        log.error(f"Database error saving requirements document for project {project_id}: {e_db}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to save generated document: {str(e_db)}")

    response_data = RequirementDocumentResponseSchema.model_validate(db_document)
    if db_document.generation_options_json and isinstance(db_document.generation_options_json, dict):
        try:
            response_data.generation_options_used = RequirementGenerationOptionsSchema(**db_document.generation_options_json)
        except Exception as e_parse_resp:
            log.warning(f"Could not parse generation_options_json for response for doc ID {db_document.requirement_document_id}: {e_parse_resp}")
            response_data.generation_options_used = None 
    return response_data



# --- CRUD functions for RequirementDocument ---

async def get_requirement_document_by_id(db: Session, requirement_document_id: int) -> Optional[RequirementDocumentResponseSchema]:
    stmt = select(RequirementDocument).where(RequirementDocument.requirement_document_id == requirement_document_id)
    db_document = db.execute(stmt).scalar_one_or_none()
    if db_document:
        response = RequirementDocumentResponseSchema.model_validate(db_document)
        if db_document.generation_options_json: 
            try:
                options_data = db_document.generation_options_json
                if isinstance(options_data, str): # Should ideally be dict from DB
                    options_data = json.loads(options_data) 
                response.generation_options_used = RequirementGenerationOptionsSchema(**options_data)
            except Exception as e_parse:
                log.warning(f"Could not parse generation_options_json for doc ID {requirement_document_id}: {e_parse}. Data: {db_document.generation_options_json}")
                response.generation_options_used = None
        return response
    return None

async def get_latest_requirement_document_by_project_id(db: Session, project_id: int) -> Optional[RequirementDocumentResponseSchema]:
    stmt = select(RequirementDocument)\
        .where(RequirementDocument.project_id == project_id)\
        .order_by(desc(RequirementDocument.created_at))\
        .limit(1)
    db_document = db.execute(stmt).scalar_one_or_none()

    if db_document:
        response = RequirementDocumentResponseSchema.model_validate(db_document)
        if db_document.generation_options_json:
            try:
                options_data = db_document.generation_options_json
                if isinstance(options_data, str):
                    options_data = json.loads(options_data)
                response.generation_options_used = RequirementGenerationOptionsSchema(**options_data)
            except Exception as e_parse:
                log.warning(f"Could not parse generation_options_json for latest doc of project ID {project_id}: {e_parse}. Data: {db_document.generation_options_json}")
                response.generation_options_used = None
        return response
    return None

async def get_all_requirement_documents_by_project_id(db: Session, project_id: int) -> List[RequirementDocumentResponseSchema]:
    stmt = select(RequirementDocument)\
        .where(RequirementDocument.project_id == project_id)\
        .order_by(desc(RequirementDocument.created_at))
    db_documents = db.execute(stmt).scalars().all()

    response_list = []
    for doc in db_documents:
        response_item = RequirementDocumentResponseSchema.model_validate(doc)
        if doc.generation_options_json:
            try:
                options_data = doc.generation_options_json
                if isinstance(options_data, str):
                    options_data = json.loads(options_data)
                response_item.generation_options_used = RequirementGenerationOptionsSchema(**options_data)
            except Exception as e_parse:
                log.warning(f"Could not parse generation_options_json for doc ID {doc.requirement_document_id} in list for project ID {project_id}: {e_parse}. Data: {doc.generation_options_json}")
                response_item.generation_options_used = None
        response_list.append(response_item)
    return response_list

async def create_requirement_document(db: Session, doc_data: RequirementDocumentCreateSchema) -> RequirementDocumentResponseSchema:
    """
    Directly creates a requirement document. Useful if generation happens elsewhere or for manual entries.
    """
    db_model_data = doc_data.model_dump()
    # generation_options_json should already be a dict or None from doc_data.model_dump()
    
    db_document = RequirementDocument(**db_model_data)
    db.add(db_document)
    try:
        db.commit()
        db.refresh(db_document)
    except Exception as e:
        db.rollback()
        log.error(f"Error creating requirement document: {e}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Could not create document: {str(e)}")

    response = RequirementDocumentResponseSchema.model_validate(db_document)
    if db_document.generation_options_json and isinstance(db_document.generation_options_json, dict):
        try:
            response.generation_options_used = RequirementGenerationOptionsSchema(**db_document.generation_options_json)
        except Exception as e_parse: 
            log.warning(f"Could not parse generation_options_json from created document for response: {e_parse}. Data: {db_document.generation_options_json}")
            response.generation_options_used = None
    elif db_document.generation_options_json:
        log.warning(f"generation_options_json is not a dict after creation. Type: {type(db_document.generation_options_json)}. Data: {db_document.generation_options_json}")
        response.generation_options_used = None
    return response