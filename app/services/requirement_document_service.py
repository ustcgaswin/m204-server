import asyncio
from sqlalchemy.orm import Session, selectinload
from sqlalchemy import desc, select
from typing import List, Optional, Dict, Any
import json

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
    # ImageStatementResponseSchema, # Removed
    M204ProcedureCallResponseSchema
)
from app.schemas.generic_analysis_schema import DDStatementResponseSchema


from app.config.llm_config import llm_config
from app.utils.logger import log
from fastapi import HTTPException, status
from datetime import datetime

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

async def _fetch_project_data_for_llm(db: Session, project_id: int, options: RequirementGenerationOptionsSchema) -> Dict[str, Any]:
    """
    Fetches all relevant data for a project to be used in the LLM prompt.
    """
    project_data: Dict[str, Any] = {}

    # Project Info
    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")
    project_data["project_info"] = _serialize_model_instance(project)

    # Input Source Summaries (JCL and M204 detailed descriptions)
    if options.include_project_overview: # Tying inclusion to project overview
        input_sources = db.query(InputSource).filter(InputSource.project_id == project_id).all()
        source_summaries = []
        for src in input_sources:
            summary_item = {
                "original_filename": src.original_filename,
                "source_type": src.source_type,
                "jcl_detailed_description": getattr(src, 'jcl_detailed_description', None), # Use getattr for safety
                "m204_detailed_description": getattr(src, 'm204_detailed_description', None) # Use getattr for safety
            }
            # Only add if there's a relevant description
            if summary_item["jcl_detailed_description"] or summary_item["m204_detailed_description"]:
                source_summaries.append(summary_item)
        if source_summaries:
            project_data["source_file_llm_summaries"] = source_summaries


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
        # Removed selectinload(M204File.fields) as M204Field model and relationship are removed
        m204_files = db.query(M204File).filter(M204File.project_id == project_id).all()
        project_data["m204_files"] = [M204FileResponseSchema.model_validate(f).model_dump(exclude_none=True) for f in m204_files]

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


def _construct_llm_prompt_content(project_data: Dict[str, Any], options: RequirementGenerationOptionsSchema) -> str:
    """
    Constructs the detailed data dump part of the LLM prompt.
    Formats the fetched data into a readable structure for the LLM.
    """
    content_parts = []

    if options.include_project_overview and "project_info" in project_data:
        pi = project_data["project_info"]
        content_parts.append(f"Project Information:\n- Name: {pi.get('project_name', 'N/A')}\n- Description: {pi.get('description', 'N/A')}\n- Created: {pi.get('created_at', 'N/A')}\n")

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

    if options.include_procedures and "procedures" in project_data and project_data["procedures"]:
        content_parts.append("M204 Procedures Data:")
        for proc in project_data["procedures"]:
            proc_details = [
                f"  - Procedure Name: {proc.get('m204_proc_name', 'N/A')}",
                f"    Type: {proc.get('m204_proc_type', 'N/A')}",
                f"    Parameters String: {proc.get('m204_parameters_string', 'None')}",
                f"    Target COBOL Program: {proc.get('target_cobol_function_name', 'N/A')}",
            ]
            if options.include_procedure_summaries and proc.get('summary'):
                 proc_details.append(f"    Summary: {proc.get('summary', 'No summary available.')}")

            proc_details.append(f"    Content Snippet (first 200 chars): {proc.get('procedure_content', 'N/A')[:200]}...")

            if options.include_procedure_variables and proc.get("variables_in_procedure"):
                proc_details.append("    Variables Defined in Procedure:")
                for var in proc["variables_in_procedure"]:
                    proc_details.append(f"      - Variable Name: {var.get('variable_name')}, Type: {var.get('variable_type')}, Scope: {var.get('scope')}, COBOL Mapped Name: {var.get('cobol_mapped_variable_name', 'N/A')}")
            content_parts.extend(proc_details)
        content_parts.append("\n")


    if options.include_files and "m204_files" in project_data and project_data["m204_files"]:
        content_parts.append("M204 File Definitions Data:")
        for f_data in project_data["m204_files"]:
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

    if options.include_global_variables and "global_variables" in project_data and project_data["global_variables"]:
        content_parts.append("Global/Public M204 Variables Data:")
        for var in project_data["global_variables"]:
            attributes_json = json.dumps(var.get('attributes')) if var.get('attributes') else 'None'
            content_parts.append(f"  - Variable Name: {var.get('variable_name')}, Type: {var.get('variable_type')}, Scope: {var.get('scope')}, COBOL Mapped Name: {var.get('cobol_mapped_variable_name', 'N/A')}, Attributes: {attributes_json}")
        content_parts.append("\n")

    if options.include_jcl_dd_statements and "dd_statements" in project_data and project_data["dd_statements"]:
        content_parts.append("JCL DD Statements Data:")
        for dd in project_data["dd_statements"]:
            content_parts.append(f"  - DD Name: {dd.get('dd_name')}, DSN: {dd.get('dsn', 'N/A')}, Disposition: {dd.get('disposition', 'N/A')}, Job Name: {dd.get('job_name', 'N/A')}, Step Name: {dd.get('step_name', 'N/A')}")
        content_parts.append("\n")

    if options.include_procedure_calls and "procedure_calls" in project_data and project_data["procedure_calls"]:
        content_parts.append("Procedure Call Relationships Data:")
        for call in project_data["procedure_calls"]:
            calling_proc_id = call.get('calling_procedure_id')
            calling_proc_name = "N/A"
            if calling_proc_id and "procedures" in project_data and project_data["procedures"]:
                caller = next((p for p in project_data["procedures"] if p.get("proc_id") == calling_proc_id or p.get("m204_proc_id") == calling_proc_id), None)
                if caller: 
                    calling_proc_name = caller.get("m204_proc_name", "N/A")
                else:
                    log.debug(f"Calling procedure ID {calling_proc_id} not found in pre-loaded procedure data for call ID {call.get('procedure_call_id')}")
            content_parts.append(f"  - Calling Procedure Name: {calling_proc_name} (ID: {calling_proc_id}), Called Procedure Name: {call.get('called_procedure_name')}, Line Number: {call.get('line_number')}, Is External: {call.get('is_external')}")
        content_parts.append("\n")

    return "\n".join(content_parts)

def _get_sections_config() -> List[Dict[str, str]]:
    """
    Defines the structure and instructions for each section of the requirements document.
    """
    return [
        {
            "id": "project_overview",
            "title": "## 1. Project Overview",
            "instructions": """
   (Provide a narrative overview in a paragraph or two based on the 'Project Information' from the input data. Include project name, description, and creation date.
    **Also, list any COBOL programs identified as targets of M204 procedures (from 'M204 Procedures Data' - `target_cobol_function_name`) and briefly describe their potential role in the system based on the procedures that call them and any relevant 'Source File LLM-Generated Summaries'.**
    You can also incorporate high-level insights from the 'Source File LLM-Generated Summaries' if they provide a good overview of the system's nature.)
"""
        },
        {
            "id": "conceptual_data_model",
            "title": "### 1.1. Conceptual Data Model Diagram",
            "instructions": """
      (Based on the overall system data, including 'M204 Procedures Data', 'M204 File Definitions Data', 'JCL DD Statements Data', and 'Source File LLM-Generated Summaries', generate a high-level Mermaid flowchart diagram (e.g., using `graph TD;` for Top-Down or `graph LR;` for Left-to-Right) illustrating the primary data flows or control sequences within the system.
       The diagram should identify:
       - Key processes (e.g., JCL Jobs, important M204 Procedures like ONLINE or BATCH ones). Represent them as nodes (e.g., `jobName["JCL Job: MYJOB"]`, `procName{M204 Proc: MYPROC}`).
       - Major data stores (e.g., M204 Files or their associated VSAM datasets). Represent them using a database shape (e.g., `fileName[("M204 File: MYFILE")]`).
       - Data flows between these processes and data stores, or control flow from one process to another. Use arrows to indicate the direction (e.g., `-->` for flow, `-- Text -->` for flow with description).
       Use information from 'JCL DD Statements Data' (`dsn`, `disposition`, `job_name`, `step_name`) to show JCLs interacting with datasets and initiating processes.
       Use information from 'M204 Procedures Data' (e.g., `m204_proc_name`, `m204_proc_type`) and 'Procedure Call Relationships Data' to infer how procedures process data or trigger other procedures/COBOL programs.
       Refer to 'Source File LLM-Generated Summaries' to understand the purpose of JCLs and M204 modules and how they contribute to data or control flow.
       The goal is to provide an overview of how data enters, is processed by, and exits the system, or how key operational sequences unfold. Focus on the most significant flows and control paths.
       **Enclose the syntactically correct Mermaid code for the flowchart within a fenced code block like this:**
       ```mermaid
       graph TD;
           ExternalInput[External Input Source] --> JCL_PROCESS_DATA{JCL Job: PROCESS_DATA};
           JCL_PROCESS_DATA -- Reads --> M204_INPUT_FILE[("M204 File: INPUT_FILE")];
           JCL_PROCESS_DATA -- Executes --> M204_PROC_VALIDATE[M204 Procedure: VALIDATE_INPUT];
           M204_PROC_VALIDATE -- Writes Validated Data --> M204_STAGING_FILE[("M204 File: STAGING_AREA")];
           M204_ONLINE_PROC{M204 Online Proc: UPDATE_MASTER} -- Reads/Writes --> M204_MASTER_FILE[("M204 File: MASTER_DATA")];
           M204_STAGING_FILE -- Input for Update --> M204_ONLINE_PROC;
           M204_MASTER_FILE -- Data Extract --> ExternalReport[External Report Output];
       ```
       If the provided data is insufficient to create a meaningful data flow or control flow diagram for this overview, include the heading and state "Data insufficient to generate a system data/control flow diagram." instead of providing malformed Mermaid code.
       This diagram is intended as a high-level overview and should complement more specific diagrams like JCL job flow or detailed procedure call diagrams found in other sections.)
"""
        },
        {
            "id": "m204_procedures",
            "title": "## 2. M204 Procedures",
            "instructions": """
   (Based on 'M204 Procedures Data' and relevant 'Source File LLM-Generated Summaries' for M204 files:
    - For each procedure, write a descriptive paragraph. This paragraph should cover its name and type. (Information about target COBOL programs is now covered in the Project Overview section).
    - If a procedure summary is available in the input, incorporate it into the descriptive paragraph.
    - Detail parameters and any variables defined within the procedure using bullet points *underneath* the main descriptive paragraph for that procedure.
    - For example:
      "The procedure **SAMPLE_PROC** is an **ONLINE** type procedure, found in the M204 source file `SOURCEA.M204` (refer to the summary for `SOURCEA.M204` for its overall purpose). The primary function of this procedure appears to be real-time data modification based on user input.
      * Parameters: `ACCOUNT_NUMBER, UPDATE_DATA`
      * Variables defined in procedure:
        * `%OLD_VALUE` (Type: `STRING`, Scope: `LOCAL`, COBOL Mapped Name: `WS-OLD-VAL`)
        * `%NEW_VALUE` (Type: `STRING`, Scope: `LOCAL`, COBOL Mapped Name: `WS-NEW-VAL`)"
    - Conclude this section with a brief summary paragraph discussing any observed overall purpose or common themes across the procedures, informed by individual procedure details and the broader M204 source file summaries.)
"""
        },
        {
            "id": "m204_file_definitions",
            "title": "## 3. M204 File Definitions",
            "instructions": """
   (Based on 'M204 File Definitions Data' and relevant 'Source File LLM-Generated Summaries' for M204 files:
    - For each M204 file, write a descriptive paragraph. This paragraph should cover its name, M204 attributes, whether it's a database file, and its target VSAM dataset name and type. Use the LLM-generated summary for the M204 source file where this file definition might be elaborated if available.
    - If field information is available within the `file_definition_json` attribute of a file, list those fields using bullet points. Each bullet point should detail the field's name and its M204 attributes. For example:
      "The M204 file **CUSTFILE** is defined with attributes `(attribute_list)`. It is flagged as a database file and maps to the target VSAM dataset **PROD.CUSTOMER.MASTER** of type `KSDS`. The M204 source summary for `MAINCUST.M204` indicates this file is central to customer data management. This file appears to store core customer master data.
      * Fields (from JSON definition, if available):
        * `CUST_ID` (Attributes: `KEY, NUMERIC, ...`)
        * `CUST_NAME` (Attributes: `TEXT, ...`)"
    - If no detailed field information is available in `file_definition_json`, state that "Detailed field structure not available in the provided data for this file."
    - Conclude with a summary paragraph about the general categories or types of data managed by these files based on their names, attributes, and the overall M204 source file summaries.)
"""
        },
        {
            "id": "m204_file_vsam_jcl_diagram",
            "title": "### 3.1. M204 File and Associated VSAM/JCL Diagram",
            "instructions": """
      (Based on 'M204 File Definitions Data' (`m204_file_name`, `target_vsam_dataset_name`) and 'JCL DD Statements Data' (`dd_name`, `dsn`), generate a Mermaid flowchart diagram.
       The diagram should show each M204 file and its target VSAM dataset name.
       If a JCL DD Statement's `dsn` matches an M204 file's `target_vsam_dataset_name`, create a link from the JCL DD statement node to the M204 file node.
       Represent M204 files as `M204: <m204_file_name> (VSAM: <target_vsam_dataset_name>)`.
       Represent JCL DD statements as `JCL DD: <dd_name> (DSN: <dsn>)`.
       **Enclose the syntactically correct Mermaid code within a fenced code block like this:**
       ```mermaid
       graph TD;
           M204_CUST["M204: CUSTFILE (VSAM: PROD.DATA.CUST)"];
           M204_ACCT["M204: ACCTFILE (VSAM: PROD.DATA.ACCT)"];
           DD_INPCUST["JCL DD: INPCUST (DSN: PROD.DATA.CUST)"];
           DD_OTACCT["JCL DD: OTACCT (DSN: PROD.DATA.ACCT)"];
           DD_INPCUST --> M204_CUST;
           DD_OTACCT --> M204_ACCT;
       ```
       **If no such relationships are found or data is insufficient to generate a meaningful and syntactically correct diagram, include the heading and state "No clear M204 file to JCL DD statement links found or data is insufficient to generate a diagram." instead of providing malformed Mermaid code.**)
"""
        },
        {
            "id": "global_variables",
            "title": "## 4. Global/Public M204 Variables",
            "instructions": """
   (Based on 'Global/Public M204 Variables Data':
    - Begin with an introductory paragraph about the role of global/public variables in the system.
    - List each global or public variable using bullet points. Each bullet point should include its name, type, scope, suggested COBOL mapped name, and any defined attributes.
    - Follow the list with a paragraph further explaining the potential collective purpose or usage patterns of these global variables based on their characteristics.)
"""
        },
        {
            "id": "jcl_dd_statements",
            "title": "## 5. JCL DD Statements",
            "instructions": """
   (Based on 'JCL DD Statements Data' and relevant 'Source File LLM-Generated Summaries' for JCL files:
    - Start with a paragraph describing the role of JCL DD statements in interfacing the M204 application with datasets. Refer to the LLM-generated summaries for the JCL files to provide context on the overall purpose of the JCLs containing these DD statements.
    - List the relevant DD statements. You can use bullet points for each DD statement, detailing its DD Name, DSN, Disposition, and the Job and Step context. A compact table could also be appropriate here if there are many with similar structures.
    - Conclude with a paragraph highlighting any DD statements that seem particularly critical (e.g., for primary input/output files, connections to other systems), considering their role within the summarized JCL job flow.)
"""
        },
        {
            "id": "jcl_job_step_flow_diagram",
            "title": "### 5.1. JCL Job/Step Flow Diagram",
            "instructions": """
      (Based on 'JCL DD Statements Data' (`job_name`, `step_name`, `dd_name`, `dsn`), generate a Mermaid flowchart diagram illustrating the JCL job and step flow.
       Group DD statements under their respective steps, and steps under their respective jobs.
       Represent jobs as `Job: <job_name>`, steps as `Step: <step_name>`. DD statements can be represented by their `dd_name` and `dsn`.
       **Enclose the syntactically correct Mermaid code within a fenced code block like this:**
       **A better example for JCL Job/Step Flow using subgraphs:**
       ```mermaid
       graph TD;
           subgraph "Job: JOB01"
               direction LR;
               J1_STEP01["Step: STEP01"];
               J1_STEP01 --> J1_DDIN["DD: DDIN (DSN: INPUT.FILE)"];
               J1_STEP01 --> J1_DDOUT["DD: DDOUT (DSN: OUTPUT.FILE)"];
           end
           subgraph "Job: JOB02"
               direction LR;
               J2_STEPX["Step: STEPX"];
               J2_STEPX --> J2_DDSYS["DD: SYSIN (DSN: *.STEPX.SYSIN)"];
           end
       ```
       **If no JCL DD statement data is available to generate a meaningful and syntactically correct diagram, include the heading and state "No JCL data available to generate a job/step flow diagram." instead of providing malformed Mermaid code.**)
"""
        },
        {
            "id": "procedure_call_flow",
            "title": "## 6. Procedure Call Flow",
            "instructions": """
   (Based on 'Procedure Call Relationships Data':
    - Describe the procedure call flow using narrative paragraphs to explain major sequences or interactions.
    - Supplement with bullet points to list specific call relationships, indicating the calling procedure, the called procedure, the line number of the call, and whether it's an external call.
    - Focus on making the control flow and dependencies clear. Try to identify and describe any main processing sequences, critical execution paths, or highly interconnected modules.
"""
        },
        {
            "id": "procedure_call_diagram",
            "title": "### 6.1. Procedure Call Diagram",
            "instructions": """
      (Based on the 'Procedure Call Relationships Data', generate a Mermaid flowchart diagram (e.g., `graph TD;` for Top-Down or `graph LR;` for Left-to-Right) illustrating the direct call relationships between procedures.
       The diagram should clearly show which procedure calls which other procedure(s). Use the actual procedure names found in the data.
       **Enclose the syntactically correct Mermaid code within a fenced code block like this:**
       ```mermaid
       graph TD;
           PROC_A["Procedure A"] --> PROC_B["Procedure B"];
           PROC_A["Procedure A"] --> PROC_C["Procedure C"];
           PROC_B["Procedure B"] --> PROC_D["Procedure D"];
           PROC_E["Procedure E"]; // A procedure that is called but doesn't call, or isn't called but exists
       ```
       Ensure all unique procedure names involved in any call (callers and callees) are represented as nodes in the diagram.
       **If no procedure calls exist in the 'Procedure Call Relationships Data' or data is insufficient to generate a meaningful and syntactically correct diagram, include the heading and state "No procedure call relationships found or data is insufficient to generate a diagram." instead of providing malformed Mermaid code.**)
"""
        },
        {
            "id": "data_dictionary",
            "title": "## 7. Data Dictionary / Key Data Elements",
            "instructions": """
   (Synthesize information from 'M204 File Definitions Data' (primarily `file_definition_json` if available for field details), 'Global/Public M204 Variables Data', and 'M204 Procedures Data' (variables_in_procedure).
    - Describe key data elements in paragraphs. For each element or group of related elements, discuss its apparent meaning and use.
    - Use bullet points to list specific attributes like M204 data type/attributes (from `file_definition_json` or variable definitions) and where it's primarily used (e.g., in which files or procedure types).
    - If a comprehensive dictionary isn't directly inferable due to lack of detailed field definitions, provide a narrative summary of the main types of data the system processes based on file names, variable names, and their significance.)
"""
        },
        {
            "id": "external_interfaces",
            "title": "## 8. External Interfaces",
            "instructions": """
   (Infer from 'JCL DD Statements Data' (DSNs) and 'M204 File Definitions Data' (attributes suggesting external links).
    - Describe in paragraph form any identified external systems, datasets, or interfaces that the M204 application interacts with. Explain the nature of these interactions if discernible.)
"""
        },
        {
            "id": "non_functional_requirements",
            "title": "## 9. Non-Functional Requirements",
            "instructions": """
   (Review all provided data for hints towards non-functional requirements.
    - Describe any identified NFRs (e.g., performance considerations from file attributes, security aspects from field encryption, operational constraints) in paragraph form.
    - If none are directly inferable, state that "No specific non-functional requirements were directly inferable from the provided system data.")
"""
        },
        {
            "id": "other_observations",
            "title": "## 10. Other Observations / Summary",
            "instructions": """
   (Provide a concluding summary of the system in narrative paragraphs. Highlight any overarching patterns, complexities, or notable observations that don't fit neatly into other sections. Address points from the 'Additional Instructions/Custom Section from User' here if not covered elsewhere.)
"""
        },
        {
            "id": "technical_requirements_main",
            "title": "# Technical Requirements",
            "instructions": """
   (This section is intended for a high-level technical audience like Project Management and CTO.
    It should provide a structured, detailed overview of the system's technical aspects based on the provided data.
    Synthesize and abstract information from the preceding detailed sections, focusing on technical architecture, data flow, and key components. **Refer to the 'Source File LLM-Generated Summaries' to enrich the descriptions of JCL and M204 components.**)
"""
        },
        {
            "id": "tech_req_architecture",
            "title": "### A. System Architecture Overview",
            "instructions": """
   (Describe the high-level components of the M204-based system and their primary roles.
    Illustrate how M204 procedures (distinguishing between `m204_proc_type` like ONLINE, BATCH, INCLUDE, SUBROUTINE), COBOL programs (as indicated by `target_cobol_function_name` in procedures, detailed in Project Overview), JCL (from `DDStatement` data providing `job_name`, `step_name`, `dd_name`, `dsn`, and `disposition`), and M204 files/VSAM datasets (from `M204File` data, including `m204_file_name`, `target_vsam_dataset_name`, `target_vsam_type`) interact to form the overall system architecture. **Use the 'Source File LLM-Generated Summaries' to provide context on the purpose of specific JCL files or M204 source modules.**
    Identify any distinct subsystems or processing layers if they can be inferred from procedure groupings, naming conventions, or data flow patterns observed in the input data and file summaries.)
"""
        },
        {
            "id": "tech_req_data_model",
            "title": "### B. Data Model and Management",
            "instructions": """
   (Detail the key data entities managed by the system, as identified from `M204 File Definitions Data` (specifically `m204_file_name` and its associated `file_definition_json` if present). For each M204 file, describe its purpose (e.g., master data, transactional data, index files, work files) based on its name, attributes, and any field information available in `file_definition_json`. **Corroborate with 'Source File LLM-Generated Summaries' for M204 files.**
    Explain how data is stored, referencing `M204File.is_db_file`, `M204File.target_vsam_dataset_name`, and `M204File.target_vsam_type` (e.g., KSDS, ESDS).
    Describe significant characteristics of the data model by analyzing field data if available in `file_definition_json`:
    - Key fields (e.g., those with `KEY` in JSON attributes).
    - Data types and lengths (e.g., NUMERIC, TEXT, specific lengths from JSON attributes).
    - Occurrences (e.g., `OCCURS` clauses in JSON attributes).
    - Relationships implied by field names or usage across different files or procedures.
    If detailed field information is not available, describe the data model at a higher level based on file names and their general attributes.
    Summarize the overall data flow:
    - Data Ingress: How data enters the system (e.g., through JCL DD statements with `disposition` SHR or OLD, or via ONLINE procedures).
    - Data Processing: How data is transformed or used internally (e.g., by M204 procedures, manipulated using variables, stored/retrieved from M204 files).
    - Data Egress: How data leaves the system (e.g., through JCL DD statements with `disposition` NEW or MOD, or reports generated via procedures).)
"""
        },
        {
            "id": "tech_req_core_processing",
            "title": "### C. Core Processing Logic and Control Flow",
            "instructions": """
   (Explain the main processing sequences and business logic implemented within the M204 procedures, referencing their `m204_proc_type` and interactions with COBOL programs (whose roles are discussed in Project Overview). **Contextualize with 'Source File LLM-Generated Summaries' for the M204 files containing these procedures.**
    Highlight critical procedures or call chains by analyzing `Procedure Call Relationships Data` (using `calling_procedure_name`, `called_procedure_name`, `line_number` of call, and `is_external` flag). Discuss the significance of frequently called procedures or key external calls.
    Describe how user interactions (for `ONLINE` procedures) or batch processes (inferred from `BATCH` procedure types and associated JCL `job_name`/`step_name` context) are handled.
    Discuss the role of `M204Variable`s:
    - Global/Public variables (`scope` is GLOBAL or PUBLIC): Their purpose in system-wide state management, data sharing between modules, or configuration. Refer to their `variable_name`, `variable_type`, and `cobol_mapped_variable_name`.
    - Procedure-local variables (`scope` is LOCAL): Their use within specific procedures, including `m204_parameters_string` for input/output to procedures.
    Analyze patterns in variable usage if discernible.)
"""
        },
        {
            "id": "tech_req_external_interfaces",
            "title": "### D. External Interfaces and Dependencies",
            "instructions": """
   (List and describe all identified external systems, datasets, or services that the M204 application interacts with.
    Base this on:
    - `JCL DD Statements Data`: Analyze `dsn`s that point to external (non-system specific) files or GDGs. Note the `disposition` (e.g., NEW, MOD for outputs; OLD, SHR for inputs) to understand data direction. **The 'Source File LLM-Generated Summaries' for JCLs might clarify the purpose of these external datasets.**
    - `M204 File Definitions Data`: Check if `m204_attributes` or `target_vsam_dataset_name` suggest links to shared datasets with other applications or standard interface files.
    - `Procedure Call Relationships Data`: `is_external` calls might indicate interfaces to other systems or shared utilities if `called_procedure_name` implies this.
    Specify the nature of these interfaces (e.g., file-based batch data exchange, shared VSAM datasets, triggers to/from other systems if inferable). Discuss their importance to the system's overall functionality and data integrity.)
"""
        },
        {
            "id": "tech_req_tech_stack",
            "title": "### E. Technology Stack and Environment",
            "instructions": """
   (Summarize the core technologies used, based on available data:
    - **Model 204:** Note its role (e.g., primary application logic, database). Mention specific features used if inferable from `Procedure.m204_proc_type` (ONLINE, BATCH, INCLUDE, SUBROUTINE indicating User Language/SOUL usage), `M204File.m204_attributes`, or field attributes. **Refer to 'Source File LLM-Generated Summaries' for M204 files for broader context on how M204 is utilized.**
    - **COBOL:** Its role (e.g., complex business logic, batch processing) as indicated by `target_cobol_function_name` in procedure data (detailed in Project Overview).
    - **JCL:** Its function in orchestrating batch jobs, file management, and program execution, detailed by `DDStatement` data (`job_name`, `step_name`, `dsn`, `disposition`). **The 'Source File LLM-Generated Summaries' for JCLs will provide an overview of the job functionalities.**
    - **VSAM:** The types of datasets used (e.g., KSDS, ESDS from `M204File.target_vsam_type`) and their purpose.
    Discuss any technical constraints or environmental considerations inferred from the system data:
    - Dataset Naming Conventions: Patterns observed in `dsn` or `target_vsam_dataset_name`.
    - Batch Windows/Scheduling: Implied by JCL structure or batch procedure design.
    - Security Mechanisms: Any hints from file attributes, variable naming, or procedure structure (though likely limited from this data).
    - Character Sets/Encoding: If any hints in field attributes.)
"""
        },
        {
            "id": "tech_req_nfr_inferred",
            "title": "### F. Key Non-Functional Aspects (Inferred)",
            "instructions": """
   (Based on the detailed analysis in section 9 and the overall system data, reiterate and detail any significant non-functional requirements or characteristics critical from a technical perspective. Be specific by referencing the data points that lead to these inferences.
    - **Performance:**
      - Identify potential bottlenecks or performance-critical areas (e.g., frequently accessed files indicated by `KEY` fields in `file_definition_json`, complex `ONLINE` procedures, large data volumes suggested by `OCCURS` clauses in `file_definition_json` or file structures).
      - Discuss indexing strategies (`KEY` fields from `file_definition_json`) and their impact.
    - **Data Integrity and Consistency:**
      - Mechanisms implied by field data types and attributes (e.g., `NUMERIC` validation, length constraints from `file_definition_json`).
      - Potential for referential integrity issues or consistency challenges if not explicitly managed.
      - Transactional characteristics, if inferable from procedure logic or COBOL interactions.
    - **Security:**
      - Data sensitivity implied by file or field names (e.g., PII).
      - Access controls suggested by M204 file attributes or security-related keywords if present (though direct security data is usually not in code artifacts alone).
    - **Maintainability and Complexity:**
      - Modularity (e.g., use of `INCLUDE` or `SUBROUTINE` procedures vs. monolithic procedures).
      - Complexity of control flow (dense `ProcedureCall` graphs, deep nesting).
      - Impact of `GLOBAL`/`PUBLIC` variables on understanding data flow and side effects.
      - Readability of code snippets or variable names (e.g., `cobol_mapped_variable_name`).
    - **Operational Stability and Reliability:**
      - Error handling mechanisms (if visible in procedure snippets or implied by COBOL interactions).
      - Dependencies on external files/systems outlined in JCL (points of failure).
      - Recovery considerations for VSAM datasets based on `target_vsam_type` and usage patterns.
    If specific NFRs are not directly inferable for an aspect, clearly state "No specific insights for [Aspect] could be directly inferred from the provided system data.")
"""
        }
    ]

async def _generate_llm_section(
    section_id: str,
    section_title: str,
    section_instructions: str,
    formatted_data_for_prompt: str,
    custom_prompt_section: Optional[str]
) -> str:
    """
    Generates content for a single section of the requirements document using the LLM.
    """
    prompt_for_section = f"""
You are a senior technical analyst and writer. Your task is to generate the content for *only* the following section of a Software Requirements Specification (SRS) document in Markdown format for an existing M204-based system.
The section content should be based *solely* on the structured data provided below. Do not invent information not present in the provided data.
Output *only* the Markdown for this specific section, starting with its heading.
Emphasize the use of **paragraphs for description and bullet points for lists of attributes or related items**. Tables can be used sparingly.
**When generating Mermaid diagrams, ensure the syntax is correct and the diagram is enclosed in a ```mermaid ... ``` fenced code block. If data is insufficient for a meaningful diagram, state this clearly instead of outputting malformed code.**

**Input Data (Summary of system components and relationships):**
--- BEGIN SYSTEM DATA ---
{formatted_data_for_prompt}
--- END SYSTEM DATA ---

{f"**Additional Instructions/Custom Section from User (consider these when generating content for this section):**\n{custom_prompt_section}\n" if custom_prompt_section else ""}

**Instructions for the section to generate:**
Your output must start *exactly* with the following heading line:
{section_title}

And then provide the content as described below:
{section_instructions}

If data for this specific section is not available or not applicable based on the input, include the heading and a brief note like "Not applicable based on provided data." or "No data available for this section."
When discussing JCL, M204 Procedures, or M204 Files (if relevant to this section), refer to any 'Source File LLM-Generated Summaries' provided in the input data to give broader context.
Do not add any preamble or explanation before the section's heading.
Ensure the entire output for this section is valid Markdown.
"""
    log.info(f"Generating section: '{section_title}' (ID: {section_id})")
    # log.debug(f"LLM prompt for section '{section_id}':\n{prompt_for_section[:1000]}...") 

    try:
        completion_response = await llm_config._llm.acomplete(prompt=prompt_for_section)
        if completion_response and completion_response.text:
            section_content = completion_response.text
            normalized_content_start = section_content.lstrip().replace('\r\n', '\n').replace('\r', '\n')
            normalized_expected_title = section_title.replace('\r\n', '\n').replace('\r', '\n')

            if not normalized_content_start.startswith(normalized_expected_title):
                log.warning(f"LLM output for section '{section_id}' did not start with the expected title. Expected: '{section_title}'. Got: '{section_content[:200]}'. Prepending title.")
                section_content = f"{section_title}\n\n{section_content.lstrip()}"
            else:
                lines = section_content.splitlines()
                if len(lines) > 0 and lines[0].strip() == section_title.strip(): # Title is on its own line
                    # If there's content on the next line and it's not a list/code block, ensure double newline
                    if len(lines) > 1 and lines[1].strip() != "" and \
                       not section_content.startswith(f"{section_title}\n\n") and \
                       not (lines[1].strip().startswith("*") or lines[1].strip().startswith("-") or lines[1].strip().startswith("```")):
                        section_content = f"{section_title}\n\n{section_content[len(lines[0]):].lstrip()}"
                # If title and content are on the same line (less likely with current prompt but possible)
                elif section_content.startswith(section_title) and not section_content.startswith(f"{section_title}\n"):
                     section_content = f"{section_title}\n\n{section_content[len(section_title):].lstrip()}"


            log.info(f"Successfully generated content for section '{section_id}'. Length: {len(section_content)}")
            return section_content
        else:
            log.warning(f"LLM returned empty or no text content for section '{section_id}'.")
            return f"{section_title}\n\nNo content was generated by the LLM for this section."
    except Exception as e_llm_section:
        log.error(f"LLM completion error for section '{section_id}': {e_llm_section}", exc_info=True)
        return f"{section_title}\n\nError generating content for this section: {str(e_llm_section)}"


async def generate_and_save_project_requirements_document(
    db: Session,
    project_id: int,
    options: RequirementGenerationOptionsSchema,
    custom_prompt_section: Optional[str] = None
) -> RequirementDocumentResponseSchema:
    """
    Generates a comprehensive Software Requirements Specification (SRS) document
    in Markdown format for a given project using an LLM.
    Each major section is generated via an LLM call, potentially in parallel.
    """
    log.info(f"Starting requirements document generation for project ID: {project_id} with options: {options.model_dump_json(exclude_none=True)}")

    if not llm_config._llm:
        log.error("LLM is not configured. Cannot generate requirements document.")
        raise HTTPException(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, detail="LLM service is not configured or available.")

    try:
        project_data_for_llm = await _fetch_project_data_for_llm(db, project_id, options)
        log.info(f"Fetched project data for LLM. Keys: {list(project_data_for_llm.keys())}")
    except HTTPException as he:
        raise he
    except Exception as e_fetch:
        log.error(f"Failed to fetch project data for LLM (Project ID: {project_id}): {e_fetch}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to fetch project data: {str(e_fetch)}")

    formatted_data_for_prompt = _construct_llm_prompt_content(project_data_for_llm, options)
    log.info(f"Formatted data for LLM prompt constructed. Length: {len(formatted_data_for_prompt)}")

    project_name = project_data_for_llm.get("project_info", {}).get("project_name", f"Project {project_id}")
    document_title = f"Requirements Document for {project_name}"
    
    sections_config = _get_sections_config()
    markdown_parts = [f"# {document_title}\n"]

    generation_tasks = []
    for section_conf in sections_config:
        section_id = section_conf["id"]
        section_title = section_conf["title"]
        section_instructions = section_conf["instructions"]

        log.info(f"Queueing LLM generation for section: {section_id} - '{section_title}'")
        task = _generate_llm_section(
            section_id=section_id,
            section_title=section_title,
            section_instructions=section_instructions,
            formatted_data_for_prompt=formatted_data_for_prompt,
            custom_prompt_section=custom_prompt_section
        )
        generation_tasks.append(task)

    try:
        log.info(f"Executing {len(generation_tasks)} section generation tasks in parallel.")
        # asyncio.gather runs tasks concurrently and returns results in the order of tasks provided.
        # _generate_llm_section is designed to return a string (content or error message),
        # so exceptions from the LLM call itself are handled within that function.
        generated_sections_content = await asyncio.gather(*generation_tasks)
        markdown_parts.extend(generated_sections_content)
        log.info("All parallel section generation tasks completed.")
    except Exception as e_gather:
        # This would catch exceptions if asyncio.gather itself fails, or if a task raises an
        # exception not caught by _generate_llm_section (which is unlikely given its try/except).
        log.error(f"Error during parallel generation of sections: {e_gather}", exc_info=True)
        # Append a generic error message; individual failed sections should have their own error messages
        # from _generate_llm_section.
        markdown_parts.append(f"\n\n## Error During Document Assembly\n\nAn error occurred while assembling the document sections: {str(e_gather)}")


    markdown_content = "\n\n".join(markdown_parts)
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
