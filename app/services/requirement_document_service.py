from sqlalchemy.orm import Session, selectinload
from sqlalchemy import desc, select
from typing import List, Optional, Dict, Any
import json

from app.models.project_model import Project
from app.models.procedure_model import Procedure
from app.models.m204_file_model import M204File
# M204Field is not directly used in queries here but is part of M204File relationship

from app.models.m204_variable_model import M204Variable
from app.models.image_statement_model import ImageStatement
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
    ImageStatementResponseSchema,
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

    # M204 Procedures
    if options.include_procedures:
        procedures_query = db.query(Procedure).filter(Procedure.project_id == project_id)
        if options.include_procedure_variables:
            procedures_query = procedures_query.options(
                selectinload(Procedure.variables_in_procedure)
            )
        procedures = procedures_query.all()
        project_data["procedures"] = [M204ProcedureResponseSchema.model_validate(p).model_dump(exclude_none=True) for p in procedures]


    # M204 Files and Fields
    if options.include_files:
        m204_files = db.query(M204File).filter(M204File.project_id == project_id)\
            .options(selectinload(M204File.fields)).all()
        project_data["m204_files"] = [M204FileResponseSchema.model_validate(f).model_dump(exclude_none=True) for f in m204_files]

    # Global/Public M204 Variables
    if options.include_global_variables:
        global_vars = db.query(M204Variable).filter(
            M204Variable.project_id == project_id,
            M204Variable.procedure_id.is_(None)
        ).all()
        project_data["global_variables"] = [M204VariableResponseSchema.model_validate(v).model_dump(exclude_none=True) for v in global_vars]

    # JCL DD Statements
    if options.include_jcl_dd_statements:
        dd_statements = db.query(DDStatement).join(InputSource, DDStatement.input_source_id == InputSource.input_source_id)\
            .filter(InputSource.project_id == project_id).all()
        project_data["dd_statements"] = [DDStatementResponseSchema.model_validate(dd).model_dump(exclude_none=True) for dd in dd_statements]

    # Image Statements
    if options.include_image_statements:
        image_statements = db.query(ImageStatement).join(InputSource, ImageStatement.input_source_id == InputSource.input_source_id)\
            .filter(InputSource.project_id == project_id).all()
        project_data["image_statements"] = [ImageStatementResponseSchema.model_validate(img).model_dump(exclude_none=True) for img in image_statements]

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

    if options.include_procedures and "procedures" in project_data and project_data["procedures"]:
        content_parts.append("M204 Procedures Data:")
        for proc in project_data["procedures"]:
            proc_details = [
                f"  - Procedure Name: {proc.get('m204_proc_name', 'N/A')}",
                f"    Type: {proc.get('m204_proc_type', 'N/A')}",
                f"    Parameters String: {proc.get('m204_parameters_string', 'None')}",
                f"    Target COBOL Program: {proc.get('target_cobol_program_name', 'N/A')}",
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
            if f_data.get("fields"):
                file_details.append("    Fields:")
                for field in f_data["fields"]:
                    file_details.append(f"      - Field Name: {field.get('field_name')}, Attributes Text: {field.get('attributes_text', 'N/A')}")
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

    if options.include_image_statements and "image_statements" in project_data and project_data["image_statements"]:
        content_parts.append("IMAGE Statements Data:")
        for img in project_data["image_statements"]:
            content_parts.append(f"  - Line Number: {img.get('line_number')}, Referenced M204 Logical Name: {img.get('referenced_m204_logical_name', 'N/A')}, Image Content Snippet (first 100 chars): `{img.get('image_content', '')[:100]}...`")
        content_parts.append("\n")

    if options.include_procedure_calls and "procedure_calls" in project_data and project_data["procedure_calls"]:
        content_parts.append("Procedure Call Relationships Data:")
        for call in project_data["procedure_calls"]:
            calling_proc_id = call.get('calling_procedure_id')
            calling_proc_name = "N/A"
            if calling_proc_id and "procedures" in project_data and project_data["procedures"]:
                caller = next((p for p in project_data["procedures"] if p.get("proc_id") == calling_proc_id), None)
                if caller: calling_proc_name = caller.get("m204_proc_name", "N/A")

            content_parts.append(f"  - Calling Procedure Name: {calling_proc_name} (ID: {calling_proc_id}), Called Procedure Name: {call.get('called_procedure_name')}, Line Number: {call.get('line_number')}, Is External: {call.get('is_external')}")
        content_parts.append("\n")

    return "\n".join(content_parts)


async def generate_and_save_project_requirements_document(
    db: Session,
    project_id: int,
    options: RequirementGenerationOptionsSchema,
    custom_prompt_section: Optional[str] = None
) -> RequirementDocumentResponseSchema:
    log.info(f"Starting requirements document generation for project ID: {project_id} with options: {options.model_dump_json()}")

    if not llm_config._llm:
        log.error("LLM is not configured. Cannot generate requirements document.")
        raise HTTPException(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, detail="LLM service is not configured or available.")

    project_data_for_llm = await _fetch_project_data_for_llm(db, project_id, options)
    formatted_data_for_prompt = _construct_llm_prompt_content(project_data_for_llm, options)

    project_name = project_data_for_llm.get("project_info", {}).get("project_name", f"Project {project_id}")
    document_title = f"Requirements Document for {project_name}"

    prompt_template = f"""
You are a senior technical analyst and writer. Your task is to generate a comprehensive Software Requirements Specification (SRS) document in Markdown format for an existing M204-based system.
The document content should be based *solely* on the structured data provided below. Do not invent information not present in the provided data.
Generate a single, well-formatted Markdown document. Your analysis should be insightful, going beyond simple listing of data where appropriate.
Emphasize the use of **paragraphs for description and bullet points for lists of attributes or related items**, rather than relying solely on tables for all data presentation. Tables can be used sparingly for very dense, tabular data if it aids clarity significantly.

**Input Data (Summary of system components and relationships):**
--- BEGIN SYSTEM DATA ---
{formatted_data_for_prompt}
--- END SYSTEM DATA ---

{f"**Additional Instructions/Custom Section from User (consider these when generating content for relevant sections):**\n{custom_prompt_section}\n" if custom_prompt_section else ""}

Please structure the Markdown document with the following sections and headings.
If data for a specific section is not available or not applicable based on the input (e.g., if the corresponding '--- BEGIN SYSTEM DATA ---' section is empty or missing), include the heading and a brief note like "Not applicable based on provided data." or "No data available for this section."

The document should start with a main title: "# Requirements Document: {project_name}"

Then, include the following sections:

## 1. Project Overview
   (Provide a narrative overview in a paragraph or two based on the 'Project Information' from the input data. Include project name, description, and creation date.)

## 2. M204 Procedures
   (Based on 'M204 Procedures Data':
    - For each procedure, write a descriptive paragraph. This paragraph should cover its name, type, and target COBOL program.
    - If a procedure summary is available in the input, incorporate it into the descriptive paragraph.
    - Detail parameters and any variables defined within the procedure using bullet points *underneath* the main descriptive paragraph for that procedure.
    - For example:
      "The procedure **SAMPLE_PROC** is an **ONLINE** type procedure. It is designed to handle user interactions for updating customer records and interfaces with the COBOL program **CUSTUPDTE**. The primary function of this procedure appears to be real-time data modification based on user input.
      * Parameters: `ACCOUNT_NUMBER, UPDATE_DATA`
      * Variables defined in procedure:
        * `%OLD_VALUE` (Type: `STRING`, Scope: `LOCAL`, COBOL Mapped Name: `WS-OLD-VAL`)
        * `%NEW_VALUE` (Type: `STRING`, Scope: `LOCAL`, COBOL Mapped Name: `WS-NEW-VAL`)"
    - Conclude this section with a brief summary paragraph discussing any observed overall purpose or common themes across the procedures.)

## 3. M204 File Definitions
   (Based on 'M204 File Definitions Data':
    - For each M204 file, write a descriptive paragraph. This paragraph should cover its name, M204 attributes, whether it's a database file, and its target VSAM dataset name and type.
    - Within the description of each file, list its fields using bullet points. Each bullet point should detail the field's name and its M204 attributes. For example:
      "The M204 file **CUSTFILE** is defined with attributes `(attribute_list)`. It is flagged as a database file and maps to the target VSAM dataset **PROD.CUSTOMER.MASTER** of type `KSDS`. This file appears to store core customer master data.
      * Fields:
        * `CUST_ID` (Attributes: `KEY, NUMERIC, ...`)
        * `CUST_NAME` (Attributes: `TEXT, ...`)
        * `CUST_ADDRESS` (Attributes: `TEXT, OCCURS 10, ...`)"
    - Conclude with a summary paragraph about the general categories or types of data managed by these files.)

## 4. Global/Public M204 Variables
   (Based on 'Global/Public M204 Variables Data':
    - Begin with an introductory paragraph about the role of global/public variables in the system.
    - List each global or public variable using bullet points. Each bullet point should include its name, type, scope, suggested COBOL mapped name, and any defined attributes.
    - Follow the list with a paragraph further explaining the potential collective purpose or usage patterns of these global variables based on their characteristics.)

## 5. JCL DD Statements
   (Based on 'JCL DD Statements Data':
    - Start with a paragraph describing the role of JCL DD statements in interfacing the M204 application with datasets.
    - List the relevant DD statements. You can use bullet points for each DD statement, detailing its DD Name, DSN, Disposition, and the Job and Step context. A compact table could also be appropriate here if there are many with similar structures.
    - Conclude with a paragraph highlighting any DD statements that seem particularly critical (e.g., for primary input/output files, connections to other systems).)

## 6. IMAGE Statements
   (Based on 'IMAGE Statements Data':
    - Describe any IMAGE statements found in a narrative form. For each statement, explain its context (line number, referenced M204 logical name if any) and provide a snippet of its content.
    - Use bullet points if listing multiple IMAGE statements, with a brief description for each.
    - Discuss the likely purpose of these IMAGE statements (e.g., dynamic screen element generation, report formatting cues) in a concluding paragraph for this section.)

## 7. Procedure Call Flow
   (Based on 'Procedure Call Relationships Data':
    - Describe the procedure call flow using narrative paragraphs to explain major sequences or interactions.
    - Supplement with bullet points to list specific call relationships, indicating the calling procedure, the called procedure, the line number of the call, and whether it's an external call.
    - Focus on making the control flow and dependencies clear. Try to identify and describe any main processing sequences, critical execution paths, or highly interconnected modules.)

## 8. Data Dictionary / Key Data Elements
   (Synthesize information from 'M204 File Definitions Data' (fields), 'Global/Public M204 Variables Data', and 'M204 Procedures Data' (variables_in_procedure).
    - Describe key data elements in paragraphs. For each element or group of related elements, discuss its apparent meaning and use.
    - Use bullet points to list specific attributes like M204 data type/attributes and where it's primarily used (e.g., in which files or procedure types).
    - If a comprehensive dictionary isn't directly inferable, provide a narrative summary of the main types of data the system processes and their significance.)

## 9. External Interfaces
   (Infer from 'JCL DD Statements Data' (DSNs) and 'M204 File Definitions Data' (attributes suggesting external links).
    - Describe in paragraph form any identified external systems, datasets, or interfaces that the M204 application interacts with. Explain the nature of these interactions if discernible.)

## 10. Non-Functional Requirements
   (Review all provided data for hints towards non-functional requirements.
    - Describe any identified NFRs (e.g., performance considerations from file attributes, security aspects from field encryption, operational constraints) in paragraph form.
    - If none are directly inferable, state that "No specific non-functional requirements were directly inferable from the provided system data.")

## 11. Other Observations / Summary
   (Provide a concluding summary of the system in narrative paragraphs. Highlight any overarching patterns, complexities, or notable observations that don't fit neatly into other sections. Address points from the 'Additional Instructions/Custom Section from User' here if not covered elsewhere.)

After the detailed sections above, include the following major section:

# Technical Requirements
   (This section is intended for a high-level technical audience like Project Management and CTO.
    It should provide a structured, detailed overview of the system's technical aspects based on the provided data.
    Synthesize and abstract information from the preceding detailed sections, focusing on technical architecture, data flow, and key components.)

### A. System Architecture Overview
   (Describe the high-level components of the M204-based system and their primary roles.
    Illustrate how M204 procedures (distinguishing between `m204_proc_type` like ONLINE, BATCH, INCLUDE, SUBROUTINE), COBOL programs (as indicated by `target_cobol_program_name` in procedures), JCL (from `DDStatement` data providing `job_name`, `step_name`, `dd_name`, `dsn`, and `disposition`), and M204 files/VSAM datasets (from `M204File` data, including `m204_file_name`, `target_vsam_dataset_name`, `target_vsam_type`) interact to form the overall system architecture.
    Mention the role of `IMAGE Statements` (from `ImageStatement` data) if they contribute significantly to user interface or report generation architecture.
    Identify any distinct subsystems or processing layers if they can be inferred from procedure groupings, naming conventions, or data flow patterns observed in the input data.)

### B. Data Model and Management
   (Detail the key data entities managed by the system, as identified from `M204 File Definitions Data` (specifically `m204_file_name` and its associated `fields`). For each M204 file, describe its purpose (e.g., master data, transactional data, index files, work files) based on its name, attributes, and fields.
    Explain how data is stored, referencing `M204File.is_db_file`, `M204File.target_vsam_dataset_name`, and `M204File.target_vsam_type` (e.g., KSDS, ESDS).
    Describe significant characteristics of the data model by analyzing `M204Field` data:
    - Key fields (e.g., those with `KEY` in `attributes_text`).
    - Data types and lengths (e.g., NUMERIC, TEXT, specific lengths from `attributes_text`).
    - Occurrences (e.g., `OCCURS` clauses in `attributes_text`).
    - Relationships implied by field names or usage across different files or procedures.
    Summarize the overall data flow:
    - Data Ingress: How data enters the system (e.g., through JCL DD statements with `disposition` SHR or OLD, or via ONLINE procedures).
    - Data Processing: How data is transformed or used internally (e.g., by M204 procedures, manipulated using variables, stored/retrieved from M204 files).
    - Data Egress: How data leaves the system (e.g., through JCL DD statements with `disposition` NEW or MOD, or reports generated via IMAGE statements or procedures).)

### C. Core Processing Logic and Control Flow
   (Explain the main processing sequences and business logic implemented within the M204 procedures, referencing their `m204_proc_type` and interactions with `target_cobol_program_name`s.
    Highlight critical procedures or call chains by analyzing `Procedure Call Relationships Data` (using `calling_procedure_name`, `called_procedure_name`, `line_number` of call, and `is_external` flag). Discuss the significance of frequently called procedures or key external calls.
    Describe how user interactions (for `ONLINE` procedures, potentially involving `IMAGE Statements`) or batch processes (inferred from `BATCH` procedure types and associated JCL `job_name`/`step_name` context) are handled.
    Discuss the role of `M204Variable`s:
    - Global/Public variables (`scope` is GLOBAL or PUBLIC): Their purpose in system-wide state management, data sharing between modules, or configuration. Refer to their `variable_name`, `variable_type`, and `cobol_mapped_variable_name`.
    - Procedure-local variables (`scope` is LOCAL): Their use within specific procedures, including `m204_parameters_string` for input/output to procedures.
    Analyze patterns in variable usage if discernible.)

### D. External Interfaces and Dependencies
   (List and describe all identified external systems, datasets, or services that the M204 application interacts with.
    Base this on:
    - `JCL DD Statements Data`: Analyze `dsn`s that point to external (non-system specific) files or GDGs. Note the `disposition` (e.g., NEW, MOD for outputs; OLD, SHR for inputs) to understand data direction.
    - `M204 File Definitions Data`: Check if `m204_attributes` or `target_vsam_dataset_name` suggest links to shared datasets with other applications or standard interface files.
    - `Procedure Call Relationships Data`: `is_external` calls might indicate interfaces to other systems or shared utilities if `called_procedure_name` implies this.
    Specify the nature of these interfaces (e.g., file-based batch data exchange, shared VSAM datasets, triggers to/from other systems if inferable). Discuss their importance to the system's overall functionality and data integrity.)

### E. Technology Stack and Environment
   (Summarize the core technologies used, based on available data:
    - **Model 204:** Note its role (e.g., primary application logic, database). Mention specific features used if inferable from `Procedure.m204_proc_type` (ONLINE, BATCH, INCLUDE, SUBROUTINE indicating User Language/SOUL usage), `M204File.m204_attributes`, or `M204Field.attributes_text`.
    - **COBOL:** Its role (e.g., complex business logic, batch processing) as indicated by `Procedure.target_cobol_program_name`.
    - **JCL:** Its function in orchestrating batch jobs, file management, and program execution, detailed by `DDStatement` data (`job_name`, `step_name`, `dsn`, `disposition`).
    - **VSAM:** The types of datasets used (e.g., KSDS, ESDS from `M204File.target_vsam_type`) and their purpose.
    Discuss any technical constraints or environmental considerations inferred from the system data:
    - Dataset Naming Conventions: Patterns observed in `dsn` or `target_vsam_dataset_name`.
    - Batch Windows/Scheduling: Implied by JCL structure or batch procedure design.
    - Security Mechanisms: Any hints from file attributes, variable naming, or procedure structure (though likely limited from this data).
    - Character Sets/Encoding: If any hints in `attributes_text` or `image_content`.)

### F. Key Non-Functional Aspects (Inferred)
   (Based on the detailed analysis in section 10 and the overall system data, reiterate and detail any significant non-functional requirements or characteristics critical from a technical perspective. Be specific by referencing the data points that lead to these inferences.
    - **Performance:**
      - Identify potential bottlenecks or performance-critical areas (e.g., frequently accessed files indicated by `KEY` fields in `M204Field.attributes_text`, complex `ONLINE` procedures, large data volumes suggested by `OCCURS` clauses or file structures).
      - Discuss indexing strategies (`KEY` fields) and their impact.
    - **Data Integrity and Consistency:**
      - Mechanisms implied by field data types and attributes (e.g., `NUMERIC` validation, length constraints).
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

Ensure the entire output is a single Markdown text block, ready to be saved.
Do not add any preamble or explanation before the first line of the Markdown document (which should be the title: "# Requirements Document: {project_name}").
"""
    log.debug(f"LLM prompt for requirements document generation (Project ID: {project_id}):\n{prompt_template[:4000]}...")

    markdown_content = ""
    try:
        completion_response = await llm_config._llm.acomplete(prompt=prompt_template)

        if completion_response and completion_response.text:
            markdown_content = completion_response.text
            expected_title = f"# Requirements Document: {project_name}"
            normalized_content_start = markdown_content.lstrip().replace('\r\n', '\n').replace('\r', '\n')
            normalized_expected_title = expected_title.replace('\r\n', '\n').replace('\r', '\n')

            if not normalized_content_start.startswith(normalized_expected_title):
                log.warning(f"LLM output did not start with the expected title. Expected: '{expected_title}'. Got: '{markdown_content[:200]}'. Prepending title.")
                markdown_content = f"{expected_title}\n\n{markdown_content.lstrip()}"
            log.info(f"LLM successfully generated markdown content for project {project_id}. Total length: {len(markdown_content)}")
        else:
            log.warning(f"LLM returned empty or no text content for project {project_id}.")
            markdown_content = f"# Requirements Document: {project_name}\n\n## 1. Project Overview\nNo data available or LLM failed to generate content.\n\n(Further sections would follow based on template)"


        if not markdown_content.strip() or len(markdown_content.strip()) < len(f"# Requirements Document: {project_name}") + 50: # Basic check for meaningful content
            log.warning(f"LLM generated empty or minimal content for project {project_id}. Content: '{markdown_content[:200]}'")
            # Ensure at least the title and a note if content is truly empty
            if not markdown_content.strip():
                 markdown_content = f"# Requirements Document: {project_name}\n\nNo content was generated by the LLM."
            elif not f"# Technical Requirements (for PMO/CTO)" in markdown_content: # If the new section is missing, add a placeholder
                 markdown_content += f"\n\n# Technical Requirements (for PMO/CTO)\n\nNo specific technical requirements could be generated based on LLM output for this section."


    except HTTPException as he: # Re-raise HTTPExceptions directly
        raise he
    except Exception as e_llm:
        log.error(f"LLM completion error for project {project_id}: {e_llm}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"LLM failed to generate document: {str(e_llm)}")

    doc_create_data = RequirementDocumentCreateSchema(
        project_id=project_id,
        document_title=document_title,
        markdown_content=markdown_content,
        generation_options_json=options.model_dump() # Store options used
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

    # Prepare response, including the options used for generation
    response_data = RequirementDocumentResponseSchema.model_validate(db_document)
    response_data.generation_options_used = options # Attach the Pydantic model directly

    return response_data

# --- CRUD functions for RequirementDocument ---

async def get_requirement_document_by_id(db: Session, requirement_document_id: int) -> Optional[RequirementDocumentResponseSchema]:
    stmt = select(RequirementDocument).where(RequirementDocument.requirement_document_id == requirement_document_id)
    db_document = db.execute(stmt).scalar_one_or_none()
    if db_document:
        response = RequirementDocumentResponseSchema.model_validate(db_document)
        if db_document.generation_options_json: # This is Dict[str, Any] or None
            try:
                # The model_validate should handle dict directly if it's already parsed
                # If it's a string from older storage, it needs parsing.
                options_data = db_document.generation_options_json
                if isinstance(options_data, str):
                    options_data = json.loads(options_data) # pragma: no cover
                response.generation_options_used = RequirementGenerationOptionsSchema(**options_data)
            except Exception as e_parse:
                log.warning(f"Could not parse generation_options_json for doc ID {requirement_document_id}: {e_parse}. Data: {db_document.generation_options_json}")
                response.generation_options_used = None # Or some default/empty schema
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
                    options_data = json.loads(options_data) # pragma: no cover
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
                    options_data = json.loads(options_data) # pragma: no cover
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
    # Ensure generation_options_json is a dict before model_dump if it's passed as string
    # However, RequirementDocumentCreateSchema expects it as Dict[str, Any] or None
    # If it's coming in as a string, it should be parsed by the caller or the route handler.
    # For robustness, we can check here too.
    if isinstance(doc_data.generation_options_json, str):
        try:
            doc_data.generation_options_json = json.loads(doc_data.generation_options_json) # pragma: no cover
        except json.JSONDecodeError as e:
            log.error(f"Error decoding generation_options_json string in create_requirement_document: {e}") # pragma: no cover
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid JSON format for generation_options_json.") # pragma: no cover

    db_model_data = doc_data.model_dump()
    # Ensure generation_options_json is stored as JSON (it should be a dict from model_dump)
    # The model RequirementDocument has generation_options_json: Mapped[Optional[Dict[str, Any]]] = mapped_column(JSON, nullable=True)
    # So, SQLAlchemy handles the dict to JSON conversion.

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
    if db_document.generation_options_json: # This will be a dict
        try:
            # generation_options_json from db_document is already a dict
            response.generation_options_used = RequirementGenerationOptionsSchema(**db_document.generation_options_json)
        except Exception as e_parse: # Should not happen if data is valid
            log.warning(f"Could not parse generation_options_json from created document for response: {e_parse}. Data: {db_document.generation_options_json}") # pragma: no cover
            response.generation_options_used = None # pragma: no cover
    return response