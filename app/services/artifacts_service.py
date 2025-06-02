from sqlalchemy.orm import Session, selectinload, joinedload
from fastapi import HTTPException
import uuid
import datetime
from typing import List, Optional, Dict, Any
import json
import os
import asyncio
import re

from app.models.m204_file_model import M204File
from app.models.project_model import Project
from app.models.procedure_model import Procedure
from app.models.input_source_model import InputSource

# Import Pydantic Schemas for artifact data structures
from app.schemas.artifacts_schema import (
    GeneratedArtifactsResponse,
    InputSourceArtifacts,
    GeneratedFileContent,
    CobolOutputSchema,
    JclOutputSchema,
    UnitTestOutputSchema
)

# Import SQLAlchemy ORM Models for database persistence
from app.models.generated_cobol_artifact_model import GeneratedCobolArtifact
from app.models.generated_jcl_artifact_model import GeneratedJclArtifact
from app.models.generated_unit_test_artifact_model import GeneratedUnitTestArtifact

from app.utils.logger import log
from app.config.llm_config import llm_config
from pydantic import BaseModel, Field as PydanticField # Added PydanticField

LLM_API_CALL_BATCH_SIZE = 5


class TestCase(BaseModel):
    test_case_id: str = PydanticField(description="A unique identifier for the test case (e.g., TC_001, TC_VALID_INPUT).")
    description: str = PydanticField(description="A brief description of what this test case covers.")
    preconditions: Optional[List[str]] = PydanticField(description="Any preconditions or setup required.", default_factory=list)
    inputs: Dict[str, Any] = PydanticField(description="Key-value pairs of input parameters or %variables and their test values.")
    expected_outputs: Dict[str, Any] = PydanticField(description="Key-value pairs of expected output %variables, screen elements, or file states and their values.")
    expected_behavior_description: str = PydanticField(description="A textual description of the expected behavior, side effects, or outcome.")

class M204ProcedureToCobolOutput(BaseModel):
    m204_procedure_name: str = PydanticField(description="Original M204 procedure name.")
    cobol_code_block: str = PydanticField(description="Generated COBOL code block.")
    comments: Optional[str] = PydanticField(description="Conversion comments.", default=None)

class FileDefinitionToCobolFDOutput(BaseModel):
    logical_file_name: Optional[str] = PydanticField(description="M204 logical name for SELECT.", default=None)
    file_control_entry: str = PydanticField(description="COBOL SELECT statement.")
    file_description_entry: str = PydanticField(description="COBOL FD entry.")
    working_storage_entries: Optional[str] = PydanticField(description="Optional WORKING-STORAGE items.", default=None)
    comments: Optional[str] = PydanticField(description="Conversion comments.", default=None)

class VsamJclGenerationOutput(BaseModel):
    """Structured output for VSAM JCL generation by LLM."""
    jcl_content: str = PydanticField(description="The fully generated JCL content for defining the VSAM cluster using IDCAMS.")
    generation_comments: Optional[str] = PydanticField(description="Any comments or notes from the LLM regarding the JCL generation process or assumptions made.", default=None)

class ArtifactsService:
    def __init__(self, db: Session):
        self.db = db
        self.llm_semaphore = asyncio.Semaphore(LLM_API_CALL_BATCH_SIZE)

    def _sanitize_filename_base(self, name: str, default_prefix="PROG") -> str:
        if not name:
            candidate_name = f"{default_prefix}{uuid.uuid4().hex[:max(0, 8-len(default_prefix))].upper()}"
        else:
            name_without_ext = os.path.splitext(name)[0]
            sanitized_for_cobol_id = re.sub(r'[^a-zA-Z0-9]', '', name_without_ext)
            candidate_name = sanitized_for_cobol_id.upper()

        cobol_program_id_base = candidate_name[:8]

        if not cobol_program_id_base or not cobol_program_id_base[0].isalpha():
            prefixed_candidate = default_prefix + cobol_program_id_base
            cobol_program_id_base = prefixed_candidate[:8]
            
            if not cobol_program_id_base or not cobol_program_id_base[0].isalpha():
                cobol_program_id_base = f"{default_prefix if default_prefix and default_prefix[0].isalpha() else 'CBL'}{uuid.uuid4().hex[:max(0, 8-(len(default_prefix) if default_prefix and default_prefix[0].isalpha() else 3))].upper()}"
                cobol_program_id_base = cobol_program_id_base[:8]
        return cobol_program_id_base

    async def _llm_convert_m204_proc_to_cobol(self, proc: Procedure) -> M204ProcedureToCobolOutput:
        if not llm_config._llm or not proc.procedure_content:
            log.warning(f"LLM not available or no content for M204 procedure: {proc.m204_proc_name}. Returning placeholder COBOL.")
            return M204ProcedureToCobolOutput(
                m204_procedure_name=proc.m204_proc_name,
                cobol_code_block=f"      * --- Placeholder COBOL for M204 Procedure: {proc.m204_proc_name} ---\n"
                                 f"      DISPLAY 'Executing M204 Procedure Logic for: {proc.m204_proc_name}'.\n"
                                 f"      * --- End of Placeholder for {proc.m204_proc_name} ---",
                comments="LLM not available or procedure content missing. Manual conversion required. Ensure COBOL6 standard."
            )
        
        # Sanitize procedure name for use in example in prompt
        example_proc_name_sanitized = proc.m204_proc_name.upper().replace('%', 'P').replace('$', 'D').replace('_', '-').replace('#','N')
        if not example_proc_name_sanitized or not (example_proc_name_sanitized[0].isalpha() or example_proc_name_sanitized[0].isdigit()):
            example_proc_name_sanitized = "M204PROC" + example_proc_name_sanitized
        # Further ensure it's a valid COBOL paragraph name start if still problematic after prefix
        if not (example_proc_name_sanitized[0].isalpha() or example_proc_name_sanitized[0].isdigit()):
             example_proc_name_sanitized = "DEFAULT-" + example_proc_name_sanitized # Add a default prefix if necessary
        example_proc_name_sanitized = re.sub(r'[^A-Z0-9-]', '', example_proc_name_sanitized)[:28]

        prompt_fstr = f"""
You are an expert M204 to COBOL migration specialist.
Convert the following M204 procedure into a COBOL code block. This block will be inserted directly into the PROCEDURE DIVISION of a larger COBOL program, inside an automatically generated COBOL paragraph (e.g., `{example_proc_name_sanitized}-PARA.`).

M204 Procedure Name: {proc.m204_proc_name}
M204 Parameters: {proc.m204_parameters_string or "None"}
M204 Procedure Content:
```m204
{proc.procedure_content}
```

Your task is to generate *only* the COBOL statements that form the logic of this M204 procedure.
The generated `cobol_code_block` in your JSON response:
- MUST be suitable for direct inclusion within such a COBOL paragraph.
- MUST NOT start with a paragraph name or section definition itself (e.g., do not include `MY-PARA.` or `MY-SECTION SECTION.` at the beginning of the block, as the surrounding paragraph is already provided).
- MUST NOT include `IDENTIFICATION DIVISION`, `ENVIRONMENT DIVISION`, `DATA DIVISION`, or the `PROCEDURE DIVISION.` header itself.
- SHOULD be well-structured, using COBOL paragraphs or sections within the block if appropriate for complex logic, and adhere to the COBOL6 standard.
- MUST assume all required data items (variables, counters, file records, etc.) are already defined in the main program's `DATA DIVISION` (specifically `FILE SECTION` or `WORKING-STORAGE SECTION`). Do not generate any `DATA DIVISION` entries or `FD`s within the `cobol_code_block`. Your code should use variable names as if they exist globally.

Respond with a JSON object structured according to the M204ProcedureToCobolOutput model:
```json
{{
  "m204_procedure_name": "{proc.m204_proc_name}",
  "cobol_code_block": "string",
  "comments": "string (optional)"
}}
```
Example of a valid `cobol_code_block` content (this would be placed inside a paragraph like `{example_proc_name_sanitized}-PARA.`):
```cobol
      DISPLAY 'Executing logic for {proc.m204_proc_name}'.
      MOVE ZEROS TO SOME-COUNTER.
      ADD 1 TO ANOTHER-VARIABLE.
      IF SOME-CONDITION PERFORM SOME-OTHER-LOGIC-PARA.
      IF ANOTHER-CONDITION
          MOVE 'X' TO SOME-FLAG
      ELSE
          MOVE 'Y' TO SOME-FLAG
      END-IF.
```
Ensure the `cobol_code_block` contains only the procedural COBOL statements, correctly indented for inclusion within a paragraph (typically starting in Area B, column 12 or further).
"""
        json_text_output: Optional[str] = None
        try:
            async with self.llm_semaphore:
                log.debug(f"Attempting LLM call for M204 procedure to COBOL: {proc.m204_proc_name} (semaphore acquired)")
                llm_call = llm_config._llm.as_structured_llm(M204ProcedureToCobolOutput)
                response = await llm_call.acomplete(prompt=prompt_fstr)
                json_text_output = response.text
            log.debug(f"LLM call for M204 procedure to COBOL: {proc.m204_proc_name} completed (semaphore released)")
            return M204ProcedureToCobolOutput(**json.loads(json_text_output))
        except Exception as e:
            log.error(f"LLM error converting M204 procedure {proc.m204_proc_name} to COBOL: {e}. Raw output: {json_text_output}", exc_info=True)
            return M204ProcedureToCobolOutput(
                m204_procedure_name=proc.m204_proc_name,
                cobol_code_block=f"      * --- Error during COBOL conversion for M204 Procedure: {proc.m204_proc_name} ---\n"
                                 f"      DISPLAY 'Error in logic for: {proc.m204_proc_name}'.\n"
                                 f"      * --- See logs for details ---",
                comments=f"LLM conversion failed: {str(e)}"
            )


    async def _llm_convert_file_definition_to_fd(self, m204_file: M204File) -> FileDefinitionToCobolFDOutput:
        """Convert M204 file definition JSON to COBOL FD using LLM"""
        if not llm_config._llm or not m204_file.file_definition_json:
            log.warning(f"LLM not available or no file definition for M204 file: {m204_file.m204_file_name}. Returning placeholder FD.")
            select_name = (m204_file.m204_logical_dataset_name or m204_file.m204_file_name or f"FILE{m204_file.m204_file_id}").replace("-", "")[:8]
            return FileDefinitionToCobolFDOutput(
                logical_file_name=select_name,
                file_control_entry=f"       SELECT {select_name}-FILE ASSIGN TO {select_name}.\n",
                file_description_entry=f"   FD  {select_name}-FILE.\n"
                                     f"   01  {select_name}-RECORD PIC X(80). *> Placeholder\n",
                comments="LLM not available or file definition missing. Manual FD creation required."
            )

        file_definition = m204_file.file_definition_json
        file_type = file_definition.get('file_type', 'unknown')
        
        # Build field information string from JSON
        field_info_parts = []
        if file_type == "db_file":
            # DB file with PARMLIB field definitions
            fields = file_definition.get('fields', {})
            for field_name, field_data in fields.items():
                attributes = field_data.get('attributes', [])
                vsam_suggestions = field_data.get('vsam_suggestions', {})
                field_info = f"- Field: {field_name}, Attributes: {', '.join(attributes)}"
                if vsam_suggestions.get('cobol_picture_clause'): # MODIFIED HERE
                    field_info += f", Suggested COBOL PIC: {vsam_suggestions['cobol_picture_clause']}" # MODIFIED HERE
                field_info_parts.append(field_info)
        elif file_type == "flat_file":
            # Flat file with IMAGE definitions
            image_definitions = file_definition.get('image_definitions', [])
            for image_def in image_definitions:
                image_name = image_def.get('image_name', 'UNKNOWN')
                fields = image_def.get('fields', [])
                field_info_parts.append(f"- IMAGE: {image_name}")
                for field in fields:
                    field_name = field.get('field_name', 'UNKNOWN')
                    data_type = field.get('data_type', 'UNKNOWN')
                    length = field.get('length', '')
                    # If IMAGE fields also have 'cobol_picture_clause' in their 'cobol_layout_suggestions'
                    # you might want to include it here too.
                    # For now, sticking to the original structure for flat_file IMAGE fields.
                    suggested_cobol_pic = field.get('cobol_layout_suggestions', {}).get('cobol_picture_clause')
                    field_info_parts.append(f"  - Field: {field_name}, Type: {data_type}, Length: {length}{f', Suggested COBOL PIC: {suggested_cobol_pic}' if suggested_cobol_pic else ''}")
        elif file_type == "mixed":
            # Combined DB and flat file definitions
            field_info_parts.append("- Mixed file type with both DB and flat definitions")
            db_def = file_definition.get('db_file_definition', {})
            flat_def = file_definition.get('flat_file_definition', {})
            # Process both definitions
            if db_def.get('fields'):
                field_info_parts.append("  DB Fields:")
                for field_name, field_data in db_def['fields'].items():
                    attributes = field_data.get('attributes', [])
                    vsam_suggestions = field_data.get('vsam_suggestions', {})
                    field_info = f"    - {field_name}: {', '.join(attributes)}"
                    if vsam_suggestions.get('cobol_picture_clause'): # MODIFIED HERE
                        field_info += f", Suggested COBOL PIC: {vsam_suggestions['cobol_picture_clause']}" # MODIFIED HERE
                    field_info_parts.append(field_info)

            if flat_def.get('image_definitions'):
                field_info_parts.append("  IMAGE Definitions:")
                for image_def in flat_def['image_definitions']:
                    image_name = image_def.get('image_name', 'UNKNOWN')
                    field_info_parts.append(f"    - IMAGE: {image_name}")
                    for field in image_def.get('fields', []):
                        field_name = field.get('field_name', 'UNKNOWN')
                        data_type = field.get('data_type', 'UNKNOWN')
                        length = field.get('length', '')
                        suggested_cobol_pic = field.get('cobol_layout_suggestions', {}).get('cobol_picture_clause')
                        field_info_parts.append(f"      - Field: {field_name}, Type: {data_type}, Length: {length}{f', Suggested COBOL PIC: {suggested_cobol_pic}' if suggested_cobol_pic else ''}")


        field_info_str = "\n".join(field_info_parts) if field_info_parts else "No field information available."

        prompt_fstr = f"""
You are an expert M204 to COBOL migration specialist.
Convert the following M204 file definition into COBOL FILE-CONTROL (SELECT) and FILE SECTION (FD) entries.

M204 File Information:
- File Name (DDNAME): {m204_file.m204_file_name or 'UNKNOWN'}
- Logical Dataset Name: {m204_file.m204_logical_dataset_name or 'Not specified'}
- File Type: {file_type}
- Is DB File: {m204_file.is_db_file}

Field/Layout Information:
{field_info_str}

File Definition JSON (for detailed structure if needed by LLM):
{json.dumps(file_definition, indent=2)}

Generate appropriate COBOL FILE-CONTROL and FD entries based on this M204 file definition.
For DB files, create record layouts based on the PARMLIB field definitions and their 'Suggested COBOL PIC'.
For flat files, use the IMAGE statement field definitions and their 'Suggested COBOL PIC'.
For mixed files, prioritize the most appropriate definition for COBOL conversion, using 'Suggested COBOL PIC' where available.

The `logical_file_name` in the output JSON should be a COBOL-friendly name derived from the M204 file name (e.g., M204 DDNAME 'MYFILE01' could become 'MYFILE01' or 'MYFILE'). This name will be used in the `SELECT ... ASSIGN TO {{{{logical_file_name}}}}` and `FD  {{{{logical_file_name}}}}-FILE.`
The `file_control_entry` should be the complete SELECT statement.
The `file_description_entry` should be the complete FD, including the 01 record level and all 05 field levels with their PICTURE clauses.
If `working_storage_entries` are needed (e.g., for complex redefines or specific counters related to this FD), include them.

Respond with a JSON object structured according to the FileDefinitionToCobolFDOutput model:
```json
{{
  "logical_file_name": "string",
  "file_control_entry": "string",
  "file_description_entry": "string",
  "working_storage_entries": "string (optional)",
  "comments": "string (optional)"
}}
```
Ensure the FD record layout (01 and 05 levels) is complete and uses the 'Suggested COBOL PIC' from the field information provided.
"""

        json_text_output: Optional[str] = None
        try:
            async with self.llm_semaphore:
                log.debug(f"Attempting LLM call for FD conversion: {m204_file.m204_file_name} (semaphore acquired)")
                llm_call = llm_config._llm.as_structured_llm(FileDefinitionToCobolFDOutput)
                response = await llm_call.acomplete(prompt=prompt_fstr)
                json_text_output = response.text
            log.debug(f"LLM call for FD conversion: {m204_file.m204_file_name} completed (semaphore released)")
            return FileDefinitionToCobolFDOutput(**json.loads(json_text_output))
        except Exception as e:
            log.error(f"LLM error converting file definition for {m204_file.m204_file_name} to FD: {e}. Raw output: {json_text_output}", exc_info=True)
            select_name = (m204_file.m204_logical_dataset_name or m204_file.m204_file_name or f"FILE{m204_file.m204_file_id}").replace("-", "")[:8]
            return FileDefinitionToCobolFDOutput(
                logical_file_name=select_name,
                file_control_entry=f"       SELECT {select_name}-FILE ASSIGN TO {select_name}. *> ERROR IN CONVERSION\n",
                file_description_entry=f"   FD  {select_name}-FILE. *> ERROR IN CONVERSION\n"
                                     f"   01  {select_name}-RECORD PIC X(80). *> Placeholder due to error\n",
                comments=f"LLM FD conversion failed: {str(e)}"
            )

    def _clear_existing_artifacts_for_input_source(self, input_source_id: int):
        self.db.query(GeneratedCobolArtifact).filter(GeneratedCobolArtifact.input_source_id == input_source_id).delete(synchronize_session=False)
        self.db.query(GeneratedJclArtifact).filter(GeneratedJclArtifact.input_source_id == input_source_id).delete(synchronize_session=False)
        self.db.query(GeneratedUnitTestArtifact).filter(GeneratedUnitTestArtifact.input_source_id == input_source_id).delete(synchronize_session=False)
        log.info(f"Cleared existing artifacts for input_source_id: {input_source_id}")

    

    async def _llm_generate_vsam_jcl(self, m204_file_obj: M204File, cobol_program_id_base: str, vsam_ds_name: str, vsam_type: str, input_source_name_for_comments: str) -> VsamJclGenerationOutput:
        if not llm_config._llm:
            log.warning(f"LLM not available for VSAM JCL generation for M204 File: {m204_file_obj.m204_file_name}. Returning placeholder JCL structure.")
            raise ValueError("LLM not available")

        # Extract field details from JSON structure
        field_details_str_parts = []
        if m204_file_obj.file_definition_json:
            file_definition = m204_file_obj.file_definition_json
            file_type = file_definition.get('file_type', 'unknown')
            
            if file_type == "db_file":
                fields = file_definition.get('fields', {})
                for field_name, field_data in fields.items():
                    attributes = field_data.get('attributes', [])
                    vsam_suggestions = field_data.get('vsam_suggestions', {})
                    
                    field_info = f"- Field Name: {field_name}, Attributes: {', '.join(attributes)}"
                    if vsam_suggestions.get('cobol_picture_clause'): # MODIFIED HERE
                        field_info += f", COBOL PIC: {vsam_suggestions['cobol_picture_clause']}" # MODIFIED HERE
                    if vsam_suggestions.get('vsam_length'):
                        field_info += f", Length: {vsam_suggestions['vsam_length']}"
                    if vsam_suggestions.get('is_key_component'):
                        key_order = vsam_suggestions.get('key_order', 'N/A')
                        field_info += f", Is Key Component: Yes (Order: {key_order})"
                    field_details_str_parts.append(field_info)
            elif file_type == "flat_file": # For flat files, VSAM JCL might be less common, but if suggestions exist, use them
                image_definitions = file_definition.get('image_definitions', [])
                for image_def in image_definitions:
                    fields = image_def.get('fields', [])
                    for field in fields:
                        field_name = field.get('field_name', 'UNKNOWN')
                        data_type = field.get('data_type', 'UNKNOWN') # Original type
                        length = field.get('length', '') # Original length
                        
                        # Check if vsam_suggestions exist at the field level for flat files
                        vsam_suggestions = field.get('vsam_suggestions', {})
                        cobol_pic_suggestion = field.get('cobol_layout_suggestions', {}).get('cobol_picture_clause')


                        field_info = f"- Field Name: {field_name}, M204 Type: {data_type}, M204 Length: {length}"
                        if cobol_pic_suggestion:
                             field_info += f", Suggested COBOL PIC: {cobol_pic_suggestion}"
                        if vsam_suggestions.get('vsam_length'): # If specific VSAM length was suggested
                            field_info += f", Suggested VSAM Length: {vsam_suggestions['vsam_length']}"
                        if vsam_suggestions.get('is_key_component'):
                            key_order = vsam_suggestions.get('key_order', 'N/A')
                            field_info += f", Is Key Component: Yes (Order: {key_order})"
                        field_details_str_parts.append(field_info)
            # Note: 'mixed' file type might need more specific handling if VSAM JCL is relevant for its parts.
            # For now, it would fall through and might not provide detailed fields unless one of its sub-definitions is processed.

        field_details_str = "\n".join(field_details_str_parts) if field_details_str_parts else "No detailed field information available. LLM should use sensible defaults for RECORDSIZE and KEYS if KSDS."

        prompt_fstr = f"""
You are an expert Mainframe JCL and VSAM specialist.
Generate the JCL (IDCAMS control statements) to define a VSAM cluster based on the provided M204 file and field information.
The JCL should include steps to DELETE the cluster (PURGE) first, then DEFINE CLUSTER.
Ensure MAXCC=0 is set after DELETE if the dataset might not exist.
Use standard conventions for VSAM definition.

M204 File Information:
- M204 File Name (DDNAME): {m204_file_obj.m204_file_name or 'UNKNOWN_DDNAME'}
- Target VSAM Dataset Name: {vsam_ds_name}
- Target VSAM Type: {vsam_type} (e.g., KSDS, ESDS, RRDS)
- Associated COBOL Program Base ID (for JOB name context): {cobol_program_id_base}
- Original M204 Input Source: {input_source_name_for_comments}

Field Information (if available, use this to determine RECORDSIZE and KEYS for KSDS). 'COBOL PIC' provides the COBOL picture clause, and 'Length' provides the storage length for VSAM:
{field_details_str}

Key Requirements for DEFINE CLUSTER:
- NAME: Use the Target VSAM Dataset Name.
- For KSDS: Determine KEYS(length offset) from fields marked as 'Is Key Component: Yes'. Sum their 'Length' values. Offset is usually 0 for the primary key.
- For ESDS: Use NONINDEXED.
- For RRDS: Use NUMBERED.
- RECORDSIZE(average maximum): Determine from the sum of 'Length' for all fields if available. The 'COBOL PIC' can help infer lengths if 'Length' is missing for some fields. Otherwise, use a sensible default like (80 80) or (100 100).
- VOLUMES: Use a placeholder like VOL001.
- FREESPACE: Use a common default like (10 10).
- SHAREOPTIONS: Use a common default like (2 3).
- DATA component: NAME(<vsam_ds_name>.DATA), TRACKS(5 1) (or other reasonable primary/secondary allocation).
- INDEX component (for KSDS): NAME(<vsam_ds_name>.INDEX), TRACKS(1 1) (or other reasonable primary/secondary allocation).

JCL Structure:
//JOBVSAM  JOB (ACCT),'DEFINE VSAM {cobol_program_id_base}',CLASS=A,MSGCLASS=X
//* JCL for M204 DB File: {m204_file_obj.m204_file_name or f'FileID_{m204_file_obj.m204_file_id}'}
//* From Input Source: {input_source_name_for_comments}
//* Target VSAM: {vsam_ds_name}, Type: {vsam_type}
//STEP005  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE {vsam_ds_name} CLUSTER PURGE
  SET MAXCC=0
  DEFINE CLUSTER ( ... )
  /*
/*

Respond with a JSON object structured according to the VsamJclGenerationOutput model.
The `jcl_content` should be the complete JCL.
"""
        json_text_output: Optional[str] = None
        try:
            async with self.llm_semaphore:
                log.info(f"Attempting LLM call for VSAM JCL generation: {m204_file_obj.m204_file_name} (semaphore acquired)")
                llm_call = llm_config._llm.as_structured_llm(VsamJclGenerationOutput)
                response = await llm_call.acomplete(prompt=prompt_fstr)
                json_text_output = response.text
            log.info(f"LLM call for VSAM JCL generation: {m204_file_obj.m204_file_name} completed (semaphore released). Raw output: {json_text_output}")
            return VsamJclGenerationOutput(**json.loads(json_text_output))
        except Exception as e:
            log.error(f"LLM error generating VSAM JCL for {m204_file_obj.m204_file_name}: {e}. Raw output: {json_text_output}", exc_info=True)
            raise
    

    def _generate_fallback_vsam_jcl(self, m204_file_obj: M204File, cobol_program_id_base: str, vsam_ds_name: str, vsam_type: str, input_source_name_for_comments: str) -> str:
        log.info(f"Generating fallback VSAM JCL for {vsam_ds_name}")
        
        # Determine RECORDSIZE from JSON field definitions
        avg_rec_len_val = 0
        if m204_file_obj.file_definition_json:
            file_definition = m204_file_obj.file_definition_json
            file_type = file_definition.get('file_type', 'unknown')
            
            if file_type == "db_file":
                fields = file_definition.get('fields', {})
                total_len = 0
                for field_name, field_data in fields.items():
                    vsam_suggestions = field_data.get('vsam_suggestions', {})
                    field_length = vsam_suggestions.get('vsam_length', 0)
                    if field_length and field_length > 0:
                        total_len += field_length
                if total_len > 0:
                    avg_rec_len_val = total_len
            elif file_type == "flat_file":
                image_definitions = file_definition.get('image_definitions', [])
                for image_def in image_definitions:
                    fields = image_def.get('fields', [])
                    total_len = 0
                    for field in fields:
                        field_length = field.get('length', 0)
                        if field_length and field_length > 0:
                            total_len += field_length
                    if total_len > avg_rec_len_val:
                        avg_rec_len_val = total_len
        
        avg_rec_len = str(avg_rec_len_val) if avg_rec_len_val > 0 else "80"
        max_rec_len = avg_rec_len

        # Determine KEYS for KSDS from JSON field definitions
        key_info_str = "6 0"  # Default
        if vsam_type.upper() == "KSDS" and m204_file_obj.file_definition_json:
            file_definition = m204_file_obj.file_definition_json
            file_type = file_definition.get('file_type', 'unknown')
            
            key_components = []
            if file_type == "db_file":
                fields = file_definition.get('fields', {})
                for field_name, field_data in fields.items():
                    vsam_suggestions = field_data.get('vsam_suggestions', {})
                    if vsam_suggestions.get('is_key_component'):
                        key_order = vsam_suggestions.get('key_order', 999)
                        field_length = vsam_suggestions.get('vsam_length', 0)
                        if field_length > 0:
                            key_components.append((key_order, field_length, field_name))
            elif file_type == "flat_file":
                image_definitions = file_definition.get('image_definitions', [])
                for image_def in image_definitions:
                    fields = image_def.get('fields', [])
                    for field in fields:
                        vsam_suggestions = field.get('vsam_suggestions', {})
                        if vsam_suggestions.get('is_key_component'):
                            key_order = vsam_suggestions.get('key_order', 999)
                            field_length = field.get('length', 0)
                            if field_length > 0:
                                key_components.append((key_order, field_length, field.get('field_name', 'UNKNOWN')))
            
            if key_components:
                key_components.sort(key=lambda x: x[0])  # Sort by key_order
                total_key_length = sum(comp[1] for comp in key_components)
                if total_key_length > 0:
                    key_info_str = f"{total_key_length} 0"
                    log.info(f"Fallback JCL: Derived KEYS for {vsam_ds_name} from JSON fields: {key_info_str}. Fields: {[comp[2] for comp in key_components]}")
            else:
                # Fallback to primary_key_field_name if no key components found
                if m204_file_obj.primary_key_field_name and re.match(r"^\d+\s+\d+$", m204_file_obj.primary_key_field_name):
                    key_info_str = m204_file_obj.primary_key_field_name
                    log.info(f"Fallback JCL: Using KEYS from M204File.primary_key_field_name: {key_info_str} for {vsam_ds_name}")

        jcl_content = f"""\
//JOBVSAM  JOB (ACCT),'DEFINE VSAM {cobol_program_id_base}',CLASS=A,MSGCLASS=X
//* JCL for M204 DB File: {m204_file_obj.m204_file_name or f'FileID_{m204_file_obj.m204_file_id}'} (Fallback Generation)
//* From Input Source: {input_source_name_for_comments}
//* Target VSAM: {vsam_ds_name}, Type: {vsam_type}
//STEP005  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE {vsam_ds_name} CLUSTER PURGE
  SET MAXCC=0
  DEFINE CLUSTER (NAME({vsam_ds_name}) -
"""
        if vsam_type.upper() == "KSDS":
            jcl_content += f"    INDEXED - KEYS({key_info_str}) -\n"
        elif vsam_type.upper() == "ESDS":
            jcl_content += "    NONINDEXED -\n"
        elif vsam_type.upper() == "RRDS":
            jcl_content += "    NUMBERED -\n"
        else: 
            jcl_content += f"    /* VSAM type: {vsam_type}. Add appropriate parameters. */ -\n"
        
        jcl_content += f"""\
    RECORDSIZE({avg_rec_len} {max_rec_len}) -
    VOLUMES(VOL001) -
    FREESPACE(10 10) -
    SHAREOPTIONS(2 3)) -
  DATA (NAME({vsam_ds_name}.DATA) -
    TRACKS(5 1)) -
"""
        if vsam_type.upper() == "KSDS":
            jcl_content += f"  INDEX (NAME({vsam_ds_name}.INDEX) - TRACKS(1 1))\n"
        jcl_content += "/*\n"
        return jcl_content


    
    
    async def _generate_and_save_artifacts_for_single_input_source(self, input_source: InputSource) -> GeneratedArtifactsResponse:
        log.info(f"Starting artifact generation for InputSource ID: {input_source.input_source_id}")
        self._clear_existing_artifacts_for_input_source(input_source.input_source_id)

        cobol_output_schemas = []
        jcl_output_schemas = []
        unit_test_output_schemas = []
        
        db_cobol_artifacts_to_add = []
        db_jcl_artifacts_to_add = []
        db_unit_test_artifacts_to_add = []

        log.debug(f"Fetching detailed InputSource data for ID: {input_source.input_source_id}")
        current_input_source_with_details = self.db.query(InputSource).filter(InputSource.input_source_id == input_source.input_source_id).options(
            selectinload(InputSource.procedures_defined).selectinload(Procedure.variables_in_procedure),
            selectinload(InputSource.m204_files_defined)
        ).one_or_none()

        if not current_input_source_with_details:
            log.error(f"InputSource ID {input_source.input_source_id} not found during artifact generation. Cannot proceed.")
            return GeneratedArtifactsResponse(
                input_source_id=input_source.input_source_id,
                cobol_files=[],
                jcl_files=[],
                unit_test_files=[]
            )
        log.info(f"Successfully fetched InputSource: {current_input_source_with_details.original_filename or f'ID_{current_input_source_with_details.input_source_id}'}")

        input_source_name_for_comments = current_input_source_with_details.original_filename or f"InputSourceID_{current_input_source_with_details.input_source_id}"
        cobol_program_id_base = self._sanitize_filename_base(current_input_source_with_details.original_filename, default_prefix="M204PROG")
        
        log.info(f"Generating artifacts for InputSource: '{input_source_name_for_comments}' (ID: {current_input_source_with_details.input_source_id}), COBOL Program ID base: '{cobol_program_id_base}'")

        related_procedures = current_input_source_with_details.procedures_defined or []
        m204_files_in_this_source = current_input_source_with_details.m204_files_defined or []
        
        log.info(f"Found {len(related_procedures)} procedures and {len(m204_files_in_this_source)} M204 files for this input source.")

        if m204_files_in_this_source:
            log.info(f"M204 Files associated with InputSource ID {current_input_source_with_details.input_source_id} ('{input_source_name_for_comments}'):")
            for m204_file_obj_log in m204_files_in_this_source:
                log.info(
                    f"  - M204File ID: {m204_file_obj_log.m204_file_id}, "
                    f"Name: '{m204_file_obj_log.m204_file_name}', "
                    f"Logical Name: '{m204_file_obj_log.m204_logical_dataset_name}', "
                    f"Is DB File: {m204_file_obj_log.is_db_file}, "
                    f"Target VSAM DSN: '{m204_file_obj_log.target_vsam_dataset_name}', "
                    f"Target VSAM Type: '{m204_file_obj_log.target_vsam_type}', "
                    f"Primary Key Field: '{m204_file_obj_log.primary_key_field_name}', "
                    f"File Definition JSON Keys: {list(m204_file_obj_log.file_definition_json.keys()) if m204_file_obj_log.file_definition_json else 'None'}"
                )
        else:
            log.info(f"No M204 Files associated with InputSource ID {current_input_source_with_details.input_source_id} ('{input_source_name_for_comments}').")
        
        cobol_file_name = f"{cobol_program_id_base}.cbl"
        log.info(f"Target COBOL file name: {cobol_file_name}")
        
        file_control_entries_str = ""
        file_section_fds_str = ""
        working_storage_for_fds_str = ""

        log.info(f"Starting FD generation for {len(m204_files_in_this_source)} M204 files.")
        if llm_config._llm and m204_files_in_this_source:
            fd_conversion_tasks = [self._llm_convert_file_definition_to_fd(m204_file) for m204_file in m204_files_in_this_source]
            log.debug(f"Created {len(fd_conversion_tasks)} tasks for FD conversion.")
            converted_fds_results = await asyncio.gather(*fd_conversion_tasks, return_exceptions=True)
            log.info(f"FD conversion tasks completed. Processing {len(converted_fds_results)} results.")
            for i, result in enumerate(converted_fds_results):
                m204_file_name_for_log = m204_files_in_this_source[i].m204_file_name or f"FileID_{m204_files_in_this_source[i].m204_file_id}"
                if isinstance(result, FileDefinitionToCobolFDOutput):
                    log.info(f"Successfully converted FD for M204 file: {m204_file_name_for_log}")
                    file_control_entries_str += result.file_control_entry
                    file_section_fds_str += result.file_description_entry + "\n"
                    if result.working_storage_entries:
                        working_storage_for_fds_str += result.working_storage_entries + "\n"
                    if result.comments:
                        file_section_fds_str += f"* FD Conversion Comment for {m204_file_name_for_log}: {result.comments}\n"
                elif isinstance(result, Exception):
                    log.error(f"Error converting file definition to FD for M204 file {m204_file_name_for_log}: {result}", exc_info=True)
                    file_section_fds_str += f"* --- ERROR DURING FILE DEFINITION TO FD CONVERSION FOR {m204_file_name_for_log} --- \n"
                else:
                    log.warning(f"Unexpected result type from FD conversion for M204 file {m204_file_name_for_log}: {type(result)}")
        elif not llm_config._llm:
            log.warning("LLM not configured. Skipping FD generation from M204 file definitions.")
            file_section_fds_str = "* LLM not configured for file definition to FD conversion.\n"
        else: 
            log.info("No M204 files to process for FD generation.")
            file_section_fds_str = "* No M204 files found for FD generation.\n"
        log.info("FD generation process finished.")

        procedure_division_main_logic = ""
        procedure_division_paragraphs = ""
        cobol_conversion_comments = []

        log.info(f"Starting M204 procedure to COBOL conversion for {len(related_procedures)} procedures.")
        if llm_config._llm and related_procedures:
            proc_conversion_tasks = [self._llm_convert_m204_proc_to_cobol(proc) for proc in related_procedures]
            log.debug(f"Created {len(proc_conversion_tasks)} tasks for procedure conversion.")
            converted_procs_results = await asyncio.gather(*proc_conversion_tasks, return_exceptions=True)
            log.info(f"Procedure conversion tasks completed. Processing {len(converted_procs_results)} results.")
            for i, result in enumerate(converted_procs_results):
                proc_name_for_log = related_procedures[i].m204_proc_name
                if isinstance(result, M204ProcedureToCobolOutput):
                    log.info(f"Successfully converted M204 procedure: {proc_name_for_log}")
                    para_base = re.sub(r'[^A-Z0-9-]', '', result.m204_procedure_name.upper().replace('%', 'P').replace('$', 'D').replace('_', '-').replace('#','N'))
                    paragraph_name = (para_base[:28] + "-PARA")
                    if not paragraph_name or not (paragraph_name[0].isalpha() or paragraph_name[0].isdigit()):
                        paragraph_name = "P" + (paragraph_name[1:] if paragraph_name else "") 
                    procedure_division_main_logic += f"           PERFORM {paragraph_name}.\n"
                    procedure_division_paragraphs += f"{paragraph_name}.\n{result.cobol_code_block}\n\n" 
                    if result.comments:
                        cobol_conversion_comments.append(f"* Procedure {result.m204_procedure_name}: {result.comments}")
                elif isinstance(result, Exception):
                    log.error(f"Error converting M204 procedure {proc_name_for_log} to COBOL: {result}", exc_info=True)
                    procedure_division_paragraphs += f"* --- ERROR DURING PROCEDURE CONVERSION FOR {proc_name_for_log} --- \n"
                else:
                    log.warning(f"Unexpected result type from procedure conversion for {proc_name_for_log}: {type(result)}")

        elif not llm_config._llm:
            log.warning("LLM not configured. Skipping M204 procedure to COBOL conversion. Generating placeholders.")
            procedure_division_paragraphs = "      * LLM not configured for M204 Procedure to COBOL conversion.\n"
            if related_procedures:
                for proc in related_procedures:
                     proc_name_sanitized = re.sub(r'[^A-Z0-9-]', '', proc.m204_proc_name.upper().replace('%', 'P').replace('$', 'D').replace('_', '-').replace('#','N'))[:28]
                     if not proc_name_sanitized or not (proc_name_sanitized[0].isalpha() or proc_name_sanitized[0].isdigit()):
                         proc_name_sanitized = "MOCK-P" + (proc_name_sanitized[1:] if proc_name_sanitized else "")
                     proc_name_sanitized += "-PARA"
                     procedure_division_main_logic += f"           PERFORM {proc_name_sanitized}.\n"
                     procedure_division_paragraphs += f"{proc_name_sanitized}.\n           DISPLAY 'MOCK EXECUTION OF {proc.m204_proc_name}'.\n\n"
        else: 
            log.info("No M204 procedures to convert to COBOL.")
            procedure_division_paragraphs = "   * No M204 procedures found for COBOL conversion.\n"
        log.info("M204 procedure to COBOL conversion process finished.")

        cobol_content = f"""\
IDENTIFICATION DIVISION.
PROGRAM-ID. {cobol_program_id_base}.
AUTHOR. ArtifactGenerator.
DATE-WRITTEN. {datetime.date.today().strftime("%Y-%m-%d")}.
* COBOL program generated for M204 Input Source: {input_source_name_for_comments} (ID: {current_input_source_with_details.input_source_id})
* Covers M204 Files: {', '.join([mf.m204_file_name or f'FileID_{mf.m204_file_id}' for mf in m204_files_in_this_source]) if m204_files_in_this_source else 'None'}
{("*" + "\n* ".join(cobol_conversion_comments)) if cobol_conversion_comments else "* No specific conversion comments."}
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
{file_control_entries_str if file_control_entries_str.strip() else "      * No FILE-CONTROL entries generated."}
DATA DIVISION.
FILE SECTION.
{file_section_fds_str if file_section_fds_str.strip() else "   * No FDs generated."}
WORKING-STORAGE SECTION.
{working_storage_for_fds_str if working_storage_for_fds_str.strip() else "   * No specific W-S entries from FDs."}
PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
MAIN-PARAGRAPH.
{procedure_division_main_logic if procedure_division_main_logic.strip() else "           DISPLAY 'No M204 procedures mapped to this COBOL program.'."}
           STOP RUN.
{procedure_division_paragraphs if procedure_division_paragraphs.strip() else "   * No procedure paragraphs generated."}
"""
        log.info(f"Generated COBOL content for {cobol_file_name} (Length: {len(cobol_content)}).")
        cobol_output_schema = CobolOutputSchema(
            input_source_id=current_input_source_with_details.input_source_id, 
            file_name=cobol_file_name,
            content=cobol_content,
            artifact_type="cobol"
        )
        cobol_output_schemas.append(cobol_output_schema)
        db_cobol_artifacts_to_add.append(GeneratedCobolArtifact(**cobol_output_schema.model_dump()))
        log.debug(f"Added COBOL artifact {cobol_file_name} to be saved to DB.")

        log.info("Starting unit test plan generation.")
        unit_test_file_name = f"test_{cobol_program_id_base}.txt"
        unit_test_content_parts = [
            f"Unit Test Plan for COBOL Program: {cobol_file_name}\n",
            f"Generated from M204 Input Source: {input_source_name_for_comments} (ID: {current_input_source_with_details.input_source_id})\n",
            "Based on M204 Procedures:\n"
        ]
        if not related_procedures:
            log.info("No M204 procedures found for unit test case generation.")
            unit_test_content_parts.append("- No M204 procedures for test case generation.\n")
        
        for proc in related_procedures:
            unit_test_content_parts.append(f"\n--- Test Cases for M204 Procedure: {proc.m204_proc_name} ---\n")
            if proc.suggested_test_cases_json:
                log.debug(f"Processing suggested test cases for procedure: {proc.m204_proc_name}")
                try:
                    test_cases_data = json.loads(proc.suggested_test_cases_json) if isinstance(proc.suggested_test_cases_json, str) else proc.suggested_test_cases_json
                    
                    if not isinstance(test_cases_data, list): 
                        log.warning(f"suggested_test_cases_json for procedure {proc.m204_proc_name} is not a list: {type(test_cases_data)}. Skipping.")
                        unit_test_content_parts.append("  Invalid format for pre-defined test cases (expected a list).\n")
                        continue

                    for tc_data in test_cases_data: 
                        if not isinstance(tc_data, dict): 
                            log.warning(f"Test case item for procedure {proc.m204_proc_name} is not a dictionary: {type(tc_data)}. Skipping.")
                            unit_test_content_parts.append("  Skipping invalid test case item (not a dictionary).\n")
                            continue
                        try:
                            tc = TestCase(**tc_data) 
                            unit_test_content_parts.extend([
                                f"Test Case ID: {tc.test_case_id}\n  Description: {tc.description}\n",
                                *("  Preconditions:\n" + "".join([f"    - {pre}\n" for pre in tc.preconditions]) if tc.preconditions else ""),
                                "  Inputs:\n" + "".join([f"    - {k}: {v}\n" for k, v in tc.inputs.items()]),
                                "  Expected Outputs:\n" + "".join([f"    - {k}: {v}\n" for k, v in tc.expected_outputs.items()]),
                                f"  Expected Behavior: {tc.expected_behavior_description}\n\n"
                            ])
                        except Exception as e_tc_parse: 
                            log.error(f"Error parsing individual test case data for {proc.m204_proc_name}: {e_tc_parse}. Data: {tc_data}", exc_info=True)
                            unit_test_content_parts.append(f"  Error parsing a test case: {str(e_tc_parse)}\n")

                except json.JSONDecodeError as e_json:
                    log.error(f"JSONDecodeError parsing test cases for {proc.m204_proc_name}: {e_json}. JSON: {proc.suggested_test_cases_json}", exc_info=True)
                    unit_test_content_parts.append(f"  Error parsing test cases JSON: {str(e_json)}\n")
                except Exception as e_other_parse: 
                    log.error(f"General error processing test cases for {proc.m204_proc_name}: {e_other_parse}. JSON: {proc.suggested_test_cases_json}", exc_info=True)
                    unit_test_content_parts.append(f"  Error processing test cases: {str(e_other_parse)}\n")
            else:
                log.info(f"No pre-defined test cases for procedure: {proc.m204_proc_name}")
                unit_test_content_parts.append("  No pre-defined test cases.\n")
        
        final_unit_test_content = "".join(unit_test_content_parts)
        log.info(f"Generated Unit Test Plan for {unit_test_file_name} (Length: {len(final_unit_test_content)}).")
        unit_test_schema = UnitTestOutputSchema(
            input_source_id=current_input_source_with_details.input_source_id, 
            file_name=unit_test_file_name,
            content=final_unit_test_content,
            artifact_type="unit_test" 
        )
        unit_test_output_schemas.append(unit_test_schema)
        db_unit_test_artifacts_to_add.append(GeneratedUnitTestArtifact(**unit_test_schema.model_dump()))
        log.debug(f"Added Unit Test artifact {unit_test_file_name} to be saved to DB.")
        log.info("Unit test plan generation finished.")

        # --- Debugging: Log all InputSource and M204File records before JCL generation ---
        log.info("--- Pre-JCL Generation DB State ---")
        try:
            all_input_sources_from_db = self.db.query(InputSource).all()
            log.info(f"Found {len(all_input_sources_from_db)} InputSource records in DB:")
            for idx, src in enumerate(all_input_sources_from_db):
                log.info(f"  InputSource {idx+1}/{len(all_input_sources_from_db)}: ID={src.input_source_id}, Name='{src.original_filename}', ProjectID={src.project_id}, Type='{src.source_type}'")
        except Exception as e_is_query:
            log.error(f"Error querying all InputSource records: {e_is_query}", exc_info=True)

        try:
            all_m204_files_from_db = self.db.query(M204File).all()
            log.info(f"Found {len(all_m204_files_from_db)} M204File records in DB:")
            for idx, mfile in enumerate(all_m204_files_from_db):
                log.info(
                    f"  M204File {idx+1}/{len(all_m204_files_from_db)}: ID={mfile.m204_file_id}, Name='{mfile.m204_file_name}', "
                    f"InputSourceID={mfile.input_source_id}, IsDBFile={mfile.is_db_file}, "
                    f"TargetVSAM_DSN='{mfile.target_vsam_dataset_name}', TargetVSAM_Type='{mfile.target_vsam_type}'"
                )
        except Exception as e_mf_query:
            log.error(f"Error querying all M204File records: {e_mf_query}", exc_info=True)
        log.info("--- End of Pre-JCL Generation DB State ---")
        # --- End Debugging ---

        log.info("Starting general JCL (run JCL) generation.")
        general_jcl_file_name = f"{cobol_program_id_base}_run.jcl"
        dd_statements_for_jcl = []
        if m204_files_in_this_source:
            log.debug(f"Processing {len(m204_files_in_this_source)} M204 files for DD statements in run JCL.")
            for m204_file_obj in m204_files_in_this_source:
                raw_dd_name = m204_file_obj.m204_file_name
                if not raw_dd_name:
                    log.warning(f"M204File ID {m204_file_obj.m204_file_id} has no m204_file_name; skipping DD card for run JCL.")
                    continue
                
                dd_name_candidate = re.sub(r'[^A-Z0-9]', '', raw_dd_name.upper())
                if not dd_name_candidate: 
                    dd_name_candidate = f"MFILE{m204_file_obj.m204_file_id}" 
                if not dd_name_candidate[0].isalpha():
                    dd_name_candidate = "F" + dd_name_candidate 
                
                dd_name_final = dd_name_candidate[:8] 
                
                if not dd_name_final: 
                    log.warning(f"Could not derive valid DDNAME for M204File ID {m204_file_obj.m204_file_id} (raw: {raw_dd_name}) for run JCL. Using placeholder.")
                    dd_name_final = f"DD{m204_file_obj.m204_file_id:06}"

                dsn = m204_file_obj.target_vsam_dataset_name or m204_file_obj.m204_logical_dataset_name or f"YOUR.DATASET.FOR.{dd_name_final}"
                if not re.match(r"^[A-Z@#$][A-Z0-9@#$]{0,7}(\.[A-Z@#$][A-Z0-9@#$]{0,7})+$", dsn.upper()):
                    dsn_sanitized = re.sub(r'[^A-Z0-9.]', '', dsn.upper())
                    dsn_parts = [part for part in dsn_sanitized.split('.') if part]
                    if not dsn_parts or not (dsn_parts[0][0].isalpha() if dsn_parts[0] else False):
                        dsn = f"USER.M204.{dd_name_final}.DATA" 
                    else:
                        dsn = ".".join(dsn_parts)
                log.debug(f"  Generated DD card for run JCL: DDNAME={dd_name_final}, DSN={dsn}")
                dd_statements_for_jcl.append(f"//{dd_name_final:<8} DD DSN={dsn},DISP=SHR")
        
        dd_statements_str = "\n".join(dd_statements_for_jcl) if dd_statements_for_jcl else "//* No M204 files with DDNAMEs found in this source for run JCL."
        
        general_jcl_content = f"""\
//JOBGENER JOB (ACCT),'RUN {cobol_program_id_base}',CLASS=A,MSGCLASS=X
//* JCL to run COBOL program: {cobol_file_name}
//* From M204 Input Source: {input_source_name_for_comments} (ID: {current_input_source_with_details.input_source_id})
//STEP010  EXEC PGM={cobol_program_id_base}
//STEPLIB  DD DSN=YOUR.COBOL.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
{dd_statements_str}
//SYSIN    DD *
/*
"""
        log.info(f"Generated general JCL for {general_jcl_file_name} (Length: {len(general_jcl_content)}).")
        jcl_general_schema = JclOutputSchema(
            input_source_id=current_input_source_with_details.input_source_id, 
            file_name=general_jcl_file_name, content=general_jcl_content,
            jcl_purpose="general", artifact_type="jcl_general"
        )
        jcl_output_schemas.append(jcl_general_schema)
        db_jcl_artifacts_to_add.append(GeneratedJclArtifact(**jcl_general_schema.model_dump()))
        log.debug(f"Added general JCL artifact {general_jcl_file_name} to be saved to DB.")
        log.info("General JCL (run JCL) generation finished.")

        log.info("Starting VSAM definition JCL generation.")
        vsam_jcls_generated_count = 0
        for m204_file_obj in m204_files_in_this_source:
            log.debug(f"Checking M204File ID {m204_file_obj.m204_file_id} ('{m204_file_obj.m204_file_name}') for VSAM JCL generation. Is DB File: {m204_file_obj.is_db_file}")
            if m204_file_obj.is_db_file:
                vsam_jcls_generated_count += 1
                m204_name_part_raw = m204_file_obj.m204_file_name or f"DBF{m204_file_obj.m204_file_id}"
                m204_name_part = re.sub(r'[^A-Z0-9]', '', m204_name_part_raw.upper())[:8]
                if not m204_name_part:
                    m204_name_part = f"DBF{m204_file_obj.m204_file_id}" 
                if not m204_name_part[0].isalpha():
                    m204_name_part = "V" + m204_name_part[:7]


                vsam_jcl_name = f"{cobol_program_id_base}_{m204_name_part}_vsam.jcl"
                vsam_ds_name = m204_file_obj.target_vsam_dataset_name or f"DEFAULT.VSAM.{cobol_program_id_base}.{m204_name_part}"
                if not re.match(r"^[A-Z@#$][A-Z0-9@#$]{0,7}(\.[A-Z@#$][A-Z0-9@#$]{0,7})+$", vsam_ds_name.upper()):
                    dsn_sanitized = re.sub(r'[^A-Z0-9.]', '', vsam_ds_name.upper())
                    dsn_parts = [part for part in dsn_sanitized.split('.') if part]
                    if not dsn_parts or not (dsn_parts[0][0].isalpha() if dsn_parts[0] else False):
                        vsam_ds_name = f"PROJ.VSAM.{cobol_program_id_base}.{m204_name_part}" 
                    else:
                        vsam_ds_name = ".".join(dsn_parts)

                vsam_type = m204_file_obj.target_vsam_type or "KSDS" 
                
                log.info(f"Preparing to generate VSAM JCL for DB file: '{m204_file_obj.m204_file_name}', Target DSN: '{vsam_ds_name}', Type: '{vsam_type}', JCL Name: '{vsam_jcl_name}'")
                
                vsam_jcl_content = ""
                llm_comments = None

                if llm_config._llm:
                    try:
                        log.info(f"Attempting LLM generation for VSAM JCL: {vsam_jcl_name} for M204 File: {m204_file_obj.m204_file_name}")
                        llm_vsam_jcl_result = await self._llm_generate_vsam_jcl(
                            m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments
                        )
                        vsam_jcl_content = llm_vsam_jcl_result.jcl_content
                        llm_comments = llm_vsam_jcl_result.generation_comments
                        if llm_comments:
                            vsam_jcl_content = f"//* LLM Generation Comments:\n//* {llm_comments.replace(chr(10), chr(10) + '//* ')}\n" + vsam_jcl_content
                        log.info(f"LLM successfully generated VSAM JCL for: {vsam_jcl_name}")
                    except Exception as e:
                        log.warning(f"LLM generation for VSAM JCL {vsam_jcl_name} (M204 File: {m204_file_obj.m204_file_name}) failed: {e}. Falling back to template.", exc_info=True)
                        vsam_jcl_content = self._generate_fallback_vsam_jcl(
                            m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments
                        )
                else:
                    log.info(f"LLM not configured. Generating fallback VSAM JCL for: {vsam_jcl_name} (M204 File: {m204_file_obj.m204_file_name})")
                    vsam_jcl_content = self._generate_fallback_vsam_jcl(
                        m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments
                    )
                
                log.info(f"Generated VSAM JCL for {vsam_jcl_name} (Length: {len(vsam_jcl_content)}).")
                jcl_vsam_schema = JclOutputSchema(
                    input_source_id=current_input_source_with_details.input_source_id, 
                    file_name=vsam_jcl_name, content=vsam_jcl_content,
                    jcl_purpose="vsam", artifact_type="jcl_vsam"
                )
                jcl_output_schemas.append(jcl_vsam_schema)
                db_jcl_artifacts_to_add.append(GeneratedJclArtifact(**jcl_vsam_schema.model_dump()))
                log.debug(f"Added VSAM JCL artifact {vsam_jcl_name} to be saved to DB.")
            else:
                log.debug(f"Skipping VSAM JCL generation for M204File ID {m204_file_obj.m204_file_id} ('{m204_file_obj.m204_file_name}') as it is not a DB file (is_db_file={m204_file_obj.is_db_file}).")
        
        if vsam_jcls_generated_count == 0:
            log.info("No M204 DB files found in this source, so no VSAM definition JCLs were generated.")
        log.info("VSAM definition JCL generation process finished.")
        
        log.info("Preparing to save generated artifacts to the database.")
        if db_cobol_artifacts_to_add:
            self.db.add_all(db_cobol_artifacts_to_add)
            log.debug(f"Added {len(db_cobol_artifacts_to_add)} COBOL artifacts to session.")
        if db_jcl_artifacts_to_add:
            self.db.add_all(db_jcl_artifacts_to_add)
            log.debug(f"Added {len(db_jcl_artifacts_to_add)} JCL artifacts to session.")
        if db_unit_test_artifacts_to_add:
            self.db.add_all(db_unit_test_artifacts_to_add)
            log.debug(f"Added {len(db_unit_test_artifacts_to_add)} Unit Test artifacts to session.")
        
        response = GeneratedArtifactsResponse(
            input_source_id=current_input_source_with_details.input_source_id, 
            cobol_files=cobol_output_schemas,
            jcl_files=jcl_output_schemas,
            unit_test_files=unit_test_output_schemas
        )
        log.info(f"Finished artifact generation for InputSource ID: {input_source.input_source_id}. Returning {len(response.cobol_files)} COBOL, {len(response.jcl_files)} JCL, {len(response.unit_test_files)} Unit Test files.")
        return response


    async def generate_artifacts_for_project(self, project_id: int) -> List[InputSourceArtifacts]:
        project = self.db.query(Project).filter(Project.project_id == project_id).first()
        if not project:
            log.warning(f"Project with id {project_id} not found for artifact generation.")
            raise HTTPException(status_code=404, detail=f"Project with id {project_id} not found")

        m204_input_sources = self.db.query(InputSource).filter(
            InputSource.project_id == project_id,
            InputSource.source_type == 'm204'
        ).options(
            joinedload(InputSource.generated_cobol_artifacts),
            joinedload(InputSource.generated_jcl_artifacts),
            joinedload(InputSource.generated_unit_test_artifacts)
        ).all()

        all_project_artifacts_content: List[InputSourceArtifacts] = []

        if not m204_input_sources:
            log.info(f"No M204 type InputSource files found for project id {project_id}. Returning empty list.")
            return []

        for input_source_obj in m204_input_sources:
            log.info(f"Processing artifacts for InputSource: {input_source_obj.original_filename or f'ID_{input_source_obj.input_source_id}'} (ID: {input_source_obj.input_source_id}) in project {project_id}")
            
            input_source_artifact_bundle = InputSourceArtifacts(
                input_source_id=input_source_obj.input_source_id,
                input_source_original_filename=input_source_obj.original_filename,
                generated_files=[] 
            )
            current_source_files: List[GeneratedFileContent] = []
            
            has_cobol = bool(input_source_obj.generated_cobol_artifacts)
            has_jcl = bool(input_source_obj.generated_jcl_artifacts)
            has_unit_tests = bool(input_source_obj.generated_unit_test_artifacts)
            
            use_cached_artifacts = has_cobol and has_jcl and has_unit_tests

            if use_cached_artifacts:
                log.info(f"Using cached artifacts from DB for InputSource ID: {input_source_obj.input_source_id}")
                for cobol_artifact_orm in input_source_obj.generated_cobol_artifacts:
                    current_source_files.append(GeneratedFileContent(
                        file_name=cobol_artifact_orm.file_name,
                        content=cobol_artifact_orm.content,
                        artifact_type=cobol_artifact_orm.artifact_type
                    ))
                for jcl_artifact_orm in input_source_obj.generated_jcl_artifacts:
                    current_source_files.append(GeneratedFileContent(
                        file_name=jcl_artifact_orm.file_name,
                        content=jcl_artifact_orm.content,
                        artifact_type=jcl_artifact_orm.artifact_type
                    ))
                for unit_test_artifact_orm in input_source_obj.generated_unit_test_artifacts:
                    current_source_files.append(GeneratedFileContent(
                        file_name=unit_test_artifact_orm.file_name,
                        content=unit_test_artifact_orm.content,
                        artifact_type=unit_test_artifact_orm.artifact_type
                    ))
            else:
                log.info(f"No complete cached artifacts or regeneration policy for InputSource ID: {input_source_obj.input_source_id}. Generating anew.")
                try:
                    single_source_artifacts_response: GeneratedArtifactsResponse = \
                        await self._generate_and_save_artifacts_for_single_input_source(input_source_obj)
                    
                    for cobol_schema in single_source_artifacts_response.cobol_files:
                        current_source_files.append(GeneratedFileContent(
                            file_name=cobol_schema.file_name, content=cobol_schema.content, artifact_type=cobol_schema.artifact_type
                        ))
                    for jcl_schema in single_source_artifacts_response.jcl_files:
                        current_source_files.append(GeneratedFileContent(
                            file_name=jcl_schema.file_name, content=jcl_schema.content, artifact_type=jcl_schema.artifact_type
                        ))
                    for unit_test_schema in single_source_artifacts_response.unit_test_files:
                        current_source_files.append(GeneratedFileContent(
                            file_name=unit_test_schema.file_name, content=unit_test_schema.content, artifact_type=unit_test_schema.artifact_type
                        ))
                    
                    self.db.commit()
                    log.info(f"Successfully generated and saved artifacts for InputSource ID: {input_source_obj.input_source_id}")

                except Exception as e:
                    self.db.rollback()
                    log.error(f"Error generating artifacts for InputSource {input_source_obj.input_source_id} ('{input_source_obj.original_filename}'): {e}", exc_info=True)
                    error_file_content = GeneratedFileContent(
                        file_name=f"ERROR_InputSource_{input_source_obj.input_source_id}_{self._sanitize_filename_base(input_source_obj.original_filename or '', 'ERR')}.txt",
                        content=f"Failed to generate artifacts for InputSource ID {input_source_obj.input_source_id} ('{input_source_obj.original_filename}').\nError: {str(e)}",
                        artifact_type="error"
                    )
                    current_source_files.append(error_file_content)
            
            input_source_artifact_bundle.generated_files = current_source_files
            all_project_artifacts_content.append(input_source_artifact_bundle)
        
        log.info(f"Successfully prepared artifact contents for project {project_id}.")
        return all_project_artifacts_content