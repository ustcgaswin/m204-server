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
from pydantic import BaseModel, Field as PydanticField 

LLM_API_CALL_BATCH_SIZE = 20


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

class MainLoopToCobolOutput(BaseModel):
    """Structured output for M204 main processing loop to COBOL conversion."""
    cobol_code_block: str = PydanticField(description="Generated COBOL code block for the main processing loop, intended to be placed in its own paragraph.")
    comments: Optional[str] = PydanticField(description="Conversion comments regarding the main loop.", default=None)

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
- Convert any M204 record loops (such as `FIND ALL RECORDS ... END FIND` or `FOR EACH VALUE ... END FOR`) into COBOL inline loops (e.g., `PERFORM UNTIL ...` or `PERFORM VARYING ...`) with the loop body implemented directly inside the generated code block. Do NOT generate a `PERFORM` that calls another paragraph for the loop body; implement the loop logic inline.
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

    async def _llm_convert_main_loop_to_cobol(self, main_loop_content: str, m204_file_name: str) -> MainLoopToCobolOutput:
        """Converts an M204 main processing loop to a COBOL paragraph using an LLM."""
        if not llm_config._llm:
            log.warning(f"LLM not available for main loop conversion from file {m204_file_name}. Returning placeholder.")
            return MainLoopToCobolOutput(
                cobol_code_block="      * --- Placeholder for Main Processing Loop ---\n"
                                 "      DISPLAY 'Main processing loop logic to be implemented.'.\n"
                                 "      * --- End of Placeholder ---",
                comments="LLM not available. Manual conversion of main loop required."
            )

        prompt_fstr = f"""
You are an expert M204 to COBOL migration specialist.
Convert the following M204 main processing loop, extracted from the M204 file '{m204_file_name}', into a COBOL code block. This block will be placed into its own paragraph named `MAIN-PROCESSING-LOOP-PARA`.

M204 Main Loop Content:
```m204
{main_loop_content}
```

Your task is to generate *only* the COBOL statements that represent the logic of this loop.
- Convert M204 `FOR EACH VALUE...` or `FIND` loops into COBOL inline loops (e.g., `PERFORM UNTIL ...` or `PERFORM VARYING ...`) with the loop body implemented directly inside the generated code block. Do NOT generate a `PERFORM` that calls another paragraph for the loop body; implement the loop logic inline.
- Convert M204 `CALL` statements to COBOL `PERFORM <procedure-name>-PARA` statements.
- Translate `IF/ELSE`, `PRINT`, and other logic into their COBOL equivalents.
- The generated `cobol_code_block` MUST NOT include the paragraph name itself (e.g., `MAIN-PROCESSING-LOOP-PARA.`).
- The code should be correctly indented to be placed inside a paragraph (starting in Area B, column 12).
- Assume all required files are OPEN and will be CLOSED elsewhere. Assume variables are defined in WORKING-STORAGE.

Respond with a JSON object structured according to the MainLoopToCobolOutput model:
```json
{{
  "cobol_code_block": "string",
  "comments": "string (optional)"
}}
```
Example of a valid `cobol_code_block` for a loop:
```cobol
      PERFORM UNTIL END-OF-FILE = 'Y'
          READ INPUT-FILE
              AT END
                  MOVE 'Y' TO END-OF-FILE
              NOT AT END
                  PERFORM PROCESS-RECORD-PARA
          END-READ
      END-PERFORM.
```
"""
        json_text_output: Optional[str] = None
        try:
            async with self.llm_semaphore:
                log.debug(f"Attempting LLM call for M204 main loop to COBOL from file: {m204_file_name} (semaphore acquired)")
                llm_call = llm_config._llm.as_structured_llm(MainLoopToCobolOutput)
                response = await llm_call.acomplete(prompt=prompt_fstr)
                json_text_output = response.text
            log.debug(f"LLM call for M204 main loop to COBOL from file: {m204_file_name} completed (semaphore released)")
            return MainLoopToCobolOutput(**json.loads(json_text_output))
        except Exception as e:
            log.error(f"LLM error converting main loop from file {m204_file_name} to COBOL: {e}. Raw output: {json_text_output}", exc_info=True)
            return MainLoopToCobolOutput(
                cobol_code_block="      * --- Error during COBOL conversion for Main Processing Loop ---\n"
                                 "      DISPLAY 'Error in main loop logic.'.\n"
                                 "      * --- See logs for details ---",
                comments=f"LLM conversion failed for main loop: {str(e)}"
            )

    async def _llm_convert_file_definition_to_fd(self, m204_file: M204File) -> Optional[FileDefinitionToCobolFDOutput]:
        """Convert M204 file definition JSON to COBOL FD using LLM. Returns None if no definition exists."""
        if not m204_file.file_definition_json:
            log.info(f"Skipping FD generation for M204 file '{m204_file.m204_file_name}' as it has no file_definition_json.")
            return None

        if not llm_config._llm:
            log.warning(f"LLM not available for M204 file: {m204_file.m204_file_name}. Returning placeholder FD because a file definition exists.")
            select_name = (m204_file.m204_logical_dataset_name or m204_file.m204_file_name or f"FILE{m204_file.m204_file_id}").replace("-", "")[:8]
            return FileDefinitionToCobolFDOutput(
                logical_file_name=select_name,
                file_control_entry=f"       SELECT {select_name}-FILE ASSIGN TO {select_name}.\n",
                file_description_entry=f"   FD  {select_name}-FILE.\n"
                                     f"   01  {select_name}-RECORD PIC X(80). *> Placeholder\n",
                comments="LLM not available. Manual FD creation required."
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


    
    
    async def _generate_and_save_artifacts_for_single_input_source(
        self, 
        input_source: InputSource, 
        generated_vsam_jcl_for_m204file_ids_in_project_run: set[int]
    ) -> GeneratedArtifactsResponse:
        log.info(f"Starting artifact generation for InputSource ID: {input_source.input_source_id}, Type: {input_source.source_type}")
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
            selectinload(InputSource.m204_files_defined).selectinload(M204File.defined_in_source)
        ).one_or_none()

        if not current_input_source_with_details:
            log.error(f"InputSource ID {input_source.input_source_id} not found during artifact generation. Cannot proceed.")
            # Return an empty response or raise an error, consistent with how generate_artifacts_for_project handles it
            return GeneratedArtifactsResponse(
                input_source_id=input_source.input_source_id,
                cobol_files=[],
                jcl_files=[],
                unit_test_files=[]
            )
        log.info(f"Successfully fetched InputSource: {current_input_source_with_details.original_filename or f'ID_{current_input_source_with_details.input_source_id}'} (Type: {current_input_source_with_details.source_type})")

        input_source_name_for_comments = current_input_source_with_details.original_filename or f"InputSourceID_{current_input_source_with_details.input_source_id}"
        
        # Adjust default prefix for cobol_program_id_base based on source type
        prefix_map = {'m204': 'M204PROG', 'parmlib': 'PARMLIB', 'jcl': 'JCL'}
        default_program_prefix = prefix_map.get(current_input_source_with_details.source_type, "UNKNOWN")
        cobol_program_id_base = self._sanitize_filename_base(current_input_source_with_details.original_filename, default_prefix=default_program_prefix)
        
        log.info(f"Generating artifacts for InputSource: '{input_source_name_for_comments}' (ID: {current_input_source_with_details.input_source_id}), Program ID base: '{cobol_program_id_base}'")

        related_procedures = current_input_source_with_details.procedures_defined or []
        m204_files_in_this_source = current_input_source_with_details.m204_files_defined or []
        
        log.info(f"Found {len(related_procedures)} procedures and {len(m204_files_in_this_source)} M204 files for this input source ('{input_source_name_for_comments}').")

        if m204_files_in_this_source:
            log.info(f"M204 Files defined in InputSource ID {current_input_source_with_details.input_source_id} ('{input_source_name_for_comments}'):")
            for m204_file_obj_log in m204_files_in_this_source:
                log.info(
                    f"  - M204File ID: {m204_file_obj_log.m204_file_id}, "
                    f"Name: '{m204_file_obj_log.m204_file_name}', "
                    f"Is DB File: {m204_file_obj_log.is_db_file}, "
                    f"Target VSAM DSN: '{m204_file_obj_log.target_vsam_dataset_name}'"
                )
        
        # --- COBOL, Unit Test, and Run JCL Generation (Only for 'm204' source type) ---
        if current_input_source_with_details.source_type == 'm204':
            cobol_file_name = f"{cobol_program_id_base}.cbl"
            log.info(f"Target COBOL file name for M204 source: {cobol_file_name}")
            
            file_control_entries_str = ""
            file_section_fds_str = ""
            working_storage_for_fds_str = ""

            log.info(f"Starting FD generation for {len(m204_files_in_this_source)} M204 files (M204 source).")
            if llm_config._llm and m204_files_in_this_source:
                fd_conversion_tasks = [self._llm_convert_file_definition_to_fd(m204_file) for m204_file in m204_files_in_this_source]
                converted_fds_results = await asyncio.gather(*fd_conversion_tasks, return_exceptions=True)
                for i, result in enumerate(converted_fds_results):
                    m204_file_name_for_log = m204_files_in_this_source[i].m204_file_name or f"FileID_{m204_files_in_this_source[i].m204_file_id}"
                    if isinstance(result, FileDefinitionToCobolFDOutput):
                        file_control_entries_str += result.file_control_entry
                        file_section_fds_str += result.file_description_entry + "\n"
                        if result.working_storage_entries:
                            working_storage_for_fds_str += result.working_storage_entries + "\n"
                        if result.comments:
                            file_section_fds_str += f"* FD Comment ({m204_file_name_for_log}): {result.comments}\n"
                    elif isinstance(result, Exception):
                        log.error(f"Error converting FD for {m204_file_name_for_log}: {result}", exc_info=True)
                        file_section_fds_str += f"* --- ERROR CONVERTING FD FOR {m204_file_name_for_log} ---\n"
            elif not llm_config._llm:
                file_section_fds_str = "* LLM not configured for FD conversion.\n"
            else:
                file_section_fds_str = "* No M204 files for FD generation.\n"
            log.info("FD generation process finished (M204 source).")

            main_loop_cobol_block = ""
            main_loop_paragraph = ""
            cobol_conversion_comments = []

            # --- Main Processing Loop Conversion ---
            if current_input_source_with_details.main_processing_loop_content and llm_config._llm:
                log.info(f"Found main processing loop in InputSource '{input_source_name_for_comments}'. Converting to COBOL.")
                main_loop_result = await self._llm_convert_main_loop_to_cobol(
                    current_input_source_with_details.main_processing_loop_content,
                    input_source_name_for_comments
                )
                main_loop_cobol_block = main_loop_result.cobol_code_block
                if main_loop_result.comments:
                    cobol_conversion_comments.append(f"Main Loop: {main_loop_result.comments}")
                main_loop_paragraph = f"MAIN-PROCESSING-LOOP-PARA.\n{main_loop_cobol_block}\n\n"
            elif current_input_source_with_details.main_processing_loop_content:
                log.warning("Main processing loop found, but LLM is not configured. Skipping conversion.")
                main_loop_paragraph = "MAIN-PROCESSING-LOOP-PARA.\n      * Main processing loop found but LLM not configured for conversion.\n\n"

            procedure_division_main_logic = ""
            procedure_division_paragraphs = ""

            log.info(f"Starting M204 procedure to COBOL conversion for {len(related_procedures)} procedures (M204 source).")
            if llm_config._llm and related_procedures:
                proc_conversion_tasks = [self._llm_convert_m204_proc_to_cobol(proc) for proc in related_procedures]
                converted_procs_results = await asyncio.gather(*proc_conversion_tasks, return_exceptions=True)
                for i, result in enumerate(converted_procs_results):
                    proc_name_for_log = related_procedures[i].m204_proc_name
                    if isinstance(result, M204ProcedureToCobolOutput):
                        para_base = re.sub(r'[^A-Z0-9-]', '', result.m204_procedure_name.upper().replace('%', 'P').replace('$', 'D').replace('_', '-').replace('#','N'))
                        paragraph_name = (para_base[:28] + "-PARA")
                        if not paragraph_name or not (paragraph_name[0].isalpha() or paragraph_name[0].isdigit()):
                            paragraph_name = "P" + (paragraph_name[1:] if paragraph_name else "") 
                        # If there is no main loop, perform procedures sequentially. Otherwise, assume main loop calls them.
                        if not main_loop_cobol_block:
                            procedure_division_main_logic += f"           PERFORM {paragraph_name}.\n"
                        procedure_division_paragraphs += f"{paragraph_name}.\n{result.cobol_code_block}\n\n" 
                        if result.comments:
                            cobol_conversion_comments.append(f"Proc {result.m204_procedure_name}: {result.comments}")
                    elif isinstance(result, Exception):
                        log.error(f"Error converting procedure {proc_name_for_log}: {result}", exc_info=True)
                        procedure_division_paragraphs += f"* --- ERROR CONVERTING PROCEDURE {proc_name_for_log} ---\n"
            elif not llm_config._llm:
                procedure_division_paragraphs = "      * LLM not configured for M204 Procedure to COBOL conversion.\n"
            else:
                procedure_division_paragraphs = "   * No M204 procedures for COBOL conversion.\n"
            log.info("M204 procedure to COBOL conversion finished (M204 source).")

            # Determine the main logic flow
            if main_loop_cobol_block:
                procedure_division_main_logic = "           PERFORM MAIN-PROCESSING-LOOP-PARA.\n"
            elif not procedure_division_main_logic: # No main loop and no procedures
                procedure_division_main_logic = "           DISPLAY 'No M204 procedures or main loop mapped.'."

            cobol_content = f"""\
IDENTIFICATION DIVISION.
PROGRAM-ID. {cobol_program_id_base}.
AUTHOR. ArtifactGenerator.
DATE-WRITTEN. {datetime.date.today().strftime("%Y-%m-%d")}.
*
* COBOL program for M204 Input Source: {input_source_name_for_comments}
*
{("* " + "\n*\n* ".join(cobol_conversion_comments)) if cobol_conversion_comments else "* No specific conversion comments."}
*
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
{file_control_entries_str if file_control_entries_str.strip() else "      * No FILE-CONTROL entries."}
DATA DIVISION.
FILE SECTION.
{file_section_fds_str if file_section_fds_str.strip() else "   * No FDs."}
WORKING-STORAGE SECTION.
{working_storage_for_fds_str if working_storage_for_fds_str.strip() else "   * No specific W-S from FDs."}
PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
MAIN-PARAGRAPH.
{procedure_division_main_logic}
           STOP RUN.
{main_loop_paragraph if main_loop_paragraph.strip() else ""}
{procedure_division_paragraphs if procedure_division_paragraphs.strip() else "   * No procedure paragraphs."}
"""
            cobol_output_schema = CobolOutputSchema(input_source_id=current_input_source_with_details.input_source_id, file_name=cobol_file_name, content=cobol_content, artifact_type="cobol")
            cobol_output_schemas.append(cobol_output_schema)
            db_cobol_artifacts_to_add.append(GeneratedCobolArtifact(**cobol_output_schema.model_dump()))
            log.info(f"Generated COBOL file {cobol_file_name} for M204 source.")

            log.info("Starting unit test plan generation (M204 source).")
            unit_test_file_name = f"test_{cobol_program_id_base}.txt"
            unit_test_content_parts = [f"Unit Test Plan for COBOL: {cobol_file_name}\nFrom M204 Source: {input_source_name_for_comments}\n"]
            if not related_procedures:
                unit_test_content_parts.append("- No M204 procedures for test cases.\n")
            for proc in related_procedures:
                unit_test_content_parts.append(f"\n--- Test Cases for M204 Procedure: {proc.m204_proc_name} ---\n")
                if proc.suggested_test_cases_json:
                    try:
                        test_cases_data = json.loads(proc.suggested_test_cases_json) if isinstance(proc.suggested_test_cases_json, str) else proc.suggested_test_cases_json
                        if isinstance(test_cases_data, list):
                            for tc_data in test_cases_data:
                                if isinstance(tc_data, dict):
                                    try:
                                        tc = TestCase(**tc_data)
                                        unit_test_content_parts.extend([f"ID: {tc.test_case_id}\n  Desc: {tc.description}\n", ("  Inputs: " + str(tc.inputs) + "\n"), ("  Expected: " + str(tc.expected_outputs) + "\n")])
                                    except Exception as e_tc:
                                        unit_test_content_parts.append(f"  Error parsing TC: {e_tc}\n")
                                else:
                                    unit_test_content_parts.append("  Invalid TC item (not dict).\n")
                        else:
                            unit_test_content_parts.append("  Invalid test cases format (not list).\n")
                    except Exception as e_json:
                        unit_test_content_parts.append(f"  Error parsing test cases JSON: {e_json}\n")
                else:
                    unit_test_content_parts.append("  No pre-defined test cases.\n")
            final_unit_test_content = "".join(unit_test_content_parts)
            unit_test_schema = UnitTestOutputSchema(input_source_id=current_input_source_with_details.input_source_id, file_name=unit_test_file_name, content=final_unit_test_content, artifact_type="unit_test")
            unit_test_output_schemas.append(unit_test_schema)
            db_unit_test_artifacts_to_add.append(GeneratedUnitTestArtifact(**unit_test_schema.model_dump()))
            log.info(f"Generated Unit Test file {unit_test_file_name} for M204 source.")

            log.info("Starting general JCL (run JCL) generation (M204 source).")
            general_jcl_file_name = f"{cobol_program_id_base}_run.jcl"
            dd_statements_for_jcl = []
            if m204_files_in_this_source:
                for m204_file_obj in m204_files_in_this_source:
                    raw_dd_name = m204_file_obj.m204_file_name
                    if not raw_dd_name:
                        continue
                    dd_name_candidate = re.sub(r'[^A-Z0-9]', '', raw_dd_name.upper())
                    if not dd_name_candidate:
                        dd_name_candidate = f"MFILE{m204_file_obj.m204_file_id}"
                    if not dd_name_candidate[0].isalpha():
                        dd_name_candidate = "F" + dd_name_candidate
                    dd_name_final = dd_name_candidate[:8]
                    if not dd_name_final:
                        dd_name_final = f"DD{m204_file_obj.m204_file_id:06}"
                    dsn = m204_file_obj.target_vsam_dataset_name or m204_file_obj.m204_logical_dataset_name or f"YOUR.DSN.FOR.{dd_name_final}"
                    # Basic DSN sanitization, can be improved
                    if not re.match(r"^[A-Z@#$][A-Z0-9@#$]{0,7}(\.[A-Z@#$][A-Z0-9@#$]{0,7})*$", dsn.upper()):
                        dsn = f"USER.M204.{dd_name_final}.DATA" # Fallback DSN
                    dd_statements_for_jcl.append(f"//{dd_name_final:<8} DD DSN={dsn},DISP=SHR")
            dd_statements_str = "\n".join(dd_statements_for_jcl) if dd_statements_for_jcl else "//* No M204 files for DD statements."
            general_jcl_content = f"""\
//JOBGENER JOB (ACCT),'RUN {cobol_program_id_base}',CLASS=A,MSGCLASS=X
//* Run JCL for COBOL: {cobol_file_name} (From M204 Source: {input_source_name_for_comments})
//STEP010  EXEC PGM={cobol_program_id_base}
//STEPLIB  DD DSN=YOUR.COBOL.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
{dd_statements_str}
//SYSIN    DD *
/*
"""
            jcl_general_schema = JclOutputSchema(input_source_id=current_input_source_with_details.input_source_id, file_name=general_jcl_file_name, content=general_jcl_content, jcl_purpose="general", artifact_type="jcl_general")
            jcl_output_schemas.append(jcl_general_schema)
            db_jcl_artifacts_to_add.append(GeneratedJclArtifact(**jcl_general_schema.model_dump()))
            log.info(f"Generated Run JCL {general_jcl_file_name} for M204 source.")

        elif current_input_source_with_details.source_type in ('parmlib', 'jcl'):
            log.info(f"InputSource type is '{current_input_source_with_details.source_type}' ({input_source_name_for_comments}). Skipping COBOL, Unit Test, and Run JCL generation.")
        else:
            log.warning(f"Unknown InputSource type: {current_input_source_with_details.source_type} for {input_source_name_for_comments}. Limited artifact generation.")

        # --- VSAM Definition JCL Generation (for 'm204', 'parmlib', and 'jcl' source types) ---
        log.info(f"Starting VSAM definition JCL consideration for InputSource: {input_source_name_for_comments} (Type: {current_input_source_with_details.source_type})")
        vsam_jcls_generated_this_pass = 0
        # m204_files_in_this_source are files *defined by* the current input_source
        for m204_file_obj in m204_files_in_this_source:
            log.debug(f"Evaluating M204File ID {m204_file_obj.m204_file_id} ('{m204_file_obj.m204_file_name}') from source '{input_source_name_for_comments}' for VSAM JCL.")

            if m204_file_obj.m204_file_id in generated_vsam_jcl_for_m204file_ids_in_project_run:
                log.info(f"VSAM JCL for M204File ID {m204_file_obj.m204_file_id} ('{m204_file_obj.m204_file_name}') already generated in this project run. Skipping for this pass with source: {input_source_name_for_comments}.")
                continue

            should_generate_vsam = False
            source_type = current_input_source_with_details.source_type
            if source_type in ('parmlib', 'jcl'):
                # For PARMLIB or JCL sources, assume files it defines are candidates if they have VSAM characteristics
                if m204_file_obj.target_vsam_dataset_name or m204_file_obj.is_db_file or m204_file_obj.file_definition_json:
                    should_generate_vsam = True
                    log.info(f"M204File ID {m204_file_obj.m204_file_id} from {source_type.upper()} source '{input_source_name_for_comments}' is candidate for VSAM JCL.")
                else:
                    log.debug(f"M204File ID {m204_file_obj.m204_file_id} from {source_type.upper()} source '{input_source_name_for_comments}' lacks VSAM indicators. Skipping.")
            elif source_type == 'm204':
                if m204_file_obj.is_db_file: # Only for actual DB files if the source is m204
                    should_generate_vsam = True
                    log.info(f"M204File ID {m204_file_obj.m204_file_id} from M204 source '{input_source_name_for_comments}' is DB file, candidate for VSAM JCL.")
                else:
                    log.debug(f"M204File ID {m204_file_obj.m204_file_id} from M204 source '{input_source_name_for_comments}' is not DB file. Skipping VSAM JCL.")
            
            if should_generate_vsam:
                m204_name_part_raw = m204_file_obj.m204_file_name or f"FILE{m204_file_obj.m204_file_id}"
                m204_name_part = re.sub(r'[^A-Z0-9]', '', m204_name_part_raw.upper())[:8]
                if not m204_name_part:
                    m204_name_part = f"F{m204_file_obj.m204_file_id}"
                if not m204_name_part[0].isalpha():
                    m204_name_part = "V" + m204_name_part[:7]

                vsam_jcl_name = f"{cobol_program_id_base}_{m204_name_part}_vsam.jcl" # cobol_program_id_base is from current InputSource
                vsam_ds_name = m204_file_obj.target_vsam_dataset_name or f"DEFAULT.VSAM.{cobol_program_id_base}.{m204_name_part}"
                # Basic DSN sanitization
                if not re.match(r"^[A-Z@#$][A-Z0-9@#$]{0,7}(\.[A-Z@#$][A-Z0-9@#$]{0,7})+$", vsam_ds_name.upper()):
                    vsam_ds_name = f"PROJ.VSAM.{cobol_program_id_base}.{m204_name_part}" # Fallback DSN

                vsam_type = m204_file_obj.target_vsam_type or "KSDS"
                
                log.info(f"Preparing VSAM JCL for file: '{m204_file_obj.m204_file_name}', Target DSN: '{vsam_ds_name}', JCL Name: '{vsam_jcl_name}'")
                vsam_jcl_content = ""
                if llm_config._llm:
                    try:
                        llm_vsam_jcl_result = await self._llm_generate_vsam_jcl(m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments)
                        vsam_jcl_content = llm_vsam_jcl_result.jcl_content
                        if llm_vsam_jcl_result.generation_comments:
                            vsam_jcl_content = f"//* LLM Comments: {llm_vsam_jcl_result.generation_comments}\n" + vsam_jcl_content
                    except Exception as e:
                        log.warning(f"LLM VSAM JCL gen failed for {vsam_jcl_name}: {e}. Falling back.", exc_info=True)
                        vsam_jcl_content = self._generate_fallback_vsam_jcl(m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments)
                else:
                    vsam_jcl_content = self._generate_fallback_vsam_jcl(m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments)
                
                jcl_vsam_schema = JclOutputSchema(input_source_id=current_input_source_with_details.input_source_id, file_name=vsam_jcl_name, content=vsam_jcl_content, jcl_purpose="vsam", artifact_type="jcl_vsam")
                jcl_output_schemas.append(jcl_vsam_schema)
                db_jcl_artifacts_to_add.append(GeneratedJclArtifact(**jcl_vsam_schema.model_dump()))
                log.info(f"Generated VSAM JCL {vsam_jcl_name} for M204File ID {m204_file_obj.m204_file_id}.")
                
                generated_vsam_jcl_for_m204file_ids_in_project_run.add(m204_file_obj.m204_file_id)
                log.info(f"Marked M204File ID {m204_file_obj.m204_file_id} ('{m204_file_obj.m204_file_name}') as processed for VSAM JCL in this project run.")
                vsam_jcls_generated_this_pass += 1
            else:
                log.debug(f"Skipping VSAM JCL for M204File ID {m204_file_obj.m204_file_id} ('{m204_file_obj.m204_file_name}') as it did not meet generation criteria for source '{input_source_name_for_comments}'.")

        if vsam_jcls_generated_this_pass == 0:
            log.info(f"No new VSAM definition JCLs were generated by InputSource '{input_source_name_for_comments}' in this pass.")
        log.info(f"VSAM definition JCL generation process finished for InputSource '{input_source_name_for_comments}'.")
        
        # --- Save all collected artifacts to DB ---
        log.info(f"Preparing to save artifacts for InputSource ID: {current_input_source_with_details.input_source_id}")
        if db_cobol_artifacts_to_add:
            self.db.add_all(db_cobol_artifacts_to_add)
        if db_jcl_artifacts_to_add:
            self.db.add_all(db_jcl_artifacts_to_add)
        if db_unit_test_artifacts_to_add:
            self.db.add_all(db_unit_test_artifacts_to_add)
        # Note: self.db.commit() will be called by the calling function (generate_artifacts_for_project) after each InputSource.
        
        response = GeneratedArtifactsResponse(
            input_source_id=current_input_source_with_details.input_source_id, 
            cobol_files=cobol_output_schemas,
            jcl_files=jcl_output_schemas,
            unit_test_files=unit_test_output_schemas
        )
        log.info(f"Finished artifact generation for InputSource ID: {input_source.input_source_id} ('{input_source_name_for_comments}'). Returning {len(response.cobol_files)} COBOL, {len(response.jcl_files)} JCL, {len(response.unit_test_files)} Unit Test files.")
        return response
        
    async def generate_artifacts_for_project(self, project_id: int) -> List[InputSourceArtifacts]:
        project = self.db.query(Project).filter(Project.project_id == project_id).first()
        if not project:
            log.warning(f"Project with id {project_id} not found for artifact generation.")
            raise HTTPException(status_code=404, detail=f"Project with id {project_id} not found")

        # Fetch m204, parmlib, and jcl InputSources for the project
        input_sources_to_process = self.db.query(InputSource).filter(
            InputSource.project_id == project_id,
            InputSource.source_type.in_(['m204', 'parmlib', 'jcl'])  # Include jcl
        ).options(
            # Eager load existing artifacts for caching check
            joinedload(InputSource.generated_cobol_artifacts),
            joinedload(InputSource.generated_jcl_artifacts),
            joinedload(InputSource.generated_unit_test_artifacts)
        ).order_by(InputSource.source_type, InputSource.input_source_id).all() # Process m204 first, then parmlib/jcl

        all_project_artifacts_content: List[InputSourceArtifacts] = []
        
        # Set to track M204File IDs for which VSAM JCL has been generated in this project run
        # This prevents duplicate VSAM JCL generation if multiple InputSources could trigger it for the same M204File
        generated_vsam_jcl_for_m204file_ids_in_project_run: set[int] = set()


        if not input_sources_to_process:
            log.info(f"No M204, PARMLIB, or JCL type InputSource files found for project id {project_id}. Returning empty list.")
            return []

        for input_source_obj in input_sources_to_process:
            log.info(f"Processing artifacts for InputSource: {input_source_obj.original_filename or f'ID_{input_source_obj.input_source_id}'} (ID: {input_source_obj.input_source_id}, Type: {input_source_obj.source_type}) in project {project_id}")
            
            input_source_artifact_bundle = InputSourceArtifacts(
                input_source_id=input_source_obj.input_source_id,
                input_source_original_filename=input_source_obj.original_filename,
                generated_files=[] 
            )
            current_source_files: List[GeneratedFileContent] = []
            
            # Caching logic:
            # For 'm204', we ideally expect COBOL, JCL (run & VSAM), and Unit Tests.
            # For 'parmlib' or 'jcl', we only expect JCL (VSAM).
            # The effectiveness of caching VSAM JCL triggered by one IS but relevant to another is limited by this simple cache check.
            # The primary de-duplication of VSAM JCL content generation happens via the `generated_vsam_jcl_for_m204file_ids_in_project_run` set.
            use_cached_artifacts = False
            if input_source_obj.source_type == 'm204':
                # For m204, check if all its primary artifacts are cached.
                # VSAM JCLs for other project files (not directly linked) might still be generated if not in the project-wide set.
                if (bool(input_source_obj.generated_cobol_artifacts) and
                    bool(input_source_obj.generated_jcl_artifacts) and # This checks for *any* JCL.
                    bool(input_source_obj.generated_unit_test_artifacts)):
                    use_cached_artifacts = True
                    log.info(f"M204 InputSource ID: {input_source_obj.input_source_id} appears to have a full set of cached artifacts.")
            elif input_source_obj.source_type in ('parmlib', 'jcl'):
                # For parmlib or jcl, only check for JCL (VSAM).
                if bool(input_source_obj.generated_jcl_artifacts):
                    use_cached_artifacts = True
                    log.info(f"{input_source_obj.source_type.upper()} InputSource ID: {input_source_obj.input_source_id} has cached JCL artifacts.")


            if use_cached_artifacts:
                log.info(f"Attempting to use cached artifacts from DB for InputSource ID: {input_source_obj.input_source_id} (Type: {input_source_obj.source_type})")
                # Load whatever is cached.
                for cobol_artifact_orm in input_source_obj.generated_cobol_artifacts: # Will be empty for parmlib/jcl
                    current_source_files.append(GeneratedFileContent(file_name=cobol_artifact_orm.file_name, content=cobol_artifact_orm.content, artifact_type=cobol_artifact_orm.artifact_type))
                for jcl_artifact_orm in input_source_obj.generated_jcl_artifacts:
                    current_source_files.append(GeneratedFileContent(file_name=jcl_artifact_orm.file_name, content=jcl_artifact_orm.content, artifact_type=jcl_artifact_orm.artifact_type))
                    # Note: We don't populate `generated_vsam_jcl_for_m204file_ids_in_project_run` from cache here.
                    # The set is for preventing re-GENERATION in the current run. If cache is used, generation is skipped.
                for unit_test_artifact_orm in input_source_obj.generated_unit_test_artifacts: # Will be empty for parmlib/jcl
                    current_source_files.append(GeneratedFileContent(file_name=unit_test_artifact_orm.file_name, content=unit_test_artifact_orm.content, artifact_type=unit_test_artifact_orm.artifact_type))
                
                log.info(f"Loaded {len(current_source_files)} cached files for InputSource ID: {input_source_obj.input_source_id}.")
                # If we use cache for an m204 source, we might still want to ensure project-wide VSAM JCLs are generated
                # if they weren't part of this m204 source's original JCL set.
                # This is a complex scenario. The current logic will call _generate_and_save if cache is not "complete".
                # If cache IS complete for an m204, it's assumed to be sufficient.
                # The `generated_vsam_jcl_for_m204file_ids_in_project_run` set primarily prevents re-generation during the generation phase.

            # If not using cache (or cache is deemed insufficient for the type), regenerate.
            if not use_cached_artifacts:
                log.info(f"Cache not used or incomplete for InputSource ID: {input_source_obj.input_source_id}. Generating anew.")
                try:
                    # Call the single source generation function, passing the project-wide set
                    single_source_artifacts_response: GeneratedArtifactsResponse = \
                        await self._generate_and_save_artifacts_for_single_input_source(
                            input_source_obj, 
                            generated_vsam_jcl_for_m204file_ids_in_project_run # Pass the set
                        )
                    
                    # Collect files from the response
                    for cobol_schema in single_source_artifacts_response.cobol_files: # Will be empty if input_source_obj was parmlib/jcl
                        current_source_files.append(GeneratedFileContent(file_name=cobol_schema.file_name, content=cobol_schema.content, artifact_type=cobol_schema.artifact_type))
                    for jcl_schema in single_source_artifacts_response.jcl_files: # Will contain VSAM for parmlib/jcl, or run+VSAM for m204
                        current_source_files.append(GeneratedFileContent(file_name=jcl_schema.file_name, content=jcl_schema.content, artifact_type=jcl_schema.artifact_type))
                    for unit_test_schema in single_source_artifacts_response.unit_test_files: # Will be empty if input_source_obj was parmlib/jcl
                        current_source_files.append(GeneratedFileContent(file_name=unit_test_schema.file_name, content=unit_test_schema.content, artifact_type=unit_test_schema.artifact_type))
                    
                    self.db.commit() # Commit after each successful single source generation
                    log.info(f"Successfully generated and saved artifacts for InputSource ID: {input_source_obj.input_source_id}")

                except Exception as e:
                    self.db.rollback()
                    log.error(f"Error generating artifacts for InputSource {input_source_obj.input_source_id} ('{input_source_obj.original_filename}'): {e}", exc_info=True)
                    # Create an error placeholder file content
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