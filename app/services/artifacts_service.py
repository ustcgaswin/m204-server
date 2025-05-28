from sqlalchemy.orm import Session, selectinload, joinedload
from fastapi import HTTPException
import uuid
import datetime
from typing import List, Optional, Dict, Any, Tuple
import json
import os
import asyncio
import re

from app.models.m204_file_model import M204File
from app.models.project_model import Project
from app.models.procedure_model import Procedure
from app.models.image_statement_model import ImageStatement
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

class ImageToCobolFDOutput(BaseModel):
    image_logical_name: Optional[str] = PydanticField(description="M204 logical name for SELECT.", default=None)
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
            llm_call = llm_config._llm.as_structured_llm(M204ProcedureToCobolOutput)
            response = await llm_call.acomplete(prompt=prompt_fstr)
            json_text_output = response.text
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


    async def _llm_convert_image_to_fd(self, image_statement: ImageStatement, m204_file_name_for_select: str) -> ImageToCobolFDOutput:
        # ... existing code ...
        if not llm_config._llm or not image_statement.image_content:
            log.warning(f"LLM not available or no content for IMAGE statement: {image_statement.image_statement_id}. Returning placeholder FD.")
            select_name = (image_statement.referenced_m204_logical_name or m204_file_name_for_select or f"IMG{image_statement.image_statement_id}").replace("-", "")[:8]
            return ImageToCobolFDOutput(
                image_logical_name=select_name,
                file_control_entry=f"       SELECT {select_name}-FILE ASSIGN TO {select_name}.\n",
                file_description_entry=f"   FD  {select_name}-FILE.\n"
                                     f"   01  {select_name}-RECORD PIC X(80). *> Placeholder\n",
                comments="LLM not available or IMAGE content missing. Manual FD creation required."
            )

        prompt_fstr = f"""
You are an expert M204 to COBOL migration specialist.
Convert the following M204 IMAGE statement content into COBOL FILE-CONTROL (SELECT) and FILE SECTION (FD) entries.
The M204 IMAGE statement defines a layout, often for a file or screen.
M204 Logical Name (if specified in IMAGE): {image_statement.referenced_m204_logical_name or 'Not specified, use DDNAME'}
Associated M204 File DDNAME (for SELECT if logical name not in IMAGE): {m204_file_name_for_select}
M204 IMAGE Content:
```m204
{image_statement.image_content}
```
Respond with a JSON object structured according to the ImageToCobolFDOutput model.
`image_logical_name` should be the M204 logical name if present in the IMAGE, otherwise the DDNAME.
`file_control_entry` should be the `SELECT ... ASSIGN TO ...` statement.
`file_description_entry` should be the `FD ...` entry with its record layout (01 level).
`working_storage_entries` can optionally include any related W-S items.
"""
        json_text_output: Optional[str] = None
        try:
            llm_call = llm_config._llm.as_structured_llm(ImageToCobolFDOutput)
            response = await llm_call.acomplete(prompt=prompt_fstr)
            json_text_output = response.text
            return ImageToCobolFDOutput(**json.loads(json_text_output))
        except Exception as e:
            log.error(f"LLM error converting IMAGE {image_statement.image_statement_id} to FD: {e}. Raw output: {json_text_output}", exc_info=True)
            select_name = (image_statement.referenced_m204_logical_name or m204_file_name_for_select or f"IMG{image_statement.image_statement_id}").replace("-", "")[:8]
            return ImageToCobolFDOutput(
                image_logical_name=select_name,
                file_control_entry=f"       SELECT {select_name}-FILE ASSIGN TO {select_name}. *> ERROR IN CONVERSION\n",
                file_description_entry=f"   FD  {select_name}-FILE. *> ERROR IN CONVERSION\n"
                                     f"   01  {select_name}-RECORD PIC X(80). *> Placeholder due to error\n",
                comments=f"LLM FD conversion failed: {str(e)}"
            )

    def _clear_existing_artifacts_for_input_source(self, input_source_id: int):
        # ... existing code ...
        self.db.query(GeneratedCobolArtifact).filter(GeneratedCobolArtifact.input_source_id == input_source_id).delete(synchronize_session=False)
        self.db.query(GeneratedJclArtifact).filter(GeneratedJclArtifact.input_source_id == input_source_id).delete(synchronize_session=False)
        self.db.query(GeneratedUnitTestArtifact).filter(GeneratedUnitTestArtifact.input_source_id == input_source_id).delete(synchronize_session=False)
        log.info(f"Cleared existing artifacts for input_source_id: {input_source_id}")

    async def _llm_generate_vsam_jcl(self, m204_file_obj: M204File, cobol_program_id_base: str, vsam_ds_name: str, vsam_type: str, input_source_name_for_comments: str) -> VsamJclGenerationOutput:
        if not llm_config._llm:
            log.warning(f"LLM not available for VSAM JCL generation for M204 File: {m204_file_obj.m204_file_name}. Returning placeholder JCL structure.")
            # Fallback to template generation will be handled by the caller
            raise ValueError("LLM not available")

        field_details_str_parts = []
        if m204_file_obj.fields:
            for field in m204_file_obj.fields:
                field_info = f"- Field Name: {field.field_name}"
                if field.target_vsam_data_type:
                    field_info += f", VSAM Type: {field.target_vsam_data_type}"
                if field.target_vsam_length:
                    field_info += f", Length: {field.target_vsam_length}"
                if field.is_primary_key_component: 
                    field_info += f", Is Key Component: Yes (Order: {field.target_vsam_key_order or 'N/A'})"
                field_details_str_parts.append(field_info)
        field_details_str = "\n".join(field_details_str_parts) if field_details_str_parts else "No detailed field information available."

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

Field Information (if available, use this to determine RECORDSIZE and KEYS for KSDS):
{field_details_str}

Key Requirements for DEFINE CLUSTER:
- NAME: Use the Target VSAM Dataset Name.
- For KSDS: Determine KEYS(length offset) from fields marked as key components. Sum their lengths. Offset is usually 0.
- For ESDS: Use NONINDEXED.
- For RRDS: Use NUMBERED.
- RECORDSIZE(average maximum): Determine from the sum of field lengths if available. Otherwise, use a sensible default like (80 80) or (100 100).
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
            llm_call = llm_config._llm.as_structured_llm(VsamJclGenerationOutput)
            response = await llm_call.acomplete(prompt=prompt_fstr)
            json_text_output = response.text
            return VsamJclGenerationOutput(**json.loads(json_text_output))
        except Exception as e:
            log.error(f"LLM error generating VSAM JCL for {m204_file_obj.m204_file_name}: {e}. Raw output: {json_text_output}", exc_info=True)
            raise  # Re-raise to be caught by the caller for fallback

    def _generate_fallback_vsam_jcl(self, m204_file_obj: M204File, cobol_program_id_base: str, vsam_ds_name: str, vsam_type: str, input_source_name_for_comments: str) -> str:
        log.info(f"Generating fallback VSAM JCL for {vsam_ds_name}")
        
        # Determine RECORDSIZE from fields
        avg_rec_len_val = 0
        if m204_file_obj.fields:
            total_len = sum(f.target_vsam_length for f in m204_file_obj.fields if f.target_vsam_length and f.target_vsam_length > 0)
            if total_len > 0:
                avg_rec_len_val = total_len
        
        avg_rec_len = str(avg_rec_len_val) if avg_rec_len_val > 0 else "80"
        max_rec_len = avg_rec_len # Assuming fixed length for simplicity in fallback

        # Determine KEYS for KSDS from fields
        key_info_str = "6 0" # Default
        if vsam_type.upper() == "KSDS":
            key_fields_components = []
            if m204_file_obj.fields:
                key_fields_components = [f for f in m204_file_obj.fields if f.is_primary_key_component and f.target_vsam_length and f.target_vsam_length > 0]
                if key_fields_components:
                    key_fields_components.sort(key=lambda f: (f.target_vsam_key_order if f.target_vsam_key_order is not None else float('inf')))
                    total_key_length = sum(f.target_vsam_length for f in key_fields_components)
                    if total_key_length > 0:
                        key_info_str = f"{total_key_length} 0" # Assuming offset 0 for primary key
                        log.info(f"Fallback JCL: Derived KEYS for {vsam_ds_name} from fields: {key_info_str}. Fields: {[f.field_name for f in key_fields_components]}")
                    else:
                        log.warning(f"Fallback JCL: Key components found for {vsam_ds_name}, but total length is 0. Using default '6 0'. Fields: {[f.field_name for f in key_fields_components]}")
                else: # No field components, check primary_key_field_name on M204File
                    if m204_file_obj.primary_key_field_name and re.match(r"^\d+\s+\d+$", m204_file_obj.primary_key_field_name):
                        key_info_str = m204_file_obj.primary_key_field_name
                        log.info(f"Fallback JCL: Using KEYS from M204File.primary_key_field_name: {key_info_str} for {vsam_ds_name}")
                    elif m204_file_obj.primary_key_field_name:
                        log.warning(f"Fallback JCL: M204File.primary_key_field_name '{m204_file_obj.primary_key_field_name}' for {vsam_ds_name} is not 'length offset'. Default '6 0' remains.")
                    else:
                        log.warning(f"Fallback JCL: No primary key components or direct key info found for KSDS {vsam_ds_name}. Using default '6 0'.")
            else: # No fields on m204_file_obj
                if m204_file_obj.primary_key_field_name and re.match(r"^\d+\s+\d+$", m204_file_obj.primary_key_field_name):
                    key_info_str = m204_file_obj.primary_key_field_name
                    log.info(f"Fallback JCL: No fields, using KEYS from M204File.primary_key_field_name: {key_info_str} for {vsam_ds_name}")
                else:
                    log.warning(f"Fallback JCL: No fields and M204File.primary_key_field_name not usable for KSDS {vsam_ds_name}. Using default '6 0'.")


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
        self._clear_existing_artifacts_for_input_source(input_source.input_source_id)

        cobol_output_schemas = []
        jcl_output_schemas = []
        unit_test_output_schemas = []
        
        db_cobol_artifacts_to_add = []
        db_jcl_artifacts_to_add = []
        db_unit_test_artifacts_to_add = []

        current_input_source_with_details = self.db.query(InputSource).filter(InputSource.input_source_id == input_source.input_source_id).options(
            selectinload(InputSource.procedures_defined).selectinload(Procedure.variables_in_procedure),
            selectinload(InputSource.m204_files_defined).options(
                selectinload(M204File.image_statements),
                selectinload(M204File.fields) # Eager load M204File.fields
            )
        ).one_or_none()

        if not current_input_source_with_details:
            log.error(f"InputSource ID {input_source.input_source_id} not found during artifact generation.")
            return GeneratedArtifactsResponse(input_source_id=input_source.input_source_id)

        input_source_name_for_comments = current_input_source_with_details.original_filename or f"InputSourceID_{current_input_source_with_details.input_source_id}"
        cobol_program_id_base = self._sanitize_filename_base(current_input_source_with_details.original_filename, default_prefix="M204PROG")
        
        log.info(f"Generating artifacts for InputSource: {input_source_name_for_comments} (ID: {current_input_source_with_details.input_source_id}), COBOL Program ID base: {cobol_program_id_base}")

        related_procedures = current_input_source_with_details.procedures_defined or []
        m204_files_in_this_source = current_input_source_with_details.m204_files_defined or []
        
        cobol_file_name = f"{cobol_program_id_base}.cbl"
        # ... (COBOL generation logic remains the same) ...
        file_control_entries_str = ""
        file_section_fds_str = ""
        working_storage_for_fds_str = ""
        
        all_image_statements_with_context: List[Tuple[ImageStatement, str]] = []
        for m204_file_obj in m204_files_in_this_source:
            if m204_file_obj.image_statements:
                parent_ddname = m204_file_obj.m204_file_name or "UNKNOWN_PARENT_DDNAME"
                for img_stmt_obj in m204_file_obj.image_statements:
                    all_image_statements_with_context.append((img_stmt_obj, parent_ddname))
        
        unique_image_statements_with_context_dict: Dict[int, Tuple[ImageStatement, str]] = {}
        for img_stmt_obj, parent_ddname in all_image_statements_with_context:
            if img_stmt_obj.image_statement_id not in unique_image_statements_with_context_dict:
                unique_image_statements_with_context_dict[img_stmt_obj.image_statement_id] = (img_stmt_obj, parent_ddname)
        
        unique_image_statements_and_contexts = list(unique_image_statements_with_context_dict.values())

        if llm_config._llm:
            image_conversion_tasks = [self._llm_convert_image_to_fd(img_stmt, ddname) for img_stmt, ddname in unique_image_statements_and_contexts]
            converted_fds_results = await asyncio.gather(*image_conversion_tasks, return_exceptions=True)
            for result in converted_fds_results:
                if isinstance(result, ImageToCobolFDOutput):
                    file_control_entries_str += result.file_control_entry
                    file_section_fds_str += result.file_description_entry + "\n"
                    if result.working_storage_entries:
                        working_storage_for_fds_str += result.working_storage_entries + "\n"
                    if result.comments:
                        file_section_fds_str += f"* {result.comments}\n"
                elif isinstance(result, Exception):
                    log.error(f"Error converting IMAGE to FD: {result}", exc_info=True)
                    file_section_fds_str += "* --- ERROR DURING IMAGE TO FD CONVERSION --- \n"
        else:
            file_section_fds_str = "* LLM not configured for IMAGE to FD conversion.\n"

        procedure_division_main_logic = ""
        procedure_division_paragraphs = ""
        cobol_conversion_comments = []

        if llm_config._llm:
            proc_conversion_tasks = [self._llm_convert_m204_proc_to_cobol(proc) for proc in related_procedures]
            converted_procs_results = await asyncio.gather(*proc_conversion_tasks, return_exceptions=True)
            for result in converted_procs_results:
                if isinstance(result, M204ProcedureToCobolOutput):
                    para_base = re.sub(r'[^A-Z0-9-]', '', result.m204_procedure_name.upper().replace('%', 'P').replace('$', 'D').replace('_', '-').replace('#','N'))
                    paragraph_name = (para_base[:28] + "-PARA")
                    if not paragraph_name or not (paragraph_name[0].isalpha() or paragraph_name[0].isdigit()):
                        paragraph_name = "P" + (paragraph_name[1:] if paragraph_name else "")
                    procedure_division_main_logic += f"           PERFORM {paragraph_name}.\n"
                    procedure_division_paragraphs += f"{paragraph_name}.\n{result.cobol_code_block}\n"
                    if result.comments:
                        cobol_conversion_comments.append(f"* Procedure {result.m204_procedure_name}: {result.comments}")
                elif isinstance(result, Exception):
                    log.error(f"Error converting procedure to COBOL: {result}", exc_info=True)
                    procedure_division_paragraphs += "* --- ERROR DURING PROCEDURE CONVERSION FOR A PROCEDURE --- \n"
        else:
            procedure_division_paragraphs = "      * LLM not configured for M204 Procedure to COBOL conversion.\n"
            if related_procedures:
                for proc in related_procedures:
                     proc_name_sanitized = re.sub(r'[^A-Z0-9-]', '', proc.m204_proc_name.upper().replace('%', 'P').replace('$', 'D').replace('_', '-').replace('#','N'))[:28]
                     if not proc_name_sanitized or not (proc_name_sanitized[0].isalpha() or proc_name_sanitized[0].isdigit()):
                         proc_name_sanitized = "MOCK-P" + (proc_name_sanitized[1:] if proc_name_sanitized else "")
                     proc_name_sanitized += "-PARA"
                     procedure_division_main_logic += f"           PERFORM {proc_name_sanitized}.\n"
                     procedure_division_paragraphs += f"{proc_name_sanitized}.\n           DISPLAY 'MOCK EXECUTION OF {proc.m204_proc_name}'.\n\n"
        cobol_content = f"""\
IDENTIFICATION DIVISION.
PROGRAM-ID. {cobol_program_id_base}.
AUTHOR. ArtifactGenerator.
DATE-WRITTEN. {datetime.date.today().strftime("%Y-%m-%d")}.
* COBOL program generated for M204 Input Source: {input_source_name_for_comments} (ID: {current_input_source_with_details.input_source_id})
* Covers M204 Files: {', '.join([mf.m204_file_name or f'FileID_{mf.m204_file_id}' for mf in m204_files_in_this_source]) if m204_files_in_this_source else 'None'}
{("*" + "\n* ".join(cobol_conversion_comments)) if cobol_conversion_comments else ""}
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
{file_control_entries_str if file_control_entries_str else "      * No FILE-CONTROL entries generated."}
DATA DIVISION.
FILE SECTION.
{file_section_fds_str if file_section_fds_str else "   * No FDs generated."}
WORKING-STORAGE SECTION.
{working_storage_for_fds_str if working_storage_for_fds_str else "   * No specific W-S entries from FDs."}
PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
MAIN-PARAGRAPH.
{procedure_division_main_logic if procedure_division_main_logic else "           DISPLAY 'No M204 procedures mapped to this COBOL program.'."}
           STOP RUN.
{procedure_division_paragraphs if procedure_division_paragraphs else "   * No procedure paragraphs generated."}
"""
        cobol_output_schema = CobolOutputSchema(
            input_source_id=current_input_source_with_details.input_source_id, 
            file_name=cobol_file_name,
            content=cobol_content,
            artifact_type="cobol"
        )
        cobol_output_schemas.append(cobol_output_schema)
        db_cobol_artifacts_to_add.append(GeneratedCobolArtifact(**cobol_output_schema.model_dump()))

        # ... (Unit test generation logic remains the same) ...
        unit_test_file_name = f"test_{cobol_program_id_base}.txt"
        unit_test_content_parts = [
            f"Unit Test Plan for COBOL Program: {cobol_file_name}\n",
            f"Generated from M204 Input Source: {input_source_name_for_comments} (ID: {current_input_source_with_details.input_source_id})\n",
            "Based on M204 Procedures:\n"
        ]
        if not related_procedures:
            unit_test_content_parts.append("- No M204 procedures for test case generation.\n")
        for proc in related_procedures:
            unit_test_content_parts.append(f"\n--- Test Cases for M204 Procedure: {proc.m204_proc_name} ---\n")
            if proc.suggested_test_cases_json:
                try:
                    test_cases_data = json.loads(proc.suggested_test_cases_json) if isinstance(proc.suggested_test_cases_json, str) else proc.suggested_test_cases_json
                    for tc_data in test_cases_data: 
                        tc = TestCase(**tc_data) 
                        unit_test_content_parts.extend([
                            f"Test Case ID: {tc.test_case_id}\n  Description: {tc.description}\n",
                            *("  Preconditions:\n" + "".join([f"    - {pre}\n" for pre in tc.preconditions]) if tc.preconditions else ""),
                            "  Inputs:\n" + "".join([f"    - {k}: {v}\n" for k, v in tc.inputs.items()]),
                            "  Expected Outputs:\n" + "".join([f"    - {k}: {v}\n" for k, v in tc.expected_outputs.items()]),
                            f"  Expected Behavior: {tc.expected_behavior_description}\n\n"
                        ])
                except Exception as e:
                    log.error(f"Error parsing test cases for {proc.m204_proc_name}: {e}", exc_info=True)
                    unit_test_content_parts.append(f"  Error parsing test cases: {str(e)}\n")
            else:
                unit_test_content_parts.append("  No pre-defined test cases.\n")
        
        unit_test_schema = UnitTestOutputSchema(
            input_source_id=current_input_source_with_details.input_source_id, 
            file_name=unit_test_file_name,
            content="".join(unit_test_content_parts),
        )
        unit_test_output_schemas.append(unit_test_schema)
        db_unit_test_artifacts_to_add.append(GeneratedUnitTestArtifact(**unit_test_schema.model_dump()))


        # General JCL (Run JCL)
        general_jcl_file_name = f"{cobol_program_id_base}_run.jcl"
        dd_statements_for_jcl = []
        if m204_files_in_this_source:
            for m204_file_obj in m204_files_in_this_source:
                raw_dd_name = m204_file_obj.m204_file_name
                if not raw_dd_name:
                    log.warning(f"M204File ID {m204_file_obj.m204_file_id} has no m204_file_name; skipping DD card for run JCL.")
                    continue
                dd_name_candidate = re.sub(r'[^A-Z0-9]', '', raw_dd_name.upper())
                if not dd_name_candidate:
                    dd_name_candidate = f"M204F{m204_file_obj.m204_file_id}"
                if not dd_name_candidate[0].isalpha():
                    dd_name_candidate = "X" + dd_name_candidate
                dd_name_final = dd_name_candidate[:8]
                if not dd_name_final:
                    log.warning(f"Could not derive valid DDNAME for M204File ID {m204_file_obj.m204_file_id} (raw: {raw_dd_name}) for run JCL.")
                    continue
                dsn = m204_file_obj.target_vsam_dataset_name or m204_file_obj.m204_logical_dataset_name or f"YOUR.DATASET.FOR.{dd_name_final}"
                if "." not in dsn:
                    dsn = f"YOUR.APP.{dsn}" # Basic DSN qualification
                
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
        jcl_general_schema = JclOutputSchema(
            input_source_id=current_input_source_with_details.input_source_id, 
            file_name=general_jcl_file_name, content=general_jcl_content,
            jcl_purpose="general", artifact_type="jcl_general"
        )
        jcl_output_schemas.append(jcl_general_schema)
        db_jcl_artifacts_to_add.append(GeneratedJclArtifact(**jcl_general_schema.model_dump()))

        # VSAM Definition JCLs
        for m204_file_obj in m204_files_in_this_source:
            if m204_file_obj.is_db_file:
                m204_name_part = re.sub(r'[^A-Z0-9]', '', (m204_file_obj.m204_file_name or f"DBF{m204_file_obj.m204_file_id}").upper())[:8]
                vsam_jcl_name = f"{cobol_program_id_base}_{m204_name_part}_vsam.jcl"
                vsam_ds_name = m204_file_obj.target_vsam_dataset_name or f"DEFAULT.VSAM.{cobol_program_id_base}.{m204_name_part}"
                vsam_type = m204_file_obj.target_vsam_type or "KSDS"
                
                vsam_jcl_content = ""
                llm_comments = None

                if llm_config._llm:
                    try:
                        log.info(f"Attempting LLM generation for VSAM JCL: {vsam_jcl_name}")
                        llm_vsam_jcl_result = await self._llm_generate_vsam_jcl(
                            m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments
                        )
                        vsam_jcl_content = llm_vsam_jcl_result.jcl_content
                        llm_comments = llm_vsam_jcl_result.generation_comments
                        if llm_comments:
                            vsam_jcl_content = f"//* LLM Generation Comments:\n//* {llm_comments.replacechr(10, 'CHR(10)')}\n" + vsam_jcl_content
                        log.info(f"LLM successfully generated VSAM JCL for: {vsam_jcl_name}")
                    except Exception as e:
                        log.warning(f"LLM generation for VSAM JCL {vsam_jcl_name} failed: {e}. Falling back to template.")
                        vsam_jcl_content = self._generate_fallback_vsam_jcl(
                            m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments
                        )
                else:
                    log.info(f"LLM not configured. Generating fallback VSAM JCL for: {vsam_jcl_name}")
                    vsam_jcl_content = self._generate_fallback_vsam_jcl(
                        m204_file_obj, cobol_program_id_base, vsam_ds_name, vsam_type, input_source_name_for_comments
                    )

                jcl_vsam_schema = JclOutputSchema(
                    input_source_id=current_input_source_with_details.input_source_id, 
                    file_name=vsam_jcl_name, content=vsam_jcl_content,
                    jcl_purpose="vsam", artifact_type="jcl_vsam"
                )
                jcl_output_schemas.append(jcl_vsam_schema)
                db_jcl_artifacts_to_add.append(GeneratedJclArtifact(**jcl_vsam_schema.model_dump()))
        
        if db_cobol_artifacts_to_add:
            self.db.add_all(db_cobol_artifacts_to_add)
        if db_jcl_artifacts_to_add:
            self.db.add_all(db_jcl_artifacts_to_add)
        if db_unit_test_artifacts_to_add:
            self.db.add_all(db_unit_test_artifacts_to_add)
        
        return GeneratedArtifactsResponse(
            input_source_id=current_input_source_with_details.input_source_id, 
            cobol_files=cobol_output_schemas,
            jcl_files=jcl_output_schemas,
            unit_test_files=unit_test_output_schemas
        )

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
            # M204File and M204Field data will be loaded within _generate_and_save_artifacts_for_single_input_source
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
            
            use_cached_artifacts = has_cobol and has_jcl and has_unit_tests # Simplified cache check

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

