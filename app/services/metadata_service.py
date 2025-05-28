from sqlalchemy.orm import Session, selectinload
from typing import List, Optional
from sqlalchemy import  or_, func
from sqlalchemy.exc import SQLAlchemyError

from app.models.procedure_model import Procedure
from app.models.m204_variable_model import M204Variable
from app.models.input_source_model import InputSource
from app.models.m204_file_model import M204File
from app.models.m204_file_field_model import M204Field
from app.models.image_statement_model import ImageStatement
from app.models.dd_statement_model import DDStatement # Added import
from app.models.procedure_call_model import ProcedureCall # Added import

from app.schemas.m204_analysis_schema import (
    M204FileUpdateSchema, 
    M204VariableUpdateSchema, 
    M204ProcedureUpdateSchema,
    M204FieldUpdateSchema
)
from app.utils.logger import log

# --- InputSource related metadata ---
async def get_m204_db_definition_files_by_project(db: Session, project_id: int, skip: int = 0, limit: int = 100) -> List[InputSource]:
    log.info(f"MetadataService: Fetching InputSource files that define M204 DB files for project ID: {project_id}")
    # Files that are PARMLIB or M204 source types and are associated with an M204File marked as is_db_file=True
    # Or JCL files that have DD statements pointing to known M204 database DSN patterns.
    # This query identifies InputSource files that *define* or *reference* M204 DB files.

    # Subquery to find input_source_ids from M204File where is_db_file is true and defined_in_input_source_id is not null
    m204_def_source_ids = db.query(M204File.defined_in_input_source_id)\
        .filter(M204File.project_id == project_id, M204File.is_db_file is True, M204File.defined_in_input_source_id.isnot(None))\
        .distinct()

    # Subquery to find input_source_ids from DDStatement that look like M204 DB DSNs
    jcl_db_source_ids = db.query(DDStatement.input_source_id)\
        .filter(
            DDStatement.project_id == project_id,
            or_(
                DDStatement.dsn.ilike('%.DBASEF%'), # Common M204 DB file suffix
                DDStatement.dsn.ilike('%.%204%')    # Contains '204' often used in M204 DSNs
            )
        )\
        .distinct()

    combined_source_ids_query = m204_def_source_ids.union(jcl_db_source_ids)
    source_ids = [row[0] for row in db.execute(combined_source_ids_query).fetchall()]

    if not source_ids:
        return []

    query = db.query(InputSource)\
        .filter(InputSource.project_id == project_id, InputSource.input_source_id.in_(source_ids))\
        .order_by(InputSource.original_filename)\
        .offset(skip)\
        .limit(limit)

    return query.all()

async def count_m204_db_definition_files_for_project(db: Session, project_id: int) -> int:
    log.info(f"MetadataService: Counting InputSource files that define M204 DB files for project ID: {project_id}")
    m204_def_source_ids = db.query(M204File.defined_in_input_source_id)\
        .filter(M204File.project_id == project_id, M204File.is_db_file is True, M204File.defined_in_input_source_id.isnot(None))\
        .distinct()
    jcl_db_source_ids = db.query(DDStatement.input_source_id)\
        .filter(
            DDStatement.project_id == project_id,
            or_(
                DDStatement.dsn.ilike('%.DBASEF%'),
                DDStatement.dsn.ilike('%.%204%')
            )
        )\
        .distinct()
    combined_source_ids_query = m204_def_source_ids.union(jcl_db_source_ids)
    source_ids = [row[0] for row in db.execute(combined_source_ids_query).fetchall()]
    if not source_ids:
        return 0
    return db.query(func.count(InputSource.input_source_id))\
        .filter(InputSource.project_id == project_id, InputSource.input_source_id.in_(source_ids))\
        .scalar() or 0

# --- M204File related metadata ---
async def get_m204_database_files_by_project(db: Session, project_id: int, skip: int = 0, limit: int = 100) -> List[M204File]:
    log.info(f"MetadataService: Fetching M204 Database File entries (is_db_file=True) for project ID: {project_id}")

    # --- Debug Logging: Log all M204Files for the project before filtering ---
    all_project_m204_files = db.query(M204File.m204_file_id, M204File.m204_file_name, M204File.is_db_file)\
                               .filter(M204File.project_id == project_id)\
                               .all()
    if not all_project_m204_files:
        log.info(f"MetadataService: DEBUG - No M204File entries found at all for project ID: {project_id} before 'is_db_file' filter.")
    else:
        log.info(f"MetadataService: DEBUG - Found {len(all_project_m204_files)} M204File entries in total for project ID: {project_id} before 'is_db_file' filter. Details:")
        for mf_id, mf_name, mf_is_db in all_project_m204_files:
            log.info(f"MetadataService: DEBUG -   M204File ID: {mf_id}, Name: '{mf_name}', is_db_file: {mf_is_db}")
    # --- End Debug Logging ---

    files = db.query(M204File)\
              .filter(M204File.project_id == project_id, M204File.is_db_file == True)\
              .options(selectinload(M204File.fields).selectinload(M204Field.defined_in_input_source))\
              .order_by(M204File.m204_file_name)\
              .offset(skip)\
              .limit(limit)\
              .all()  # noqa: E712
    
    if not files:
        log.info(f"MetadataService: No M204File entries found for project ID: {project_id} *after* filtering for 'is_db_file == True'.")
    else:
        log.info(f"MetadataService: Found {len(files)} M204File entries for project ID: {project_id} *after* filtering for 'is_db_file == True'.")

    return files

async def count_m204_database_files_for_project(db: Session, project_id: int) -> int:
    log.info(f"MetadataService: Counting M204 Database File entries for project ID: {project_id}")
    return db.query(func.count(M204File.m204_file_id))\
             .filter(M204File.project_id == project_id, M204File.is_db_file == True)\
             .scalar() or 0  # noqa: E712

async def get_other_m204_files_by_project(db: Session, project_id: int, skip: int = 0, limit: int = 100) -> List[M204File]:
    log.info(f"MetadataService: Fetching other (non-DB) M204File entries for project ID: {project_id}")
    files = db.query(M204File)\
              .filter(M204File.project_id == project_id, or_(M204File.is_db_file == False, M204File.is_db_file.is_(None)))\
              .options(selectinload(M204File.image_statements).selectinload(ImageStatement.input_source))\
              .order_by(M204File.m204_file_name)\
              .offset(skip)\
              .limit(limit)\
              .all()  # noqa: E712
    return files

async def count_other_m204_files_for_project(db: Session, project_id: int) -> int:
    log.info(f"MetadataService: Counting other (non-DB) M204File entries for project ID: {project_id}")
    return db.query(func.count(M204File.m204_file_id))\
             .filter(M204File.project_id == project_id, or_(M204File.is_db_file == False, M204File.is_db_file.is_(None)))\
             .scalar() or 0  # noqa: E712

async def get_m204_file_by_id(db: Session, project_id: int, m204_file_id: int) -> Optional[M204File]:
    log.info(f"MetadataService: Fetching M204File by ID: {m204_file_id} for project ID: {project_id}")
    m204_file = db.query(M204File).filter(
        M204File.project_id == project_id,
        M204File.m204_file_id == m204_file_id
    ).options(
        selectinload(M204File.fields).selectinload(M204Field.defined_in_input_source),
        selectinload(M204File.image_statements).selectinload(ImageStatement.input_source),
        selectinload(M204File.defined_in_source) # For the file itself
    ).first()
    return m204_file

async def update_m204_file_details(
    db: Session,
    project_id: int,
    m204_file_id: int,
    update_data: M204FileUpdateSchema
) -> Optional[M204File]:
    log.info(f"MetadataService: Updating M204File ID: {m204_file_id} for project ID: {project_id}")
    db_m204_file = await get_m204_file_by_id(db, project_id, m204_file_id) # This already loads related data
    if not db_m204_file:
        log.warning(f"MetadataService: M204File with ID {m204_file_id} not found in project {project_id} for update.")
        return None

    update_values = update_data.model_dump(exclude_unset=True)
    if not update_values:
        log.info(f"MetadataService: No update values provided for M204File ID: {m204_file_id}")
        return db_m204_file

    for key, value in update_values.items():
        setattr(db_m204_file, key, value)

    try:
        db.add(db_m204_file)
        db.commit()
        db.refresh(db_m204_file) # Refresh to get any DB-generated changes and ensure relationships are current
        log.info(f"MetadataService: M204File ID: {m204_file_id} updated successfully.")
        return db_m204_file
    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"MetadataService: Database error updating M204File ID {m204_file_id}: {e}", exc_info=True)
        raise

# --- M204Field related metadata ---
async def get_m204_field_by_id(db: Session, project_id: int, m204_field_id: int) -> Optional[M204Field]:
    log.info(f"MetadataService: Fetching M204Field by ID: {m204_field_id} for project ID: {project_id}")
    m204_field = db.query(M204Field).filter(
        M204Field.project_id == project_id,
        M204Field.m204_field_id == m204_field_id
    ).options(
        selectinload(M204Field.m204_file),
        selectinload(M204Field.defined_in_input_source)
    ).first()
    return m204_field

async def update_m204_field_details(
    db: Session,
    project_id: int,
    m204_field_id: int,
    update_data: M204FieldUpdateSchema
) -> Optional[M204Field]:
    log.info(f"MetadataService: Updating M204Field ID: {m204_field_id} for project ID: {project_id}")
    db_m204_field = await get_m204_field_by_id(db, project_id, m204_field_id)
    if not db_m204_field:
        log.warning(f"MetadataService: M204Field with ID {m204_field_id} not found in project {project_id} for update.")
        return None

    update_values = update_data.model_dump(exclude_unset=True)
    if not update_values:
        log.info(f"MetadataService: No update values provided for M204Field ID: {m204_field_id}")
        return db_m204_field

    for key, value in update_values.items():
        setattr(db_m204_field, key, value)

    try:
        db.add(db_m204_field)
        db.commit()
        db.refresh(db_m204_field)
        log.info(f"MetadataService: M204Field ID: {m204_field_id} updated successfully.")
        return db_m204_field
    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"MetadataService: Database error updating M204Field ID {m204_field_id}: {e}", exc_info=True)
        raise

# --- Procedure related metadata ---
async def get_procedures_by_project(db: Session, project_id: int, skip: int = 0, limit: int = 100) -> List[Procedure]:
    log.info(f"MetadataService: Fetching procedures for project ID: {project_id} with skip: {skip}, limit: {limit}")
    procedures = db.query(Procedure)\
                   .filter(Procedure.project_id == project_id)\
                   .options(
                       selectinload(Procedure.input_source), # Added based on typical needs, can be removed if not used by schema
                       selectinload(Procedure.variables_in_procedure), # Eager load variables
                       selectinload(Procedure.calls_made).selectinload(ProcedureCall.resolved_procedure), # Corrected
                       selectinload(Procedure.calls_made).selectinload(ProcedureCall.calling_input_source), # To know where the call was made from
                       selectinload(Procedure.calls_received).selectinload(ProcedureCall.calling_procedure), # Corrected
                       selectinload(Procedure.calls_received).selectinload(ProcedureCall.calling_input_source) # To know where the call that was received originated
                   )\
                   .order_by(Procedure.m204_proc_name)\
                   .offset(skip)\
                   .limit(limit)\
                   .all()
    return procedures

async def count_procedures_for_project(db: Session, project_id: int) -> int:
    log.info(f"MetadataService: Counting procedures for project ID: {project_id}")
    return db.query(func.count(Procedure.proc_id))\
             .filter(Procedure.project_id == project_id)\
             .scalar() or 0

async def get_procedure_by_id(db: Session, project_id: int, procedure_id: int) -> Optional[Procedure]:
    log.info(f"MetadataService: Fetching Procedure by ID: {procedure_id} for project ID: {project_id}")
    procedure = db.query(Procedure).filter(
        Procedure.project_id == project_id,
        Procedure.proc_id == procedure_id
    ).options(
        selectinload(Procedure.input_source),
        selectinload(Procedure.variables_in_procedure),
        selectinload(Procedure.calls_made).selectinload(ProcedureCall.resolved_procedure),
        selectinload(Procedure.calls_made).selectinload(ProcedureCall.calling_input_source),
        selectinload(Procedure.calls_received).selectinload(ProcedureCall.calling_procedure),
        selectinload(Procedure.calls_received).selectinload(ProcedureCall.calling_input_source)
    ).first()
    return procedure

async def update_procedure_details(
    db: Session,
    project_id: int,
    procedure_id: int,
    update_data: M204ProcedureUpdateSchema
) -> Optional[Procedure]:
    log.info(f"MetadataService: Updating Procedure ID: {procedure_id} for project ID: {project_id}")
    db_procedure = await get_procedure_by_id(db, project_id, procedure_id)
    if not db_procedure:
        log.warning(f"MetadataService: Procedure with ID {procedure_id} not found in project {project_id} for update.")
        return None

    update_values = update_data.model_dump(exclude_unset=True)
    if not update_values:
        log.info(f"MetadataService: No update values provided for Procedure ID: {procedure_id}")
        return db_procedure

    for key, value in update_values.items():
        setattr(db_procedure, key, value)

    try:
        db.add(db_procedure)
        db.commit()
        db.refresh(db_procedure)
        log.info(f"MetadataService: Procedure ID: {procedure_id} updated successfully.")
        return db_procedure
    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"MetadataService: Database error updating Procedure ID {procedure_id}: {e}", exc_info=True)
        raise

# --- M204Variable related metadata ---
async def get_variables_by_project(db: Session, project_id: int, skip: int = 0, limit: int = 100) -> List[M204Variable]:
    log.info(f"MetadataService: Fetching M204 variables for project ID: {project_id} with skip: {skip}, limit: {limit}")
    variables = (
        db.query(M204Variable)
        .filter(M204Variable.project_id == project_id)
        .options(selectinload(M204Variable.procedure))  # Eager load the procedure for procedure_name property
        .order_by(M204Variable.variable_name, M204Variable.scope, M204Variable.procedure_id)
        .offset(skip)
        .limit(limit)
        .all()
    )
    return variables

async def count_variables_for_project(db: Session, project_id: int) -> int:
    log.info(f"MetadataService: Counting M204 variables for project ID: {project_id}")
    return db.query(func.count(M204Variable.variable_id))\
             .filter(M204Variable.project_id == project_id)\
             .scalar() or 0

async def get_m204_variable_by_id(db: Session, project_id: int, variable_id: int) -> Optional[M204Variable]:
    log.info(f"MetadataService: Fetching M204 variable by ID: {variable_id} for project ID: {project_id}")
    variable = db.query(M204Variable).filter(
        M204Variable.project_id == project_id,
        M204Variable.variable_id == variable_id
    ).options(selectinload(M204Variable.procedure)).first() # Eager load procedure
    return variable

async def update_m204_variable_details(
    db: Session,
    project_id: int,
    variable_id: int,
    update_data: M204VariableUpdateSchema
) -> Optional[M204Variable]:
    log.info(f"MetadataService: Updating M204Variable ID: {variable_id} for project ID: {project_id}")
    db_variable = await get_m204_variable_by_id(db, project_id, variable_id) # This now eager loads the procedure
    if not db_variable:
        log.warning(f"MetadataService: M204Variable with ID {variable_id} not found in project {project_id} for update.")
        return None

    update_values = update_data.model_dump(exclude_unset=True)
    if not update_values:
        log.info(f"MetadataService: No update values provided for M204Variable ID: {variable_id}")
        return db_variable

    for key, value in update_values.items():
        setattr(db_variable, key, value)

    try:
        db.add(db_variable)
        db.commit()
        db.refresh(db_variable)
        log.info(f"MetadataService: M204Variable ID: {variable_id} updated successfully.")
        return db_variable
    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"MetadataService: Database error updating M204Variable ID {variable_id}: {e}", exc_info=True)
        raise