from fastapi import FastAPI, Request, APIRouter
from fastapi.middleware.cors import CORSMiddleware
from app.config.db_config import Base, engine 
from app.config.app_config import settings as app_settings 
from app.utils.logger import log
from app.routers import project_router, file_router,analysis_router,metadata_router,requirement_document_router,artifacts_router
from app.services.rag_service import RagService # Import the RagService class
from app.services.analysis_service import set_global_rag_service # Import the setter function
import asyncio # Import asyncio

app = FastAPI(
    title=app_settings.APP_NAME,
    version=app_settings.APP_VERSION,
    description="A FastAPI application for converting M204 to Cobol+VSAM.",
    debug=app_settings.DEBUG,
)


# Middleware to log requests
@app.middleware("http")
async def log_requests_middleware(request: Request, call_next):
    path_for_log = request.url.path
    if request.scope.get("root_path"):
         path_for_log = f"{request.scope['root_path']}{path_for_log}"

    log.info(f"Incoming request: {request.method} {path_for_log} - Client: {request.client.host}")
    response = await call_next(request)
    log.info(f"Outgoing response: {request.method} {path_for_log} - Status: {response.status_code}")
    return response


# Create database tables and initialize RAG service on startup
@app.on_event("startup")
async def startup_event():
    log.info(f"Application startup: {app_settings.APP_NAME} v{app_settings.APP_VERSION}")
    log.info(f"Environment: {app_settings.ENV}, Debug mode: {app_settings.DEBUG}")
    log.info("Creating database tables (if they don't exist)...")
    # Base.metadata.create_all is synchronous, run in a thread if it's slow
    await asyncio.to_thread(Base.metadata.create_all, bind=engine)
    log.info("Database tables checked/created.")

    log.info("Initializing RAG service...")
    try:
        # Create the RAG service instance
        rag_instance = RagService() 
        # Initialize the RAG service asynchronously
        await rag_instance.initialize()
        
        # Store it on app.state for potential direct access if needed elsewhere (optional)
        app.state.rag_service = rag_instance
        # Pass the instance to analysis_service
        set_global_rag_service(rag_instance) 
        
        if rag_instance.is_ready(): # Use the new is_ready() method
            log.info(f"RAG service initialized successfully in main.py. Loaded nodes: {rag_instance.get_loaded_documents_count()}")
        else:
            log.error("RAG service initialization failed or not ready after startup. Check RAG service logs.")
    except Exception as e:
        log.error(f"Error initializing RAG service during startup: {e}", exc_info=True)


origins=[
    "http://localhost:5173",
    "http://localhost:5174",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health")
async def get_health_status():
    log.info("Health check endpoint '/health' called.")
    rag_status = "RAG service instance not found in app state." # Default if rag_service not on app.state
    if hasattr(app.state, 'rag_service') and isinstance(app.state.rag_service, RagService):
        rag_instance: RagService = app.state.rag_service
        if rag_instance.is_ready():
            rag_status = f"RAG service ready. Documents: {rag_instance.get_loaded_documents_count()}"
        elif rag_instance._is_initializing: # Accessing internal state for more detail
            rag_status = "RAG service initialization in progress."
        elif rag_instance._initialization_failed: # Accessing internal state
            rag_status = "RAG service initialization failed."
        elif not rag_instance._initialized_event.is_set(): # Check if event is not set and not initializing
            rag_status = "RAG service initialization not yet complete or not started."
        elif rag_instance._index is None: # Event might be set, not failed, but index is still None
            rag_status = "RAG service initialized but index is missing (unexpected)."
        else:
            # This case should ideally not be reached if the above cover all states
            rag_status = "RAG service state unknown but not explicitly failed or initializing."
            
    return {"status":"OK", "rag_service_status": rag_status}


# Router for API version 1
api_v1_router = APIRouter(prefix="/api/v1")
api_v1_router.include_router(project_router.router)
api_v1_router.include_router(file_router.router) 
api_v1_router.include_router(analysis_router.router)
api_v1_router.include_router(metadata_router.router)
api_v1_router.include_router(requirement_document_router.router)
api_v1_router.include_router(artifacts_router.router)


app.include_router(api_v1_router)