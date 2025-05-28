from fastapi import FastAPI, Request, APIRouter
from fastapi.middleware.cors import CORSMiddleware
from app.config.db_config import Base, engine 
from app.config.app_config import settings as app_settings 
from app.utils.logger import log
from app.routers import project_router, file_router,analysis_router,metadata_router,requirement_document_router,artifacts_router
from app.services.rag_service import RagService, set_global_rag_service, get_global_rag_service
import asyncio 
from contextlib import asynccontextmanager

@asynccontextmanager
async def lifespan(app_instance: FastAPI): # Renamed 'app' to 'app_instance' to avoid conflict
    # Startup logic
    log.info(f"Application startup: {app_settings.APP_NAME} v{app_settings.APP_VERSION}")
    log.info(f"Environment: {app_settings.ENV}, Debug mode: {app_settings.DEBUG}")
    log.info("Creating database tables (if they don't exist)...")
    await asyncio.to_thread(Base.metadata.create_all, bind=engine)
    log.info("Database tables checked/created.")

    log.info("Initializing RAG service...")
    rag_instance = None  # Initialize to None
    try:
        rag_instance = RagService() 
        await rag_instance.initialize()
        
        # Store it on app.state for potential direct access if needed elsewhere (optional)
        app_instance.state.rag_service = rag_instance # Use app_instance here
        # Set the global instance
        set_global_rag_service(rag_instance) 
        
        if rag_instance.is_ready():
            log.info(f"RAG service initialized successfully. Loaded documents: {rag_instance.get_loaded_documents_count()}")
        else:
            log.error("RAG service initialization failed or not ready after startup. Check RAG service logs.")
    except Exception as e:
        log.error(f"Error initializing RAG service during startup: {e}", exc_info=True)
        # Ensure global service is None if initialization fails critically before setting
        if rag_instance and not get_global_rag_service():
             set_global_rag_service(rag_instance) # It might have been partially set up
    
    yield
    # Shutdown logic (if any) can go here
    log.info("Application shutdown.")

app = FastAPI(
    title=app_settings.APP_NAME,
    version=app_settings.APP_VERSION,
    description="A FastAPI application for converting M204 to Cobol+VSAM.",
    debug=app_settings.DEBUG,
    lifespan=lifespan # Use the lifespan context manager
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
    rag_instance = get_global_rag_service() # Use the global getter
    
    if rag_instance:
        if rag_instance.is_ready():
            rag_status = f"RAG service ready. Documents: {rag_instance.get_loaded_documents_count()}"
        elif hasattr(rag_instance, '_is_initializing') and rag_instance._is_initializing:
            rag_status = "RAG service initialization in progress."
        elif hasattr(rag_instance, '_initialization_failed') and rag_instance._initialization_failed:
            rag_status = "RAG service initialization failed."
        elif hasattr(rag_instance, '_initialized_event') and not rag_instance._initialized_event.is_set():
            rag_status = "RAG service initialization not yet complete or not started."
        elif hasattr(rag_instance, '_index') and rag_instance._index is None and rag_instance._initialized_event.is_set() and not rag_instance._initialization_failed :
             rag_status = "RAG service initialized but index is missing (unexpected)."
        else:
            rag_status = "RAG service state unknown but not explicitly failed or initializing."
    else:
        rag_status = "RAG service global instance not available."
            
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