import os
import shutil
import asyncio # Added for async operations
from llama_index.core import (
    VectorStoreIndex,
    SimpleDirectoryReader,
    Settings,
    StorageContext,
    load_index_from_storage
)
from llama_index.core.node_parser import SentenceSplitter
from llama_index.core.base.response.schema import RESPONSE_TYPE
from app.config.llm_config import llm_config
from app.utils.logger import log
from typing import Optional

APP_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_DIR = os.path.join(APP_ROOT, "data")
PERSIST_DIR = os.path.join(APP_ROOT, "rag_index_persist")


def set_global_rag_service(rag_service: 'RagService'): # Forward reference RagService
        global _rag_service_instance
        _rag_service_instance = rag_service
        log.info(f"Global RAG service instance set in rag_service: {type(rag_service)}")

def get_global_rag_service() -> Optional['RagService']: # Optional: a getter can be useful
        return _rag_service_instance

class RagService:
    _instance = None
    _index: Optional[VectorStoreIndex] = None
    _initialization_lock = asyncio.Lock()
    _initialized_event = asyncio.Event()
    _initialization_failed: bool = False
    _is_initializing: bool = False # To prevent re-entrant calls to initialize()

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = super(RagService, cls).__new__(cls)
            # Initialization is now explicitly called via async initialize()
        return cls._instance

    async def _load_index_from_storage_async(self) -> bool:
        """Asynchronously loads the index from storage."""
        log.info(f"Attempting to load RAG index from persisted storage: {PERSIST_DIR}")
        try:
            storage_context = await asyncio.to_thread(
                StorageContext.from_defaults, persist_dir=PERSIST_DIR
            )
            # Ensure LLM and embed_model are set in Settings before loading
            if Settings.llm is None or Settings.embed_model is None:
                log.info("LLM/Embed Model not set before loading index. Initializing from llm_config.")
                await asyncio.to_thread(llm_config.init_llamaindex)

            self._index = await asyncio.to_thread(
                load_index_from_storage, storage_context
            )
            log.info(f"RAG index loaded successfully from {PERSIST_DIR}.")
            return True
        except Exception as e:
            log.warning(f"Failed to load RAG index from {PERSIST_DIR}: {e}. Will attempt to rebuild.", exc_info=True)
            self._index = None
            return False

    async def _build_and_persist_index_async(self) -> bool:
        """Asynchronously builds the index from documents and persists it."""
        log.info(f"Attempting to build RAG index from source documents in: {DATA_DIR}")
        
        data_dir_exists = await asyncio.to_thread(os.path.exists, DATA_DIR)
        if not data_dir_exists:
            log.error(f"Data directory '{DATA_DIR}' not found. RAG service cannot load documents to build index.")
            return False

        try:
            # Check if DATA_DIR is empty using a thread
            def _is_dir_empty_sync(directory):
                return not any(os.scandir(directory))
            
            is_data_dir_empty = await asyncio.to_thread(_is_dir_empty_sync, DATA_DIR)
            if is_data_dir_empty:
                log.warning(f"Data directory '{DATA_DIR}' is empty. Cannot build RAG index.")
                return False

            def _load_documents_sync():
                return SimpleDirectoryReader(DATA_DIR).load_data()
            
            documents = await asyncio.to_thread(_load_documents_sync)

            if not documents:
                log.warning(f"No documents successfully loaded from {DATA_DIR}. RAG index will be empty.")
                return False

            node_parser = SentenceSplitter(chunk_size=512, chunk_overlap=20)
            
            # Ensure LLM and embed_model are set in Settings before building
            if Settings.llm is None or Settings.embed_model is None:
                log.info("LLM/Embed Model not set before building index. Initializing from llm_config.")
                await asyncio.to_thread(llm_config.init_llamaindex)
            
            def _build_index_sync():
                return VectorStoreIndex.from_documents(
                    documents,
                    transformations=[node_parser]
                )
            
            self._index = await asyncio.to_thread(_build_index_sync)
            log.info(f"RAG index built successfully with {len(documents)} documents.")

            await asyncio.to_thread(os.makedirs, PERSIST_DIR, exist_ok=True)
            
            def _persist_index_sync():
                if self._index: # Ensure index is not None
                    self._index.storage_context.persist(persist_dir=PERSIST_DIR)

            await asyncio.to_thread(_persist_index_sync)
            log.info(f"Newly built RAG index persisted to {PERSIST_DIR}.")
            return True

        except Exception as ex:
            log.error(f"Error building and persisting RAG index: {ex}", exc_info=True)
            self._index = None
            return False

    async def initialize(self, force_rebuild: bool = False):
        """
        Initializes the RAG service by loading from persistence or building the index.
        This method is idempotent and ensures initialization happens only once unless forced.
        """
        async with self._initialization_lock:
            if self._initialized_event.is_set() and not force_rebuild:
                log.info("RAG index already initialized and ready.")
                return
            if self._is_initializing and not force_rebuild:
                log.info("RAG index initialization already in progress. Waiting for completion...")
                await self._initialized_event.wait()
                log.info(f"Waited for ongoing RAG initialization. Ready: {self.is_ready()}")
                return

            self._is_initializing = True
            self._initialized_event.clear()
            self._initialization_failed = False
            self._index = None 

            log.info(f"Initializing RAG Service (force_rebuild={force_rebuild})...")

            # Initialize LlamaIndex settings (LLM and embed_model) first
            # This is crucial before any index operations (load or build)
            if Settings.llm is None or Settings.embed_model is None:
                log.info("LLM or Embed Model not set in LlamaIndex Settings. Initializing from llm_config.")
                await asyncio.to_thread(llm_config.init_llamaindex)
            else:
                log.info("LLM and Embed Model already set in LlamaIndex Settings.")


            if force_rebuild:
                persist_dir_exists = await asyncio.to_thread(os.path.exists, PERSIST_DIR)
                if persist_dir_exists:
                    log.info(f"Force rebuild requested. Removing existing persisted index at {PERSIST_DIR}")
                    try:
                        await asyncio.to_thread(shutil.rmtree, PERSIST_DIR)
                    except Exception as e:
                        log.error(f"Error removing persisted directory {PERSIST_DIR} for rebuild: {e}", exc_info=True)
            
            success = False
            if not force_rebuild:
                persist_dir_exists = await asyncio.to_thread(os.path.exists, PERSIST_DIR)
                if persist_dir_exists:
                    success = await self._load_index_from_storage_async()
            
            if not success: 
                success = await self._build_and_persist_index_async()

            if success and self._index is not None:
                log.info("RAG Service initialization process completed successfully.")
                self._initialization_failed = False
            else:
                log.error("RAG Service initialization process failed.")
                self._initialization_failed = True
                self._index = None

            self._initialized_event.set()
            self._is_initializing = False
            log.info(f"RAG Service initialization attempt finished. Ready: {self.is_ready()}")


    async def _ensure_initialized(self):
        """Waits for initialization to complete if it hasn't already and checks status."""
        if not self._initialized_event.is_set():
            log.warning("RAG service accessed before initialization event was set. Waiting...")
            # This should ideally not happen if initialize() is called and awaited at startup.
            # However, as a fallback, wait for the event.
            # A timeout could be added here if necessary.
            await self._initialized_event.wait()
            log.info(f"Initialization event set after waiting. Ready: {self.is_ready()}")


        if self._initialization_failed:
            log.error("RAG index initialization previously failed.")
            raise RuntimeError("RAG Service is not available due to a prior initialization failure.")
        if self._index is None:
            log.error("RAG index is None after initialization. This indicates a problem.")
            raise RuntimeError("RAG Service is not available (index is None after initialization).")


    async def query(self, query_text: str, similarity_top_k: int = 3) -> Optional[str]:
        await self._ensure_initialized()
        
        log.info(f"Querying RAG index with text: '{query_text}' (sync wrapper)")
        try:
            query_engine = self._index.as_query_engine(
                similarity_top_k=similarity_top_k,
            )
            
            response = await asyncio.to_thread(query_engine.query, query_text)
            
            response_text = str(response.response) if hasattr(response, 'response') else str(response)
            log.info(f"RAG query response: {response_text}")
            return response_text
        except RuntimeError as r_err: 
            log.error(f"Cannot perform RAG query due to initialization issue: {r_err}")
            return f"RAG Service not available: {r_err}"
        except Exception as e:
            log.error(f"Error during RAG query: {e}", exc_info=True)
            return f"Error performing RAG query: {e}"

    async def aquery(self, query_text: str, similarity_top_k: int = 3) -> Optional[str]:
        await self._ensure_initialized()

        log.info(f"Asynchronously querying RAG index with text: '{query_text}'")
        try:
            query_engine = self._index.as_query_engine(
                similarity_top_k=similarity_top_k,
            )
            response: RESPONSE_TYPE = await query_engine.aquery(query_text)
            
            response_text = str(response.response) if hasattr(response, 'response') else str(response)
            log.info(f"Async RAG query response: {response_text}")
            return response_text
        except RuntimeError as r_err: 
            log.error(f"Cannot perform async RAG query due to initialization issue: {r_err}")
            return f"RAG Service not available: {r_err}"
        except Exception as e:
            log.error(f"Error during async RAG query: {e}", exc_info=True)
            return f"Error performing async RAG query: {e}"

    async def rebuild_index(self):
        log.info(f"Force rebuilding RAG index. Will build from {DATA_DIR} and overwrite {PERSIST_DIR}.")
        await self.initialize(force_rebuild=True)
        if self.is_ready():
            log.info("RAG index rebuilt and persisted successfully.")
        else:
            log.error("Failed to rebuild RAG index. Check data directory and logs.")

    def get_loaded_documents_count(self) -> int:
        if self.is_ready() and self._index and self._index.docstore:
            try:
                return len(self._index.docstore.docs)
            except AttributeError: 
                log.warning("Could not get document count directly from docstore.docs.")
        return 0


    def is_ready(self) -> bool:
        """Checks if the RAG service is initialized successfully and ready to be used."""
        return self._initialized_event.is_set() and not self._initialization_failed and self._index is not None
