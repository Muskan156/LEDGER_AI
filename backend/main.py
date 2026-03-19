"""
backend/main.py
LedgerAI FastAPI entry point.
Run with:  python -m uvicorn main:app --reload --port 8000
"""
import logging
import os
import sys

# ── Add project root to sys.path so services/ is importable from backend/ ──
_project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _project_root not in sys.path:
    sys.path.append(_project_root)


# ── Configure logging BEFORE any imports that create loggers ──
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(name)-35s  %(levelname)-7s  %(message)s",
    handlers=[logging.StreamHandler(sys.stdout)],
    force=True,  # Override any existing configuration
)

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from auth.routes import router as auth_router
from api.document_routes import router as document_router

app = FastAPI(title="LedgerAI API", version="1.0.0")

# ── CORS (allow all localhost ports for Vite dev) ───────────
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:5173",
        "http://localhost:5174",
        "http://localhost:5175",
        "http://127.0.0.1:5173",
        "http://127.0.0.1:5174",
        "http://127.0.0.1:5175",
        "http://localhost:3000",
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ── Routers ────────────────────────────────────────────────
app.include_router(auth_router, prefix="/auth", tags=["Auth"])
app.include_router(document_router, prefix="/documents", tags=["Documents"])


@app.get("/health")
def health():
    return {"status": "ok"}
