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
    force=True,
)

logger = logging.getLogger("ledgerai.main")

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from auth.routes import router as auth_router
from api.document_routes import router as document_router

app = FastAPI(title="LedgerAI API", version="1.0.0")

# ── CORS ─────────────────────────────────────────────────────
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ── Routers ───────────────────────────────────────────────────
app.include_router(auth_router, prefix="/auth", tags=["Auth"])
app.include_router(document_router, prefix="/documents", tags=["Documents"])


# ── Startup config validation ─────────────────────────────────
@app.on_event("startup")
async def validate_config():
    """Warn loudly at startup if Supabase env vars are missing or still placeholders."""
    from config import SUPABASE_URL, SUPABASE_ANON_KEY, SUPABASE_SERVICE_ROLE_KEY
    bad = {"", "your-anon-public-key-here", "your-service-role-secret-key-here"}
    issues = []
    if not SUPABASE_URL or "your-project-ref" in SUPABASE_URL:
        issues.append("SUPABASE_URL")
    if not SUPABASE_ANON_KEY or SUPABASE_ANON_KEY in bad:
        issues.append("SUPABASE_ANON_KEY")
    if not SUPABASE_SERVICE_ROLE_KEY or SUPABASE_SERVICE_ROLE_KEY in bad:
        issues.append("SUPABASE_SERVICE_ROLE_KEY")

    if issues:
        logger.error("=" * 65)
        logger.error("  MISSING / PLACEHOLDER SUPABASE CONFIG DETECTED")
        logger.error("  The following .env values must be set:")
        for v in issues:
            logger.error("    ❌  %s", v)
        logger.error("  Get values from: Supabase Dashboard → Settings → API")
        logger.error("  Auth and DB calls WILL FAIL until these are filled in.")
        logger.error("=" * 65)
    else:
        logger.info("✅ Supabase config OK — all credentials present.")


@app.get("/health")
def health():
    from config import SUPABASE_URL, SUPABASE_ANON_KEY, SUPABASE_SERVICE_ROLE_KEY
    configured = all([
        SUPABASE_URL and "your-project-ref" not in SUPABASE_URL,
        SUPABASE_ANON_KEY and len(SUPABASE_ANON_KEY) > 20,
        SUPABASE_SERVICE_ROLE_KEY and len(SUPABASE_SERVICE_ROLE_KEY) > 20,
    ])
    return {"status": "ok", "supabase_configured": configured}
