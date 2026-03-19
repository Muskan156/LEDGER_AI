"""
db/connection.py
────────────────
Supabase client singleton for database access.

Uses SUPABASE_SERVICE_ROLE_KEY so all server-side operations
bypass Row Level Security (RLS) — correct for backend services.

Usage (anywhere in the codebase):
    from db.connection import get_client
    sb = get_client()
    result = sb.table("documents").select("*").eq("document_id", 1).execute()
    rows = result.data  # list[dict]
"""

from supabase import create_client, Client
from config import SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY
import logging

logger = logging.getLogger("ledgerai.db")

_client: Client | None = None


def get_client() -> Client:
    """Return the singleton Supabase service-role client."""
    global _client
    if _client is None:
        if not SUPABASE_URL or not SUPABASE_SERVICE_ROLE_KEY:
            raise RuntimeError(
                "SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY must be set in .env"
            )
        _client = create_client(SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY)
        logger.info("Supabase service-role client initialised.")
    return _client
