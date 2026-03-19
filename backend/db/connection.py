"""
backend/db/connection.py
────────────────────────
Supabase client singleton for database access (backend process).

Uses SUPABASE_SERVICE_ROLE_KEY so all server-side operations
bypass Row Level Security (RLS) — correct for backend services.
"""

from supabase import create_client, Client
import logging
import sys, os

sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from config import SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY

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
