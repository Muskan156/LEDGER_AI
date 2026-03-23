# config.py  (project root — used by services/ and repository/)
import os
from dotenv import load_dotenv

load_dotenv(override=True)

# ── Supabase credentials ────────────────────────────────────
SUPABASE_URL             = os.environ.get("SUPABASE_URL", "")
SUPABASE_ANON_KEY        = os.environ.get("SUPABASE_ANON_KEY", "")
SUPABASE_SERVICE_ROLE_KEY = os.environ.get("SUPABASE_SERVICE_ROLE_KEY", "")

# ── Anthropic credentials ────────────────────────────────────
ANTHROPIC_API_KEY    = os.environ.get("ANTHROPIC_API_KEY")
ANTHROPIC_MODEL_NAME = os.environ.get("ANTHROPIC_MODEL_NAME", "claude-sonnet-4-20250514")