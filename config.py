# config.py  (project root — used by services/ and repository/)
import os
from dotenv import load_dotenv

load_dotenv(override=True)

# ── Supabase credentials ────────────────────────────────────
SUPABASE_URL             = os.environ.get("SUPABASE_URL", "")
SUPABASE_ANON_KEY        = os.environ.get("SUPABASE_ANON_KEY", "")
SUPABASE_SERVICE_ROLE_KEY = os.environ.get("SUPABASE_SERVICE_ROLE_KEY", "")

# Gemini AI API ───────────────────────────────────────────────
GEMINI_API_KEY    = os.environ.get("GEMINI_API_KEY", "")
GEMINI_MODEL_NAME = os.environ.get("GEMINI_MODEL_NAME", "models/gemini-2.5-flash")

