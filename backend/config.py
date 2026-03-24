# backend/config.py
import os
from pathlib import Path
from dotenv import load_dotenv

# Absolute path to the .env in project root
dotenv_path = Path(__file__).parent.parent / ".env"
load_dotenv(dotenv_path)

# ── Supabase credentials ─────────────────────────────────────
SUPABASE_URL              = os.environ.get("SUPABASE_URL", "")
SUPABASE_ANON_KEY         = os.environ.get("SUPABASE_ANON_KEY", "")
SUPABASE_SERVICE_ROLE_KEY = os.environ.get("SUPABASE_SERVICE_ROLE_KEY", "")

# GEMINI_API_KEY    = os.environ.get("GEMINI_API_KEY")
# GEMINI_MODEL_NAME = os.environ.get("GEMINI_MODEL_NAME", "gemini-1.5-flash")
# ── Anthropic credentials ────────────────────────────────────
ANTHROPIC_API_KEY    = os.environ.get("ANTHROPIC_API_KEY")
ANTHROPIC_MODEL_NAME = os.environ.get("ANTHROPIC_MODEL_NAME", "claude-sonnet-4-20250514")
