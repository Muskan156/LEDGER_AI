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


OPENROUTER_API_KEY = os.getenv("OPENROUTER_API_KEY")
OPENROUTER_URL = "https://openrouter.ai/api/v1/chat/completions"
# FIX: Use os.getenv to allow overriding, and fix the default model ID (dot instead of hyphen)
OPENROUTER_MODEL_NAME = "anthropic/claude-3.5-sonnet"
DEFAULT_MAX_TOKENS = 4096