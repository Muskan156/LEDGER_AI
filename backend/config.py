# backend/config.py
import os
from pathlib import Path
from dotenv import load_dotenv

# Absolute path to the .env in project root
dotenv_path = Path(__file__).parent.parent / ".env"
load_dotenv(dotenv_path)

DB_CONFIG = {
    "host":     os.environ.get("DB_HOST", "localhost"),
    "user":     os.environ.get("DB_USER", "root"),
    "password": os.environ.get("DB_PASSWORD", ""),
    "database": os.environ.get("DB_NAME", "ledger_db"),
}

# Ensure secret is consistent
JWT_SECRET  = os.environ.get("JWT_SECRET", "ledgerai_secret_key_change_me")
JWT_ALGORITHM = "HS256"
JWT_EXPIRE_HOURS = 12

GEMINI_API_KEY = os.environ.get("GEMINI_API_KEY")
GEMINI_MODEL_NAME = os.environ.get("GEMINI_MODEL_NAME", "gemini-1.5-flash")

