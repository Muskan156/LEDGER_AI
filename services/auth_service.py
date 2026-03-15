"""
services/auth_service.py
────────────────────────
User authentication and session management.
"""

import bcrypt
import uuid
import logging
from datetime import datetime, timedelta
from db.connection import get_cursor

logger = logging.getLogger("ledgerai.auth_service")


def hash_password(password: str) -> str:
    return bcrypt.hashpw(password.encode(), bcrypt.gensalt()).decode()


def verify_password(password: str, hashed: str) -> bool:
    return bcrypt.checkpw(password.encode(), hashed.encode())


def register_user(email: str, password: str) -> int:
    pw_hash = hash_password(password)
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute(
            "INSERT INTO users (email, password_hash) VALUES (%s, %s)",
            (email, pw_hash),
        )
        user_id = cursor.lastrowid
    logger.info("Registered user_id=%s email=%s", user_id, email)
    return user_id


def login_user(email: str, password: str):
    """Returns (user_id, token) on success, None on failure."""
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute(
            "SELECT user_id, password_hash FROM users WHERE email=%s AND status='ACTIVE'",
            (email,),
        )
        user = cursor.fetchone()

    if not user:
        return None

    if not verify_password(password, user["password_hash"]):
        return None

    token = str(uuid.uuid4())
    expires_at = datetime.now() + timedelta(hours=12)

    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute(
            "INSERT INTO user_sessions (user_id, token, expires_at) VALUES (%s, %s, %s)",
            (user["user_id"], token, expires_at),
        )

    logger.info("Login: user_id=%s", user["user_id"])
    return user["user_id"], token
