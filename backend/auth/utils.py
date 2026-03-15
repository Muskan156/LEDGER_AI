"""
backend/auth/utils.py
Mirrors app.py's auth helpers exactly:
  - SHA-256 password hashing
  - JWT token creation / decoding
  - Looks up `password_hash` column, respects `status='ACTIVE'`
"""
import hashlib
import datetime
import uuid
from fastapi import Depends, HTTPException, status
from fastapi.security import OAuth2PasswordBearer
from jose import jwt, JWTError
import sys, os

sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from config import JWT_SECRET, JWT_ALGORITHM, JWT_EXPIRE_HOURS
from db.connection import get_cursor

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="auth/login")


def hash_password(password: str) -> str:
    """SHA-256 – matches app.py exactly."""
    return hashlib.sha256(password.encode()).hexdigest()


def create_access_token(user_id: int) -> str:
    payload = {
        "user_id": user_id,
        "exp": datetime.datetime.utcnow() + datetime.timedelta(hours=JWT_EXPIRE_HOURS),
    }
    return jwt.encode(payload, JWT_SECRET, algorithm=JWT_ALGORITHM)  # jose returns str directly


def decode_token(token: str) -> dict:
    return jwt.decode(token, JWT_SECRET, algorithms=[JWT_ALGORITHM])  # jose


def get_user_by_email(email: str) -> dict | None:
    with get_cursor(dictionary=True) as (_, cursor):
        cursor.execute(
            "SELECT * FROM users WHERE email=%s AND status='ACTIVE'",
            (email,),
        )
        return cursor.fetchone()


def create_user(email: str, password: str) -> int:
    pw_hash = hash_password(password)
    with get_cursor(commit=True) as (_, cursor):
        cursor.execute(
            "INSERT INTO users (email, password_hash) VALUES (%s, %s)",
            (email, pw_hash),
        )
        return cursor.lastrowid


def create_session(user_id: int) -> str:
    token = str(uuid.uuid4())
    expires_at = datetime.datetime.now() + datetime.timedelta(hours=JWT_EXPIRE_HOURS)
    with get_cursor(commit=True) as (_, cursor):
        cursor.execute(
            "INSERT INTO user_sessions (user_id, token, expires_at) VALUES (%s, %s, %s)",
            (user_id, token, expires_at),
        )
    return token

def get_current_user(token: str = Depends(oauth2_scheme)):
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )
    try:
        payload = decode_token(token)
        user_id: int = payload.get("user_id")
        if user_id is None:
            raise credentials_exception
        return {"user_id": user_id}
    except JWTError:
        raise credentials_exception
