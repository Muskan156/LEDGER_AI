"""
backend/auth/routes.py
POST /auth/register  – create account (email + password)
POST /auth/login     – returns JWT access_token
"""
from fastapi import APIRouter, HTTPException, status
from fastapi.security import OAuth2PasswordRequestForm
from fastapi import Depends
from pydantic import BaseModel, EmailStr
import sys, os

sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from auth.utils import (
    hash_password, create_access_token,
    get_user_by_email, create_user, create_session,
)

router = APIRouter()


# ── Schemas ────────────────────────────────────────────────

class UserCreate(BaseModel):
    email: EmailStr
    password: str


class TokenResponse(BaseModel):
    access_token: str
    token_type: str = "bearer"


# ── Register ───────────────────────────────────────────────

@router.post("/register", status_code=201)
def register(body: UserCreate):
    if len(body.password) < 8:
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail="Password must be at least 8 characters.",
        )
    existing = get_user_by_email(body.email)
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="An account with this email already exists.",
        )
    try:
        user_id = create_user(body.email, body.password)
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Registration failed: {e}",
        )
    return {"message": "Account created successfully.", "user_id": user_id}


# ── Login ──────────────────────────────────────────────────

@router.post("/login", response_model=TokenResponse)
def login(form_data: OAuth2PasswordRequestForm = Depends()):
    user = get_user_by_email(form_data.username)   # username = email field
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid email or password.",
        )
    stored_hash = user["password_hash"]
    given_pw = form_data.password
    
    # Check if it's a SHA-256 hash (64 chars hex) or plain text
    is_sha256 = len(stored_hash) == 64 and all(c in "0123456789abcdef" for c in stored_hash.lower())
    
    if is_sha256:
        if stored_hash != hash_password(given_pw):
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid email or password.",
            )
    else:
        # Legacy/Seed plain text check
        if stored_hash != given_pw:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid email or password.",
            )
    # Create DB session + issue JWT
    create_session(user["user_id"])
    access_token = create_access_token(user["user_id"])
    return {"access_token": access_token, "token_type": "bearer"}
