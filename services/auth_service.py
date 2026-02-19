import bcrypt
import uuid
from datetime import datetime, timedelta
from db.connection import get_connection


# =====================================================
# REGISTER USER
# =====================================================
def register_user(email: str, password: str):
    conn = get_connection()
    cursor = conn.cursor()

    # Hash password
    password_hash = bcrypt.hashpw(password.encode(), bcrypt.gensalt()).decode()

    query = """
        INSERT INTO users (email, password_hash)
        VALUES (%s, %s)
    """

    cursor.execute(query, (email, password_hash))
    conn.commit()

    user_id = cursor.lastrowid

    cursor.close()
    conn.close()

    return user_id


# =====================================================
# LOGIN USER
# =====================================================
def login_user(email: str, password: str):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT * FROM users
        WHERE email = %s AND status = 'ACTIVE'
    """

    cursor.execute(query, (email,))
    user = cursor.fetchone()

    if not user:
        return None

    stored_hash = user["password_hash"]

    if not bcrypt.checkpw(password.encode(), stored_hash.encode()):
        return None

    # Create session token
    token = str(uuid.uuid4())
    expiry = datetime.utcnow() + timedelta(hours=8)

    insert_session = """
        INSERT INTO user_sessions (user_id, token, expires_at)
        VALUES (%s, %s, %s)
    """

    cursor.execute(insert_session, (user["user_id"], token, expiry))
    conn.commit()

    cursor.close()
    conn.close()

    return {
        "user_id": user["user_id"],
        "token": token
    }


# =====================================================
# VALIDATE SESSION
# =====================================================
def validate_session(token: str):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT * FROM user_sessions
        WHERE token = %s AND expires_at > NOW()
    """

    cursor.execute(query, (token,))
    session = cursor.fetchone()

    cursor.close()
    conn.close()

    return session


# =====================================================
# LOGOUT
# =====================================================
def logout_user(token: str):
    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute(
        "DELETE FROM user_sessions WHERE token = %s",
        (token,)
    )
    conn.commit()

    cursor.close()
    conn.close()
