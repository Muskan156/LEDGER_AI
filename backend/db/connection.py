"""
backend/db/connection.py
MySQL connection pool – mirrors app.py's `get_cursor` pattern.
"""
import mysql.connector
from mysql.connector import pooling
from contextlib import contextmanager
import logging
import sys, os

sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from config import DB_CONFIG

logger = logging.getLogger("ledgerai.db")
_pool = None


def _get_pool():
    global _pool
    if _pool is None:
        _pool = pooling.MySQLConnectionPool(
            pool_name="ledgerai_pool",
            pool_size=10,
            pool_reset_session=True,
            **DB_CONFIG,
        )
        logger.info("DB pool created.")
    return _pool


@contextmanager
def get_cursor(dictionary=False, commit=False, prepared=False):
    conn = cursor = None
    try:
        conn   = _get_pool().get_connection()
        cursor = conn.cursor(dictionary=dictionary, prepared=prepared)
        yield conn, cursor
        if commit:
            conn.commit()
    except Exception:
        if conn and conn.is_connected():
            conn.rollback()
        raise
    finally:
        if cursor:
            try: cursor.close()
            except: pass
        if conn and conn.is_connected():
            try: conn.close()
            except: pass
