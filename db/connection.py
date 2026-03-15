"""
db/connection.py
────────────────
Centralised database connection management with pooling.
"""

import mysql.connector
from mysql.connector import pooling
from contextlib import contextmanager
from config import DB_CONFIG
import logging

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
        logger.info("Database connection pool created (size=10).")
    return _pool


@contextmanager
def get_cursor(dictionary=False, commit=False, prepared=False):
    """
    Yields (conn, cursor). Auto-commits on success if commit=True,
    rolls back on error, and always returns connection to pool.
    """
    conn = None
    cursor = None
    try:
        conn = _get_pool().get_connection()
        cursor = conn.cursor(dictionary=dictionary, prepared=prepared)
        yield conn, cursor
        if commit:
            conn.commit()
    except Exception:
        if conn and conn.is_connected():
            try:
                conn.rollback()
            except Exception:
                pass
        raise
    finally:
        if cursor:
            try:
                cursor.close()
            except Exception:
                pass
        if conn and conn.is_connected():
            try:
                conn.close()
            except Exception:
                pass


def get_connection():
    """Backward-compatible. Prefer get_cursor()."""
    return _get_pool().get_connection()
