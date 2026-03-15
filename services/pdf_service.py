"""
services/pdf_service.py
───────────────────────
Extract text from PDF files page by page using pdfplumber.
"""

import logging
from typing import List

import pdfplumber

logger = logging.getLogger("ledgerai.pdf_service")


def extract_pages(pdf_path: str, password: str = None) -> List[str]:
    """Returns list of text strings, one per page."""
    logger.info("Extracting pages: %s (password=%s)", pdf_path, bool(password))

    pages = []
    try:
        with pdfplumber.open(pdf_path, password=password) as pdf:
            for page in pdf.pages:
                text = page.extract_text() or ""
                pages.append(text)
    except Exception as exc:
        logger.error("PDF extraction failed: %s", exc, exc_info=True)
        raise

    total_chars = sum(len(p) for p in pages)
    logger.info("Extracted %d pages, %d chars.", len(pages), total_chars)
    return pages