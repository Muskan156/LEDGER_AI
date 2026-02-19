import pdfplumber
from typing import List

def extract_pages(pdf_path: str, password: str = None) -> List[str]:
    """Extract text from all pages of PDF"""
    pages = []
    with pdfplumber.open(pdf_path, password=password) as pdf:
        for page in pdf.pages:
            t = page.extract_text()
            if t:
                pages.append(t)
    return pages

def extract_full_text(pdf_path: str, password: str = None) -> str:
    """Return full PDF text"""
    return "\n".join(extract_pages(pdf_path, password))