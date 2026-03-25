
import re

def build_prompt(identifier_json: dict, text_sample: str) -> str:
    
    institution = identifier_json.get("institution_name", "Unknown")
    doc_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")

    return f"""
You are a Python code generation engine for financial document parsing.
 
Generate COMPLETE, EXECUTABLE Python code to extract ALL transactions from this bank statement.
 
══════════════════════════════════════════════════════════════
DOCUMENT INFO
══════════════════════════════════════════════════════════════
Document Type: {doc_family}
Institution: {institution}

══════════════════════════════════════════════════════════════
ACTUAL DOCUMENT SAMPLE TEXT
══════════════════════════════════════════════════════════════
 
SAMPLE TEXT FROM ACTUAL DOCUMENT:{text_sample}
 
══════════════════════════════════════════════════════════════
ANALYSIS REQUIREMENTS
══════════════════════════════════════════════════════════════
 
Before writing code, identify the following from the sample text:
1. Column headers and their positions (Date, Narration, Debit, Credit, Balance)
2. Date format used (DD/MM/YYYY, DD-MM-YYYY, etc.)
3. Transaction row patterns (what makes a line a transaction?)
4. Number format (1,00,000.00 or 100000.00)
5. Multi-line transaction indicators
 
══════════════════════════════════════════════════════════════
EXTRACTION RULES
══════════════════════════════════════════════════════════════
 
SKIP (never extract):
- Headers: "Date", "Particulars", "Debit", "Credit", "Balance"
- Page markers: "Page 1 of 3", "PAGE 1", "============"
- Footers: "Generated on", "This is computer generated"
- Summary rows: "Opening Balance", "Closing Balance", "Grand Total", "STATEMENT SUMMARY"
- Account metadata: "Account Number", "IFSC", "Branch", "Customer ID"
 
EXTRACT:
- DATE: Convert to YYYY-MM-DD format
- DETAILS: Transaction description ONLY (no dates, amounts, page numbers)
- DEBIT: Withdrawal amount as float or None
- CREDIT: Deposit amount as float or None
- BALANCE: Closing balance as float or None
- CONFIDENCE: 0.7 to 1.0 based on extraction quality
 
HANDLE:
- Multi-line descriptions (append lines without dates to previous transaction)
- Indian number format (1,00,000.00 → 100000.00)
- Various date formats (DD/MM/YY, DD-MM-YYYY, DD Mon YYYY)
- Currency symbols (₹, Rs., INR) - remove them
 
══════════════════════════════════════════════════════════════
CODE REQUIREMENTS
══════════════════════════════════════════════════════════════
 
Your code MUST:
1. Be complete and executable (NO placeholders, NO "TODO", NO "pass", NO "...")
2. Use ONLY standard library (re, datetime, json)
3. Define: def extract_transactions(text: str) -> list:
4. Return: List of dicts with keys: date, details, debit, credit, balance, confidence
5. Include error handling (try-except)
6. Parse ALL transaction rows correctly
7. Skip ALL non-transaction rows
 
══════════════════════════════════════════════════════════════
OUTPUT FORMAT
══════════════════════════════════════════════════════════════
 
Return ONLY executable Python code. No markdown blocks. No explanations.
 
The code should:
1. Define extract_transactions() function
2. Define parse_date() function  
3. Define parse_amount() function
4. At the end: call extract_transactions(text) and print JSON
 
Example output structure:
[
  {{
    "date": "2026-01-03",
    "details": "UPI-Redbus India Private",
    "debit": 2680.00,
    "credit": None,
    "balance": 566438.40,
    "confidence": 1.0
  }}
]
 
CRITICAL: Every function must be fully implemented. No placeholders.
"""