# import pdfplumber
# import requests
# import re
# from groq import Groq
# import os
# import json

# client = Groq(api_key=os.getenv("GROQ_API_KEY"))
# MODEL_NAME = "llama-3.1-8b-instant"
# # ==========================================================
# # TEXT EXTRACTION
# # ==========================================================
# def extract_text(pdf_path, password=None):

#     full_text = ""

#     if password:
#         pdf = pdfplumber.open(pdf_path, password=password)
#     else:
#         pdf = pdfplumber.open(pdf_path)

#     with pdf:
#         for page in pdf.pages:
#             text = page.extract_text()
#             if text:
#                 full_text += text + "\n"

#     lines = full_text.split("\n")

#     merged_lines = []
#     current_line = ""

#     for line in lines:
#         line = line.strip()

#         if not line:
#             continue

#         # Transaction starts with date
#         if re.match(r"^\d{2}/\d{2}/\d{4}", line):
#             if current_line:
#                 merged_lines.append(current_line.strip())
#             current_line = line
#         else:
#             if current_line:
#                 current_line += " " + line

#     if current_line:
#         merged_lines.append(current_line.strip())

#     return "\n".join(merged_lines)


# # ==========================================================
# # LLM PARSER
# # ==========================================================
# def parse_with_llm(pdf_path, password=None):

#     text = extract_text(pdf_path, password)

#     prompt = f"""
# You are extracting transactions from an Indian bank statement.

# Instructions:
# - Extract ALL transaction rows you see.
# - Do NOT skip transactions just because format is unclear.
# - Ignore page headers, footers, and running balances ONLY.
# - If type is unclear, infer using amount sign or keywords.
# - If unsure, still include transaction with best guess.
# - Do NOT invent new transactions.
# - Use only the rows provided.

# Return STRICT valid JSON only.

# Output format:
# [
#   {{
#     "date": "YYYY-MM-DD",
#     "details": "text",
#     "debit": number or null,
#     "credit": number or null,
#     "balance": number,
#     "confidence": number between 0 and 1
#   }}
# ]

# Rules:
# - If transaction is DEBIT:
#     debit = amount
#     credit = null
# - If transaction is CREDIT:
#     credit = amount
#     debit = null
# - Never return both debit and credit.


# Transaction Rows:
# {text}
# """

#     response = client.chat.completions.create(
#         model=MODEL_NAME,
#         messages=[{"role": "user", "content": prompt}],
#         temperature=0
#     )

#     llm_response = response.choices[0].message.content.strip()

#     print("\n--- RAW LLM RESPONSE ---\n")
#     print(llm_response[:2000])
#     print("\n-------------------------\n")

#     return llm_response

from groq import Groq
import os
import json

client = Groq(api_key=os.getenv("GROQ_API_KEY"))
MODEL_NAME = "llama-3.1-8b-instant"


# ==========================================================
# LLM PARSER (Uses full_text directly – NO pdfplumber here)
# ==========================================================
def parse_with_llm(full_text: str):

    prompt = f"""
You are extracting transactions from an Indian bank statement.

Instructions:
- Extract ALL transaction rows you see.
- Do NOT skip transactions just because format is unclear.
- Ignore page headers, footers, and summary sections.
- Do NOT invent transactions.
- Use only the rows provided.

Return STRICT valid JSON only.

Output format:
[
  {{
    "date": "YYYY-MM-DD",
    "details": "text",
    "debit": number or null,
    "credit": number or null,
    "balance": number,
    "confidence": number between 0 and 1
  }}
]

Rules:
- If transaction is DEBIT:
    debit = amount
    credit = null
- If transaction is CREDIT:
    credit = amount
    debit = null
- Never return both debit and credit.

Transaction Rows:
{full_text}
"""

    response = client.chat.completions.create(
        model=MODEL_NAME,
        messages=[{"role": "user", "content": prompt}],
        temperature=0
    )

    llm_response = response.choices[0].message.content.strip()

    print("\n--- RAW LLM RESPONSE ---\n")
    print(llm_response[:2000])
    print("\n-------------------------\n")

    return llm_response
