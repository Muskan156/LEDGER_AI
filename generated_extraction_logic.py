import re

IDENTIFIER_SCHEMA = {
  "bank_identification": {
    "bank_name": {
      "patterns": [
        "State Bank of India"
      ]
    }
  },
  "header_markers": {
    "patterns": [
      "STATEMENT OF ACCOUNT",
      "Account Statement",
      "As on"
    ]
  },
  "footer_markers": {
    "patterns": [
      "Page no.",
      "Please do not share your ATM, Debit/Credit Card number, PIN (Personal Identification number ), OTP (One-Time Password), Username or Password with anyone via email, SMS, phone call, or any other medium.",
      "This is a computer generated statement and does not require a signature."
    ]
  },
  "metadata_keywords": [
    "Opening Balance",
    "Closing Balance",
    "Total",
    "Summary",
    "Account Summary",
    "Account open Date",
    "Statement From",
    "Statement Summary",
    "Brought Forward",
    "Dr Count",
    "Cr Count",
    "Total Debits",
    "Total Credits",
    "Closing Balance"
  ],
  "table_structure": {
    "column_headers": [
      "Date",
      "Narration",
      "Debit",
      "Credit",
      "Balance"
    ]
  },
  "transaction_anchor": {
    "date_pattern": "\\d{1,2}/\\d{1,2}/\\d{2,4}|\\d{1,2}-\\d{1,2}-\\d{2,4}|\\d{1,2}-\\d{1,2}-\\d{2}|\\d{4}-\\d{2}-\\d{2}|\\d{1,2} [A-Za-z]{3} \\d{2,4}|\\d{4}-\\d{2}-\\d{2}"
  },
  "amount_pattern": "\\d+(?:,\\d{2})*(?:,\\d{3})*\\.\\d{2}"
}

def extract_transactions(text: str) -> list:
    text = text.replace("\u00A0", " ")
    lines = [line.rstrip() for line in text.splitlines()]

    DATE_PATTERN = IDENTIFIER_SCHEMA["transaction_anchor"]["date_pattern"]
    DATE_ANCHOR_REGEX = rf'^\s*(?:\d+\s+)?({DATE_PATTERN})'
    MONEY_REGEX = IDENTIFIER_SCHEMA["amount_pattern"]

    debit = None
    credit = None
    transactions = []
    current_transaction = None
    previous_balance = None
    confidence = 0.0

    for line in lines:
        if any(pattern in line for pattern in IDENTIFIER_SCHEMA["footer_markers"]["patterns"]) or line.startswith("Account Summary"):
            break

        if any(keyword in line for keyword in IDENTIFIER_SCHEMA["metadata_keywords"]):
            continue

        if any(header in line for header in IDENTIFIER_SCHEMA["table_structure"]["column_headers"]):
            continue

        date_match = re.match(DATE_ANCHOR_REGEX, line)
        if date_match:
            if current_transaction:
                transactions.append({
                    "date": date_match.group(1),
                    "details": current_transaction["details"],
                    "debit": current_transaction["debit"],
                    "credit": current_transaction["credit"],
                    "balance": current_transaction["balance"],
                    "confidence": confidence
                })
                current_transaction = None
            current_transaction = {
                "date": date_match.group(1),
                "details": "",
                "debit": None,
                "credit": None,
                "balance": None,
                "confidence": 1.0
            }
            previous_balance = None

        money_matches = re.findall(MONEY_REGEX, line)
        if money_matches:
            if current_transaction:
                balance = float(money_matches[-1].replace(",", ""))
                if previous_balance is not None:
                    delta = balance - previous_balance
                    if delta > 0:
                        credit = balance
                        debit = None
                        confidence = 0.8
                    elif delta < 0:
                        debit = balance
                        credit = None
                        confidence = 0.8
                current_transaction["balance"] = balance
                current_transaction["debit"] = debit
                current_transaction["credit"] = credit
                current_transaction["confidence"] = confidence
                previous_balance = balance
            else:
                current_transaction["balance"] = float(money_matches[-1].replace(",", ""))
                current_transaction["confidence"] = 1.0

            if len(money_matches) > 1:
                if current_transaction["debit"] is None:
                    current_transaction["debit"] = float(money_matches[-2].replace(",", ""))
                if current_transaction["credit"] is None:
                    current_transaction["credit"] = float(money_matches[-1].replace(",", ""))

            current_transaction["details"] += line + " "

    if current_transaction:
        transactions.append({
            "date": current_transaction["date"],
            "details": current_transaction["details"],
            "debit": current_transaction["debit"],
            "credit": current_transaction["credit"],
            "balance": current_transaction["balance"],
            "confidence": current_transaction["confidence"]
        })

    return transactions