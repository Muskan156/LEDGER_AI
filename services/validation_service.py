import json
import re
from difflib import SequenceMatcher


# ==========================================================
# HELPER FUNCTIONS
# ==========================================================
from datetime import datetime

def normalize_date(date_str):
    if not date_str:
        return date_str

    for fmt in ("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d"):
        try:
            return datetime.strptime(str(date_str), fmt).strftime("%Y-%m-%d")
        except:
            continue

    return str(date_str)

def extract_json_from_response(response_text):

    response_text = response_text.replace("```json", "").replace("```", "").strip()

    match = re.search(r"\[.*\]", response_text, re.DOTALL)
    if match:
        try:
            return json.loads(match.group(0))
        except:
            return []
    return []


def calculate_similarity(a, b):
    return SequenceMatcher(None, str(a), str(b)).ratio()


# def validate_transactions(code_txns, llm_txns):

#     min_length = min(len(code_txns), len(llm_txns))

#     if min_length == 0:
#         return None

#     date_matches = 0
#     amount_matches = 0
#     balance_matches = 0
#     description_scores = []

#     for i in range(min_length):

#         code = code_txns[i]
#         llm = llm_txns[i]

#         # Date match
#         # if str(code.get("date")) == str(llm.get("date")):date_matches += 1
#         if normalize_date(code.get("date")) == normalize_date(llm.get("date")):
#             date_matches += 1

#         # Amount match
#         try:
#             code_amount = float(code.get("debit", 0) or code.get("credit", 0) or 0)
#             llm_amount = float(llm.get("debit", 0) or llm.get("credit", 0) or 0)

#             if abs(code_amount - llm_amount) < 1:
#                amount_matches += 1
#         except:
#             pass

#         # Balance match
#         try:
#            if abs(float(code.get("balance", 0)) - float(llm.get("balance", 0))) < 1:
#             balance_matches += 1
#         except:
#             pass

#         # Description similarity
#         desc_similarity = calculate_similarity(
#             code.get("details", ""),
#             llm.get("details", "")
#         )
#         description_scores.append(desc_similarity)

#     total = min_length

#     date_accuracy = (date_matches / total) * 100
#     amount_accuracy = (amount_matches / total) * 100
#     balance_accuracy = (balance_matches / total) * 100
#     description_accuracy = (sum(description_scores) / total) * 100

#     overall_accuracy = (
#         date_accuracy +
#         amount_accuracy +
#         balance_accuracy +
#         description_accuracy
#     ) / 4

#     return {
#         "date_accuracy": round(date_accuracy, 2),
#         "amount_accuracy": round(amount_accuracy, 2),
#         "balance_accuracy": round(balance_accuracy, 2),
#         "description_accuracy": round(description_accuracy, 2),
#         "overall_accuracy": round(overall_accuracy, 2),
#     }


# def validate_transactions(code_txns, llm_txns):

#     if not code_txns or not llm_txns:
#         return None

#     matched_llm_indexes = set()

#     date_matches = 0
#     amount_matches = 0
#     balance_matches = 0
#     description_scores = []

#     total = 0

#     for code in code_txns:

#         code_date = normalize_date(code.get("date"))
#         code_balance = float(code.get("balance") or 0)
#         code_amount = float(code.get("debit") or code.get("credit") or 0)

#         best_match_index = None

#         for idx, llm in enumerate(llm_txns):

#             if idx in matched_llm_indexes:
#                 continue

#             llm_date = normalize_date(llm.get("date"))
#             llm_balance = float(llm.get("balance") or 0)
#             llm_amount = float(llm.get("debit") or llm.get("credit") or 0)

#             if code_date == llm_date and abs(code_balance - llm_balance) < 1:
#                 best_match_index = idx
#                 break

#         if best_match_index is None:
#             continue

#         matched_llm_indexes.add(best_match_index)
#         llm = llm_txns[best_match_index]

#         total += 1

#         # Date
#         if normalize_date(code.get("date")) == normalize_date(llm.get("date")):
#             date_matches += 1

#         # Amount
#         if abs(code_amount - float(llm.get("debit") or llm.get("credit") or 0)) < 1:
#             amount_matches += 1

#         # Balance
#         if abs(code_balance - float(llm.get("balance") or 0)) < 1:
#             balance_matches += 1

#         # Description similarity
#         desc_similarity = calculate_similarity(
#             code.get("details", ""),
#             llm.get("details", "")
#         )
#         description_scores.append(desc_similarity)

#     if total == 0:
#         return None

#     date_accuracy = (date_matches / total) * 100
#     amount_accuracy = (amount_matches / total) * 100
#     balance_accuracy = (balance_matches / total) * 100
#     description_accuracy = (sum(description_scores) / total) * 100

#     overall_accuracy = (
#         date_accuracy +
#         amount_accuracy +
#         balance_accuracy +
#         description_accuracy
#     ) / 4

#     return {
#         "date_accuracy": round(date_accuracy, 2),
#         "amount_accuracy": round(amount_accuracy, 2),
#         "balance_accuracy": round(balance_accuracy, 2),
#         "description_accuracy": round(description_accuracy, 2),
#         "overall_accuracy": round(overall_accuracy, 2),
#     }

def validate_transactions(code_txns, llm_txns):

    if not code_txns or not llm_txns:
        return None

    matched_llm_indexes = set()

    date_matches = 0
    amount_matches = 0
    balance_matches = 0
    description_scores = []

    total = 0

    for code in code_txns:

        code_date = normalize_date(code.get("date"))
        code_amount = float(code.get("debit") or code.get("credit") or 0)
        code_balance = float(code.get("balance") or 0)
        code_desc = str(code.get("details") or "").strip()

        best_match_index = None
        best_match_score = 0

        for idx, llm in enumerate(llm_txns):

            if idx in matched_llm_indexes:
                continue

            llm_date = normalize_date(llm.get("date"))
            llm_amount = float(llm.get("debit") or llm.get("credit") or 0)
            llm_balance = float(llm.get("balance") or 0)
            llm_desc = str(llm.get("details") or "").strip()

            score = 0

            # 1Date match (strong weight)
            if code_date and llm_date and code_date == llm_date:
                score += 3

            # 2Amount match (strong weight)
            if abs(code_amount - llm_amount) < 1:
                score += 3

            # 3Balance match (medium weight)
            if abs(code_balance - llm_balance) < 1:
                score += 2

            # 4Description similarity (soft weight)
            desc_similarity = calculate_similarity(code_desc, llm_desc)
            if desc_similarity > 0.7:
                score += 2

            if score > best_match_score:
                best_match_score = score
                best_match_index = idx

        # Require minimum score to accept match
        if best_match_index is not None and best_match_score >= 3:
            matched_llm_indexes.add(best_match_index)
            llm = llm_txns[best_match_index]
            total += 1

            # Final scoring breakdown
            if normalize_date(code.get("date")) == normalize_date(llm.get("date")):
                date_matches += 1

            if abs(code_amount - float(llm.get("debit") or llm.get("credit") or 0)) < 1:
                amount_matches += 1

            if abs(code_balance - float(llm.get("balance") or 0)) < 1:
                balance_matches += 1

            description_scores.append(
                calculate_similarity(code_desc, llm.get("details", ""))
            )

    if total == 0:
        return None

    date_accuracy = (date_matches / total) * 100
    amount_accuracy = (amount_matches / total) * 100
    balance_accuracy = (balance_matches / total) * 100
    description_accuracy = (sum(description_scores) / total) * 100

    # Weighted final score (balanced scoring model)
    overall_accuracy = (
        (date_accuracy * 0.30) +
        (amount_accuracy * 0.30) +
        (balance_accuracy * 0.25) +
        (description_accuracy * 0.15)
    )

    return {
        "matched_transactions": total,
        "date_accuracy": round(date_accuracy, 2),
        "amount_accuracy": round(amount_accuracy, 2),
        "balance_accuracy": round(balance_accuracy, 2),
        "description_accuracy": round(description_accuracy, 2),
        "overall_accuracy": round(overall_accuracy, 2),
    }
