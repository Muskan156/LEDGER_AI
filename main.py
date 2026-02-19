import json
import getpass
from services.pdf_service import extract_pages
from services.identifier_service import (
    identify_statement,
    reduce_text_for_llm,
    generate_identifier_llm
)
from services.extraction_service import generate_extraction_logic_llm
from services.extraction_service import extract_transactions_using_logic
from repository.statement_category_repo import insert_statement_category, get_active_statement_categories


# def main():
#     try:
#         # -----------------------------
#         # 1️⃣ Get PDF Input
#         # -----------------------------
#         file_path = input("Enter PDF path: ").strip()
#         password = getpass.getpass("Enter password (if any): ").strip() or None

#         print("\n📄 Extracting PDF content...")
#         pages = extract_pages(file_path, password)
#         full_text = "\n".join(pages)
#         print(full_text[:900] + "\n...")  # Print first 500 chars for verification
#         # -----------------------------
#         # 2️⃣ Check if format exists
#         # -----------------------------
#         matched, result = identify_statement(full_text)

#         if matched:
#             print("✅ Format already exists in DB.")
#             return

#         # -----------------------------
#         # 3️⃣ New Format → Generate Identifier
#         # -----------------------------
#         print("🆕 New format detected. Generating identifier...")

#         reduced = reduce_text_for_llm(pages)
#         identifier_json = generate_identifier_llm(reduced)
#         print("\n🧠 Raw Identifier JSON:")
#         print(json.dumps(identifier_json, indent=2))

#         if not isinstance(identifier_json, dict):
#             raise ValueError("Identifier JSON is not valid.")

#         print("✅ Identifier generated successfully.")

#         # -----------------------------
#         # 4️⃣ Generate Extraction CODE
#         # # -----------------------------
#         print("⚙ Generating extraction logic based on identifier...")
#         extraction_code = generate_extraction_logic_llm(identifier_json)
#         if not isinstance(extraction_code, str) or not extraction_code.strip():
#             raise ValueError("Extraction code is not valid.")
#         print("✅ Extraction code generated successfully.")
#         print("\n================ GENERATED PYTHON CODE ================\n")
#         print(extraction_code)
#         print("\n================ END OF GENERATED CODE ================\n")
#         # -----------------------------
#         # # 5️⃣ Extract Bank Name Safely
#         # # -----------------------------
#         bank_name = (
#             identifier_json
#             .get("standard_identification_marks", {})
#             .get("bank_identification", {})
#             .get("bank_name", {})
#             .get("patterns", ["Unknown"])[0]
#             )
#         # -----------------------------
#         # # 6️⃣ Save to Database (store code string)
#         # # -----------------------------
#         print("💾 Saving new format to database...")
#         statement_id = insert_statement_category(
#             statement_type="BANK_STATEMENT",
#             format_name="AUTO_GENERATED_FORMAT",
#             institution_name=bank_name,
#             identifier_json=identifier_json.get("standard_identification_marks", {}),
#             extraction_logic_json=extraction_code,  # <-- store CODE not JSON
#             threshold=65.0
#             )
#         print(f"\n✅ New statement format saved with ID: {statement_id}")
#         print("Status: UNDER_REVIEW")
#         # -----------------------------
#         # # 7️⃣ TEST EXTRACTION IMMEDIATELY
#         # # -----------------------------
#         print("\n🧪 Testing extraction on current PDF...")
#         transactions = extract_transactions_using_logic(
#             full_text,identifier_json,extraction_code)
#         print(f"\nExtracted {len(transactions)} transactions.\n")
#         for i, txn in enumerate(transactions, 1):
#             print(f"---------------- Transaction {i} ----------------")
#             print(json.dumps(txn, indent=2))
#             print()
#             print("\n🚀 Program finished successfully.")
            
#     except Exception as e:
#         print("\n❌ Error occurred:")
#         print(str(e))

# if __name__ == "__main__":
#     main()
 
import json
import getpass


import json
import getpass


# def main():
#     try:
#         # -----------------------------
#         # 1️⃣ Get PDF Input
#         # -----------------------------
#         file_path = input("Enter PDF path: ").strip()
#         password = getpass.getpass("Enter password (if any): ").strip() or None

#         print("\nExtracting PDF content...")
#         pages = extract_pages(file_path, password)
#         full_text = "\n".join(pages)

#         print("\nPreview of extracted text:\n")
#         print(full_text[:900] + "\n...")

#         # -----------------------------
#         # 2️⃣ Generate Identifier JSON
#         # -----------------------------
#         print("\nGenerating identifier...")

#         reduced_text = reduce_text_for_llm(pages)
#         identifier_json = generate_identifier_llm(reduced_text)

#         if not isinstance(identifier_json, dict):
#             raise ValueError("Generated identifier is not valid JSON.")

#         print("Identifier generated successfully.")

#         # -----------------------------
#         # 3️⃣ Extract Bank Name
#         # -----------------------------
#         bank_name = (
#             identifier_json
#                 .get("bank_identification", {})
#                 .get("bank_name", {})
#                 .get("patterns", ["Unknown"])[0]
#         )

#         print(f"\n🏦 Detected Bank Name: {bank_name}")

#         # -----------------------------
#         # 4️⃣ Check If Bank Already Exists
#         # -----------------------------
#         existing_formats = get_active_statement_categories()

#         for fmt in existing_formats:
#             if (
#                 fmt["statement_type"] == "BANK_STATEMENT"
#                 and fmt["institution_name"] == bank_name
#             ):
#                 print("Format already exists for this bank. Skipping insert.")
#                 print("Process completed.")
#                 return

#         # -----------------------------
#         # 5️⃣ Generate Extraction Logic
#         # -----------------------------
#         print("\nGenerating extraction logic...")
#         extraction_code = generate_extraction_logic_llm(identifier_json)

#         if not isinstance(extraction_code, str) or not extraction_code.strip():
#             raise ValueError("Generated extraction logic is invalid.")

#         print("Extraction logic generated successfully.")

#         # -----------------------------
#         # 6️⃣ Save to Database (ACTIVE)
#         # -----------------------------
#         print("\nSaving new format to database...")

#         statement_id = insert_statement_category(
#             statement_type="BANK_STATEMENT",
#             format_name="AUTO_GENERATED_FORMAT",
#             institution_name=bank_name,
#             identifier_json=identifier_json,
#             extraction_logic_json=extraction_code,
#             threshold=65.0
#         )

#         print(f"\nFormat saved successfully.")
#         print(f"Statement ID: {statement_id}")
#         print("Status: ACTIVE")

#         print("\nProcess completed successfully.")

#     except Exception as e:
#         print("\nError occurred:")
#         print(str(e))


# if __name__ == "__main__":
#     main()
# import json
# import getpass
# import re

# from services.identifier_service import (
#     evaluate_identifier,
#     generate_identifier_llm,
#     reduce_text_for_llm
# )

# from repository.statement_category_repo import (
#     insert_statement_category
# )

# from services.pdf_service import extract_pages
# from db.connection import get_connection


# # ---------------------------------------------------
# # Extract IFSC
# # ---------------------------------------------------
# def extract_ifsc_from_text(text: str):
#     match = re.search(r"\b[A-Z]{4}0[A-Z0-9]{6}\b", text)
#     return match.group(0) if match else None


# # ---------------------------------------------------
# # Fetch formats by IFSC
# # ---------------------------------------------------
# def get_formats_by_ifsc(ifsc_code: str):
#     conn = get_connection()
#     cursor = conn.cursor(dictionary=True)

#     query = """
#         SELECT *
#         FROM statement_categories
#         WHERE statement_type = 'BANK_STATEMENT'
#         AND ifsc_code = %s
#         AND status = 'ACTIVE'
#     """

#     cursor.execute(query, (ifsc_code,))
#     rows = cursor.fetchall()

#     cursor.close()
#     conn.close()

#     for row in rows:
#         row["statement_identifier"] = json.loads(row["statement_identifier"])
#         row["extraction_logic"] = json.loads(row["extraction_logic"])

#     return rows


# # ---------------------------------------------------
# # Get next version based on IFSC
# # ---------------------------------------------------
# def get_next_version_for_ifsc(ifsc_code: str):
#     conn = get_connection()
#     cursor = conn.cursor(dictionary=True)

#     query = """
#         SELECT format_name
#         FROM statement_categories
#         WHERE statement_type = 'BANK_STATEMENT'
#         AND ifsc_code = %s
#     """

#     cursor.execute(query, (ifsc_code,))
#     rows = cursor.fetchall()

#     cursor.close()
#     conn.close()

#     max_version = 0

#     for row in rows:
#         match = re.search(r"_V(\d+)$", row["format_name"])
#         if match:
#             version = int(match.group(1))
#             max_version = max(max_version, version)

#     return max_version + 1


# # ---------------------------------------------------
# # MAIN
# # ---------------------------------------------------
# def main():
#     try:
#         # -----------------------------
#         # 1️⃣ Load PDF
#         # -----------------------------
#         file_path = input("Enter PDF path: ").strip()
#         password = getpass.getpass("Enter password (if any): ").strip() or None

#         print("\nExtracting PDF content...")
#         pages = extract_pages(file_path, password)
#         full_text = "\n".join(pages)
#         # -----------------------------
#         # 2️⃣ Extract IFSC
#         # -----------------------------
#         ifsc_code = extract_ifsc_from_text(full_text)

#         if not ifsc_code:
#             raise ValueError("IFSC code not found in statement.")

#         print(f"\nDetected IFSC Code: {ifsc_code}")

#         # -----------------------------
#         # 3️⃣ Fetch Existing Formats
#         # -----------------------------
#         print("\nChecking formats for this IFSC...")

#         formats = get_formats_by_ifsc(ifsc_code)

#         if formats:
#             best_match = None
#             best_score = 0

#             for fmt in formats:
#                 score, details = evaluate_identifier(fmt, full_text)

#                 print(f"Evaluating {fmt['format_name']} → Score: {score}%")

#                 if score > best_score:
#                     best_score = score
#                     best_match = fmt

#             if best_score >= 65:
#                 print("\nExisting format detected!")
#                 print("=" * 60)
#                 print(f"Bank Name   : {best_match['institution_name']}")
#                 print(f"Format Name : {best_match['format_name']}")
#                 print(f"Match Score : {best_score}%")
#                 print("=" * 60)
#                 # -----------------------------
#                 # # Print Stored Identifier
#                 # # -----------------------------
#                 print("\nStored Identifier JSON:")
#                 print("=" * 80)
#                 print(json.dumps(best_match["statement_identifier"], indent=2))
#                 print("=" * 80)
#                 # -----------------------------
#                 # # Print Stored Extraction Logic
#                 # # -----------------------------
#                 # print("\nStored Extraction Code:")
#                 # print("=" * 80)
#                 # print(best_match["extraction_logic"])
#                 # print("=" * 80)
#                 print("\nSkipping LLM generation.")
#                 return


#         # -----------------------------
#         # 4️⃣ Generate New Identifier
#         # -----------------------------
#         print("\nNo matching format found.")
#         print("Generating identifier...")

#         reduced = reduce_text_for_llm(pages)
#         identifier_json = generate_identifier_llm(reduced)

#         if not isinstance(identifier_json, dict):
#             raise ValueError("Invalid identifier JSON.")

#         print("\nGenerated Identifier JSON:")
#         print("=" * 80)
#         print(json.dumps(identifier_json, indent=2))
#         print("=" * 80)

#         # Extract bank name
#         bank_name = (
#             identifier_json
#             .get("bank_identification", {})
#             .get("bank_name", {})
#             .get("patterns", ["Unknown"])[0]
#         )

#         if not bank_name or bank_name.lower() == "unknown":
#             raise ValueError("Bank name not detected.")

#         print(f"\nBank Name: {bank_name}")

#         # -----------------------------
#         # 5️⃣ Generate Extraction Logic
#         # -----------------------------
#         from services.extraction_service import generate_extraction_logic_llm

#         print("\nGenerating extraction logic...")
#         extraction_code = generate_extraction_logic_llm(identifier_json)

#         # print("\nGenerated Extraction Code:")
#         # print("=" * 80)
#         # print(extraction_code)
#         print("=" * 80)

#         # -----------------------------
#         # 6️⃣ Versioning
#         # -----------------------------
#         version = get_next_version_for_ifsc(ifsc_code)
#         formatted_bank = bank_name.replace(" ", "_").upper()
#         format_name = f"{formatted_bank}_V{version}"

#         print(f"\nAssigned Format Name: {format_name}")

#         # -----------------------------
#         # 7️⃣ Insert (ACTIVE for testing)
#         # -----------------------------
#         statement_id = insert_statement_category(
#             statement_type="BANK_STATEMENT",
#             format_name=format_name,
#             institution_name=bank_name,
#             ifsc_code=ifsc_code,
#             identifier_json=identifier_json,
#             extraction_logic_json=extraction_code,
#             threshold=65.0
#         )

#         print("\nFormat saved successfully.")
#         print(f"Statement ID: {statement_id}")
#         print("Status: ACTIVE")

#         print("\nProcess completed successfully.")

#     except Exception as e:
#         print("\nError occurred:")
#         print(str(e))


# if __name__ == "__main__":
#     main()
# import json
# import getpass
# import re

# from services.identifier_service import (
#     evaluate_identifier,
#     generate_identifier_llm,
#     reduce_text_for_llm,
#     derive_bank_name_from_ifsc
# )

# from repository.statement_category_repo import insert_statement_category
# from services.pdf_service import extract_pages
# from db.connection import get_connection


# # ---------------------------------------------------
# # Fetch formats by BANK CODE
# # ---------------------------------------------------
# def get_formats_by_bank_code(bank_code: str):
#     conn = get_connection()
#     cursor = conn.cursor(dictionary=True)

#     query = """
#         SELECT *
#         FROM statement_categories
#         WHERE statement_type = 'BANK_STATEMENT'
#         AND ifsc_code = %s
#         AND status = 'ACTIVE'
#     """

#     cursor.execute(query, (bank_code,))
#     rows = cursor.fetchall()

#     cursor.close()
#     conn.close()

#     for row in rows:
#         row["statement_identifier"] = json.loads(row["statement_identifier"])
#         row["extraction_logic"] = json.loads(row["extraction_logic"])

#     return rows


# # ---------------------------------------------------
# # MAIN
# # ---------------------------------------------------
# def main():
#     try:
#         # -----------------------------
#         # 1️⃣ Load PDF
#         # -----------------------------
#         file_path = input("Enter PDF path: ").strip()
#         password = getpass.getpass("Enter password (if any): ").strip() or None

#         print("\nExtracting PDF content...")
#         pages = extract_pages(file_path, password)
#         full_text = "\n".join(pages)

#         # -----------------------------
#         # 2️⃣ Derive Bank Name & Code
#         # -----------------------------
#         bank_name, bank_code = derive_bank_name_from_ifsc(full_text)

#         if not bank_code:
#             raise ValueError("IFSC not found in document.")

#         print(f"\nBank Name : {bank_name}")
#         print(f"Bank Code : {bank_code}")

#         # -----------------------------
#         # 3️⃣ Check Existing Formats
#         # -----------------------------
#         print("\nChecking formats for this bank...")

#         formats = get_formats_by_bank_code(bank_code)

#         if formats:
#             best_match = None
#             best_score = 0

#             for fmt in formats:
#                 score, details = evaluate_identifier(fmt, full_text)
#                 print(f"Evaluating {fmt['format_name']} → Score: {score}%")
#                 print("Details:", details)
#                 if score > best_score:
#                     best_score = score
#                     best_match = fmt

#             # ✅ If strong match → reuse
#             if best_match and best_score >= 65:
#                 print("\nExisting format detected!")
#                 print("=" * 60)
#                 print(f"Format Name : {best_match['format_name']}")
#                 print(f"Match Score : {best_score}%")
#                 print("=" * 60)
#                 # 🔥 Print Stored Identification Markers
#                 print("\nStored Identification Markers:")
#                 print("=" * 80)
#                 print(json.dumps(best_match["statement_identifier"], indent=2))
#                 print("=" * 80)

#                 print("\nSkipping LLM generation.")
#                 return

#             # 🔥 If any format exists for this bank → DO NOT create V2
#             print("\nFormat already exists for this bank.")
#             print("Skipping new format creation.")
#             return

#         # ---------------------------------------------------
#         # 4️⃣ Generate New Format (Only if Bank Not Exists)
#         # ---------------------------------------------------
#         print("\nNo format found for this bank.")
#         print("Generating identifier...")

#         reduced = reduce_text_for_llm(pages)
#         identifier_json = generate_identifier_llm(reduced, full_text)

#         print("\nGenerated Identifier JSON:")
#         print("=" * 80)
#         print(json.dumps(identifier_json, indent=2))
#         print("=" * 80)

#         # -----------------------------
#         # 5️⃣ Generate Extraction Logic
#         # -----------------------------
#         from services.extraction_service import generate_extraction_logic_llm

#         print("\nGenerating extraction logic...")
#         extraction_code = generate_extraction_logic_llm(identifier_json)
#         # ---------------------------------------------------
#         # 🔎 TEST GENERATED EXTRACTION CODE BEFORE SAVING
#         # ---------------------------------------------------
#         print("\nTesting generated extraction logic...")
#         try:
#             transactions = extract_transactions_using_logic(
#             full_text,
#             identifier_json,
#             extraction_code)

#             print(f"\nExtracted {len(transactions)} transactions for testing.")
#             print("Sample Transactions:")
#             print(json.dumps(transactions[:5], indent=2))

#             if len(transactions) == 0:
#                 print("\nWARNING: No transactions extracted!")
#                 print("Human review required before saving.")

#         except Exception as exec_error:
#             print("\nExtraction logic execution failed:")
#             print(str(exec_error))
#             print("Human intervention required.")
#             return
#         # -----------------------------
#         # 6️⃣ Create Format Name (Always V1)
#         # -----------------------------
#         formatted_bank = bank_name.replace(" ", "_").upper()
#         format_name = f"{formatted_bank}_V1"

#         # -----------------------------
#         # 7️⃣ Insert New Format
#         # -----------------------------
#         statement_id = insert_statement_category(
#             statement_type="BANK_STATEMENT",
#             format_name=format_name,
#             institution_name=bank_name,
#             ifsc_code=bank_code,   # store only prefix
#             identifier_json=identifier_json,
#             extraction_logic_json=extraction_code,
#             threshold=65.0
#         )

#         print("\nFormat saved successfully.")
#         print(f"Statement ID: {statement_id}")
#         print("Status: ACTIVE")

#     except Exception as e:
#         print("\nError occurred:")
#         print(str(e))


# if __name__ == "__main__":
#     main()
import json
import getpass
import re
from services.review_service import run_review_engine
from services.identifier_service import (
    generate_identifier_llm,
    reduce_text_for_llm,
    derive_bank_name_from_ifsc
)

from services.extraction_service import (
    generate_extraction_logic_llm,
    extract_transactions_using_logic
)
from repository.statement_category_repo import insert_statement_category
from db.connection import get_connection
from services.pdf_service import extract_pages

# ---------------------------------------------------
# Fetch formats by BANK CODE (ACTIVE + UNDER_REVIEW)
# ---------------------------------------------------
def get_formats_by_bank_code(bank_code: str):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT *
        FROM statement_categories
        WHERE statement_type = 'BANK_STATEMENT'
        AND ifsc_code = %s
    """

    cursor.execute(query, (bank_code,))
    rows = cursor.fetchall()

    cursor.close()
    conn.close()

    # Deserialize JSON fields
    for row in rows:
        if isinstance(row["statement_identifier"], str):
            row["statement_identifier"] = json.loads(row["statement_identifier"])

    return rows

def main():
    try:
        # ---------------------------------------------------
        # 1️⃣ Load PDF
        # ---------------------------------------------------
        file_path = input("Enter PDF path: ").strip()
        password = getpass.getpass("Enter password (if any): ").strip() or None

        print("\nExtracting PDF content...")
        pages = extract_pages(file_path, password)
        full_text = "\n".join(pages)

        # ---------------------------------------------------
        # 2️⃣ Derive Bank via IFSC (Deterministic)
        # ---------------------------------------------------
        bank_name, bank_code = derive_bank_name_from_ifsc(full_text)

        if not bank_name:
           bank_name = bank_code or "UNKNOWN_BANK"

        if not bank_code:
            raise ValueError("IFSC not found.")

        print(f"\nBank Name : {bank_name}")
        print(f"Bank Code : {bank_code}")
        # -----------------------------
        # 3️⃣ Check Existing Formats
        # -----------------------------
        print("\nChecking formats for this bank...")

        formats = get_formats_by_bank_code(bank_code)

        if formats:
            print("\n⚠ Format already exists for this bank.")

            for fmt in formats:
                print("=" * 60)
                print(f"Format Name : {fmt['format_name']}")
                print(f"Status      : {fmt['status']}")
                print("=" * 60)

            print("\nRunning Matching / Review Engine on existing format...")
            run_review_engine(formats[0]["statement_id"], file_path, full_text)
            return
        # ---------------------------------------------------
        # 3️⃣ Generate Identifier
        # ---------------------------------------------------
        print("\nNo format found for this bank.")
        print("\nGenerating identifier...")
        reduced = reduce_text_for_llm(pages)
        identifier_json = generate_identifier_llm(reduced)

        # Override bank name safely
        # identifier_json["bank_identification"] = {
        #     "bank_name": {
        #         "patterns": [bank_name]
        #     }
        # }
        if bank_name:
            identifier_json["bank_identification"] = {
        "bank_name": {
            "patterns": [bank_name]
        }
        }

        print("\nGenerated Identifier JSON:")
        print(json.dumps(identifier_json, indent=2))

        # ---------------------------------------------------
        # 4️⃣ Generate Extraction Code
        # ---------------------------------------------------
        from services.extraction_service import generate_extraction_logic_llm

        print("\nGenerating extraction logic...")
        # extraction_code = generate_extraction_logic_llm(identifier_json)
        # Extract lightweight context from PDF
        headers = [
               line.strip()
               for line in full_text.splitlines()
               if any(k in line.lower() for k in ["date", "debit", "credit", "balance"])
               ][:5]

        footer = [
               line.strip()
               for line in full_text.splitlines()
               if any(k in line.lower() for k in ["summary", "end of statement"])
               ][-5:]

        sample_text = full_text[:5000]
        extraction_code = generate_extraction_logic_llm(identifier_json,headers,sample_text,footer)

        # # ---------------------------------------------------
        # # 5️⃣ Test Extraction Logic
        # # ---------------------------------------------------
        # print("\nTesting generated extraction logic...")

        # try:
        #     transactions = extract_transactions_using_logic(
        #         full_text,identifier_json,extraction_code
        #     )

        #     print(f"\nExtracted {len(transactions)} transactions.")
        #     print("Sample:")
        #     print(json.dumps(transactions[:5], indent=2))

        #     if len(transactions) == 0:
        #         print("\n⚠ WARNING: No transactions extracted.")
        #         print("Human review recommended.")

        # except Exception as exec_error:
        #     print("\n❌ Extraction logic execution failed:")
        #     print(str(exec_error))
        #     print("Human intervention required.")
        #     return

        # ---------------------------------------------------
        # 6️⃣ Save Format to DB
        # ---------------------------------------------------
        formatted_bank = bank_name.replace(" ", "_").upper()
        format_name = f"{formatted_bank}_V1"

        statement_id = insert_statement_category(
            statement_type="BANK_STATEMENT",
            format_name=format_name,
            institution_name=bank_name,
            ifsc_code=bank_code,
            identifier_json=identifier_json,
            extraction_logic_json=extraction_code,
            threshold=65.0
        )

        print("\nFormat saved successfully.")
        print(f"Statement ID: {statement_id}")
        print("Status: UNDER_REVIEW")
        print("\nRunning Review Engine...")
        run_review_engine(statement_id,file_path, full_text)

        
    except Exception as e:
        print("\nError occurred:")
        print(str(e))
        raise


if __name__ == "__main__":
    main()
