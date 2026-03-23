"""
services/account_detector.py
─────────────────────────────
Account linking for uploaded documents.

Uses ONLY account_identifiers table — no accounts table join.
Filters strictly by user_id so each user sees only their own accounts.

Public functions:
  get_user_accounts(user_id)
      → queries account_identifiers WHERE user_id = ? AND is_active = true
      → returns list for the Review screen dropdown

  link_document_to_account(document_id, account_id)
      → sets documents.account_id = account_id
"""

import logging
from db.connection import get_client

logger = logging.getLogger("ledgerai.account_detector")


def get_user_accounts(user_id: str) -> list:
    """
    Fetch all active account_identifiers for this user.

    Returns list of dicts:
    [
        {
            "account_id":            42,
            "institution_name":      "HDFC BANK",
            "account_number_last4":  "8323",
            "account_number_masked": "XXXXXX8323",
            "card_last4":            None,
        },
        ...
    ]
    """
    try:
        sb = get_client()

        result = (
            sb.table("account_identifiers")
            .select(
                "account_id, institution_name, "
                "account_number_last4, account_number_masked, card_last4"
            )
            .eq("user_id", user_id)
            .eq("is_active", True)
            .execute()
        )
        rows = result.data or []

        if not rows:
            logger.info("get_user_accounts: no accounts for user=%s", user_id)
            return []

        # Deduplicate by account_id — keep first row per account
        seen = set()
        accounts = []
        for row in rows:
            aid = row.get("account_id")
            if aid in seen:
                continue
            seen.add(aid)
            accounts.append({
                "account_id":            aid,
                "institution_name":      row.get("institution_name") or "",
                "account_number_last4":  row.get("account_number_last4"),
                "account_number_masked": row.get("account_number_masked"),
                "card_last4":            row.get("card_last4"),
            })

        # Sort by institution name for consistent order
        accounts.sort(key=lambda x: x["institution_name"])

        logger.info(
            "get_user_accounts: user=%s  found=%d", user_id, len(accounts)
        )
        return accounts

    except Exception as exc:
        logger.warning("get_user_accounts: failed — %s", exc)
        return []


def link_document_to_account(document_id: int, account_id: int) -> None:
    """Set documents.account_id for this document."""
    try:
        sb = get_client()
        sb.table("documents").update(
            {"account_id": account_id}
        ).eq("document_id", document_id).execute()
        logger.info(
            "link_document_to_account: doc=%s → account_id=%s",
            document_id, account_id,
        )
    except Exception as exc:
        logger.warning(
            "link_document_to_account: failed for doc=%s: %s", document_id, exc
        )
        raise