# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# IMPORTS
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

import logging
import re
from pathlib import Path
from typing import List

import fitz
import pdfplumber
from pypdf import PdfReader, PdfWriter

logger = logging.getLogger("ledgerai.pdf_service")


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# ── STEP 2: CONFIG (verbatim from notebook)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

HEADER_KEYWORDS = [
    "date", "narration", "description", "particulars",
    "withdrawal", "deposit", "balance", "debit", "credit",
    "chq", "ref", "amount", "valuedt", "txn", "closing",
    "value", "number", "voucher", "cheque"
]

Y_TOLERANCE = 5

MIN_COLUMN_GAP = 8

TXN_STOP_PATTERNS = [
    re.compile(r'^\d{1,2}[-/]\w{2,3}[-/]\d{2,4}\b'),
    re.compile(r'^\d{2}/\d{2}/\d{4}\b'),
    re.compile(r'^\d+\s+\d{2}[-/]\w{2,3}[-/]\d{2,4}'),
]

HEADER_BAND_MERGE_GAP = 30


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# ── STEP 3: CORE ENGINE (verbatim from notebook)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

def clean(val):
    return re.sub(r'\s+', ' ', str(val or '')).strip()


def rows_to_pipe(rows):
    lines = []
    for row in rows:
        if any(str(c).strip() for c in row):
            lines.append("| " + " | ".join(clean(c) for c in row) + " |")
    return "\n".join(lines)


def find_column_boundaries(all_words, page_width, min_gap=None):
    gap = min_gap if min_gap is not None else MIN_COLUMN_GAP
    if not all_words:
        return [0, page_width]
    x0_vals = [int(w[0]) for w in all_words]
    max_x = max(x0_vals) + 1
    hist = [0] * (max_x + 1)
    for x in x0_vals:
        if 0 <= x <= max_x:
            hist[x] += 1
    win = max(1, gap // 2)
    smooth = []
    for i in range(len(hist)):
        lo = max(0, i - win)
        hi = min(len(hist), i + win + 1)
        smooth.append(sum(hist[lo:hi]))
    boundaries = [0.0]
    in_gap = False
    gap_start = 0
    for i, v in enumerate(smooth):
        if v == 0 and not in_gap:
            in_gap = True
            gap_start = i
        elif v > 0 and in_gap:
            in_gap = False
            if (i - gap_start) >= gap:
                boundaries.append((gap_start + i) / 2.0)
    boundaries.append(float(page_width))
    merged = [boundaries[0]]
    for b in boundaries[1:]:
        if b - merged[-1] >= gap:
            merged.append(b)
    if merged[-1] < page_width:
        merged.append(float(page_width))
    return merged


def assign_col(x, boundaries):
    for i in range(len(boundaries) - 1):
        if boundaries[i] <= x < boundaries[i + 1]:
            return i
    return len(boundaries) - 2


def find_header_band(y_bands, sorted_ys):
    """Returns (header_y, header_words, header_found:bool)"""
    # v4 ORIGINAL: single-band scan
    best_y = None
    best_words = []
    best_score = 0

    for yk in sorted_ys:
        band = y_bands[yk]
        text = " ".join(w[4].lower() for w in band)
        score = sum(1 for kw in HEADER_KEYWORDS if kw in text)
        if score > best_score:
            best_score = score
            best_y = yk
            best_words = band

    if best_score >= 2:
        return best_y, sorted(best_words, key=lambda w: w[0]), True
    if best_score == 1:
        return best_y, sorted(best_words, key=lambda w: w[0]), True


    pair_best_score = 0
    pair_best_y = None
    pair_best_words = []

    for i in range(len(sorted_ys) - 1):
        y1, y2 = sorted_ys[i], sorted_ys[i + 1]
        if y2 - y1 > HEADER_BAND_MERGE_GAP:
            continue
        combined_words = y_bands[y1] + y_bands[y2]
        combined_text  = " ".join(w[4].lower() for w in combined_words)
        score = sum(1 for kw in HEADER_KEYWORDS if kw in combined_text)
        if score > pair_best_score:
            pair_best_score = score
            pair_best_y     = y2
            pair_best_words = combined_words

    if pair_best_score >= 2:
        return pair_best_y, sorted(pair_best_words, key=lambda w: w[0]), True
    if pair_best_score == 1:
        return pair_best_y, sorted(pair_best_words, key=lambda w: w[0]), True

    return None, [], False


def build_col_names_from_header(header_words, boundaries):
    n_cols = len(boundaries) - 1
    col_buckets = [[] for _ in range(n_cols)]
    for w in header_words:
        ci = assign_col(w[0], boundaries)
        col_buckets[ci].append(w[4])
    return [' '.join(b) if b else f'Col_{i+1}' for i, b in enumerate(col_buckets)]


DATE_PAT   = re.compile(r'\b(\d{1,2}[-/]\w{2,3}[-/]\d{2,4}|\d{2}/\d{2}/\d{2,4})\b')
AMOUNT_PAT = re.compile(r'^[\d,]+\.\d{2}$')
REFNO_PAT  = re.compile(r'^\d{8,}$')


def infer_column_meanings(data_rows, col_names):
    n = len(col_names)
    meanings = {}
    for ci in range(n):
        samples = [row[ci] for row in data_rows if ci < len(row) and row[ci].strip()][:20]
        if not samples:
            meanings[ci] = 'Empty'
            continue
        date_hits   = sum(1 for s in samples if DATE_PAT.search(s))
        amount_hits = sum(1 for s in samples if AMOUNT_PAT.match(s.replace(',', '')))
        refno_hits  = sum(1 for s in samples if REFNO_PAT.match(s.replace(' ', '')))
        long_hits   = sum(1 for s in samples if len(s) > 15)
        total = len(samples)
        if date_hits / total > 0.5:
            meanings[ci] = 'Date'
        elif amount_hits / total > 0.4:
            meanings[ci] = 'Amount / Balance'
        elif refno_hits / total > 0.4:
            meanings[ci] = 'Reference / Cheque No.'
        elif long_hits / total > 0.5:
            meanings[ci] = 'Narration / Description'
        else:
            meanings[ci] = 'Unknown'
    return meanings


def merge_wrap_rows(rows, n_cols):
    if not rows:
        return rows
    merged = []
    for row in rows:
        row = list(row) + [''] * max(0, n_cols - len(row))
        col0_empty = not row[0].strip()
        rest_empty = all(not row[i].strip() for i in range(2, len(row)))
        if col0_empty and rest_empty and merged:
            extra = row[1].strip()
            if extra and len(merged[-1]) > 1:
                merged[-1][1] = (merged[-1][1] + ' ' + extra).strip()
        else:
            merged.append(row)
    return merged


def is_collapsed_table(table):
    """
    FIX 2: True only if >50% of cells in any single column have ≥3 newlines.
    """
    if not table or len(table) < 2:
        return False
    n_cols = max(len(row) for row in table)
    n_data = len(table) - 1
    if n_data < 1:
        return False
    for ci in range(n_cols):
        col_cells = [(table[ri][ci] or '') for ri in range(1, len(table))
                     if ci < len(table[ri])]
        collapsed_count = sum(1 for c in col_cells if c.count('\n') >= 3)
        if collapsed_count > len(col_cells) * 0.5:
            return True
    return False


def extract_pdfplumber(pdf_path, page_num):
    results = []
    try:
        with pdfplumber.open(pdf_path) as pdf:
            page = pdf.pages[page_num]
            for strat in [
                {"vertical_strategy": "lines",  "horizontal_strategy": "lines"},
                {"vertical_strategy": "lines",  "horizontal_strategy": "text"},
            ]:
                tables = page.extract_tables({**strat,
                    "snap_tolerance": 5, "join_tolerance": 5,
                    "edge_min_length": 3, "intersection_tolerance": 5})
                if not tables:
                    continue
                ok = []
                for t in tables:
                    if not t or len(t) < 2:
                        continue
                    if is_collapsed_table(t):
                        continue
                    col_names = [clean(c) for c in t[0]]
                    data = [[clean(c) for c in row] for row in t[1:]
                            if any(c for c in row)]
                    if len(data) < 2:
                        continue
                    data = merge_wrap_rows(data, len(col_names))
                    pipe = rows_to_pipe([col_names] + data)
                    if pipe:
                        ok.append(pipe)
                if ok:
                    return ok
    except Exception as e:
        logger.warning("[pdfplumber] p%d: %s", page_num + 1, e)
    return results


def extract_word_coords(pdf_path, page_num):
    """Returns (pipe_string, column_map_string)"""
    try:
        doc = fitz.open(pdf_path)
        page = doc[page_num]
        words = page.get_text("words")
        page_width = page.rect.width
        doc.close()
        if not words:
            return '', ''

        def yk(w):
            return round(w[1] / Y_TOLERANCE) * Y_TOLERANCE

        y_bands = {}
        for w in words:
            y_bands.setdefault(yk(w), []).append(w)
        sorted_ys = sorted(y_bands)

        header_y, header_words, header_found = find_header_band(y_bands, sorted_ys)

        start_y = header_y if header_found else -1
        data_words = [w for w in words if yk(w) > start_y]
        if not data_words:
            return '', ''

        boundaries = find_column_boundaries(data_words, page_width)
        n_cols = len(boundaries) - 1
        if n_cols < 2:
            return '', ''

        if header_found:
            col_names = build_col_names_from_header(header_words, boundaries)
            col_map_str = ''
        else:
            col_names = [f'Col_{i+1}' for i in range(n_cols)]

        row_map = {}
        for w in data_words:
            y = yk(w)
            if y not in row_map:
                row_map[y] = [''] * n_cols
            ci = assign_col(w[0], boundaries)
            row_map[y][ci] = (row_map[y][ci] + ' ' + w[4]).strip() if row_map[y][ci] else w[4]

        raw_rows = [row_map[y] for y in sorted(row_map)]
        if not raw_rows:
            return '', ''

        merged = merge_wrap_rows(raw_rows, n_cols)

        col_map_str = ''
        if not header_found:
            meanings = infer_column_meanings(merged, col_names)
            lines = ["\nCOLUMN MAP (auto-inferred — no header row found):"]
            lines.append(f"  {'Column':<12} {'Likely Meaning':<30} {'Sample Values'}")
            lines.append("  " + "-" * 70)
            for ci, name in enumerate(col_names):
                samples = [r[ci] for r in merged if ci < len(r) and r[ci].strip()][:3]
                lines.append(f"  {name:<12} {meanings.get(ci,''):<30} {' | '.join(samples)}")
            col_map_str = "\n".join(lines)

        pipe = rows_to_pipe([col_names] + merged)
        return pipe, col_map_str

    except Exception as e:
        logger.warning("[word-coords] p%d: %s", page_num + 1, e)
        return '', ''


def extract_raw_text(pdf_path, page_num):
    try:
        doc = fitz.open(pdf_path)
        text = doc[page_num].get_text("text")
        doc.close()
        return text.strip()
    except Exception as e:
        logger.warning("[raw-text] p%d: %s", page_num + 1, e)
        return ''


def extract_account_header(pdf_path, page_num=0):
  
    lines_out = []
    try:
        doc = fitz.open(pdf_path)
        blocks = sorted(doc[page_num].get_text("blocks"), key=lambda b: (b[1], b[0]))
        doc.close()
        for block in blocks:
            for line in block[4].split('\n'):
                line = line.strip()
                if not line:
                    continue
            
                if sum(1 for kw in HEADER_KEYWORDS if kw in line.lower()) >= 2:
                    return lines_out
                
                if any(p.search(line) for p in TXN_STOP_PATTERNS):
                    return lines_out
                lines_out.append(line)
    except Exception as e:
        logger.warning("[acct-header] %s", e)
    return lines_out



def extract_pages(pdf_path: str, password: str = None) -> List[str]:
    """Legacy pdfplumber extractor — one plain-text string per page."""
    logger.info("extract_pages: %s (password=%s)", pdf_path, bool(password))
    pages = []
    try:
        with pdfplumber.open(pdf_path, password=password) as pdf:
            for page_num, page in enumerate(pdf.pages, start=1):
                try:
                    text = page.extract_text(layout=True) or ""
                except Exception:
                    logger.warning("layout=True failed on page %d — falling back", page_num)
                    text = page.extract_text() or ""
                pages.append(text)
    except Exception as exc:
        logger.error("extract_pages failed: %s", exc, exc_info=True)
        raise
    logger.info("extract_pages: %d pages, %d chars",
                len(pages), sum(len(p) for p in pages))
    return pages


def open_pdf(path: str, password: str = None):
    """Open a PDF with fitz, handling encrypted files."""
    doc = fitz.open(path)
    if doc.is_encrypted:
        pwd = password or ''
        ok  = doc.authenticate(pwd)
        if not ok and isinstance(password, str):
            ok = doc.authenticate(password.encode())
        if not ok:
            doc.close()
            raise ValueError(f'Wrong password for: {Path(path).name}')
    return doc




def extract_pdf_text(pdf_path: str, password: str = None) -> str:
    # ── Decrypt if needed ────────────────────────────────────────────
    working_path = pdf_path
    if password:
        try:
            reader = PdfReader(pdf_path)
            if reader.is_encrypted:
                logger.info("PDF is encrypted — decrypting")
                if reader.decrypt(password) == 0:
                    raise ValueError("Wrong PDF password.")
                decrypted_path = pdf_path.rsplit('.', 1)[0] + "_svc_tmp.pdf"
                writer = PdfWriter()
                for pg in reader.pages:
                    writer.add_page(pg)
                with open(decrypted_path, "wb") as f:
                    writer.write(f)
                working_path = decrypted_path
                logger.info("Decrypted → %s", decrypted_path)
        except Exception as exc:
            logger.error("Decryption failed: %s", exc)
            raise

    # ── Page count ───────────────────────────────────────────────────
    doc   = fitz.open(working_path)
    total = len(doc)
    doc.close()
    logger.info("extract_pdf_text: %s  pages=%d", working_path, total)

    # ── Step 1: Account header ───────────────────────────────────────
    header_lines = extract_account_header(working_path, page_num=0)

    # ── Step 2: Detect METHOD on page 0 ─────────────────────────────
    test_pl = extract_pdfplumber(working_path, 0)
    test_wc, _ = extract_word_coords(working_path, 0) if not test_pl else ('', '')

    if test_pl:
        METHOD = "pdfplumber"
        logger.info("Method: pdfplumber (%d table(s) on page 1)", len(test_pl))
    elif test_wc:
        METHOD = "word-coords"
        logger.info("Method: word-coords (%d rows on page 1)", test_wc.count('\n') + 1)
    else:
        METHOD = "raw-text"
        logger.info("Method: raw-text fallback")

    # ── Step 3: Build output exactly like the notebook ───────────────
    out = []
    out += ["=" * 65, "ACCOUNT / STATEMENT HEADER INFORMATION", "=" * 65]
    out += header_lines
    out.append("")

    # -- Per-page loop (mirrors notebook exactly) ----------------------
    for pn in range(total):
        
        out += [
            "═" * 60,
            f"  PAGE {pn + 1} [BORDERED  H:12 V:5]",
            "═" * 60,
        ]

        if METHOD == "pdfplumber":
            tables = extract_pdfplumber(working_path, pn)
            if tables:
                for i, t in enumerate(tables, 1):
                    out += [f"\n--- Table {i} (pipe-separated) ---", t]
            else:
                wc, col_map = extract_word_coords(working_path, pn)
                if wc:
                    out += ["\n--- Table (pipe-separated) ---", wc]
                    if col_map:
                        out.append(col_map)
                else:
                    out += ["\n--- Raw Text ---", extract_raw_text(working_path, pn)]

        elif METHOD == "word-coords":
            wc, col_map = extract_word_coords(working_path, pn)
            if wc:
                out += ["\n--- Table (pipe-separated) ---", wc]
                if col_map:
                    out.append(col_map)
            else:
                out += ["\n--- Raw Text ---", extract_raw_text(working_path, pn)]

        else:  # raw-text
            rt = extract_raw_text(working_path, pn)
            out += ["\n--- Raw Text (spacing preserved) ---", rt]

        out.append("")

    full_text = "\n".join(str(x) for x in out)
    logger.info(
        "extract_pdf_text complete: pages=%d chars=%d method=%s",
        total, len(full_text), METHOD,
    )
    return full_text