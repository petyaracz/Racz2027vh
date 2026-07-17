#!/usr/bin/env python3
"""
uesz_scrape.py

Extracts etymology entries for a list of Hungarian words from the
Uj magyar etimologiai szotar (UESz), using the official per-letter
PDF downloads (https://uesz.nytud.hu/downloads.html), which the site
explicitly permits downloading for personal use.

This does NOT query the site's search interface repeatedly -- that would
fall under the site's "Jogi nyilatkozat", which reserves all rights and
requires written permission for copying/storage. Downloading the
letter-PDFs is the sanctioned route; this script just automates fetching
the PDFs you need and pulling out the specific entries.

Usage:
    python uesz_scrape.py ../dat/dzsungel.tsv --out entries.csv
    python uesz_scrape.py ../dat/dzsungel.tsv --out entries.json --format json
    python uesz_scrape.py --debug-letter A     # dump detected headwords for eyeballing

Input is a TSV file with a "stem" column (tab-delimited, header row required).
Use --stem-col to point at a different column name if needed.

Requirements:
    pip install requests pdfplumber --break-system-packages
"""

import argparse
import csv
import json
import re
import sys
from pathlib import Path

import requests
import pdfplumber

BASE_URL = "https://uesz.nytud.hu/files/uesz-{code}.pdf"

# PDFs are assumed to already be present here; this is only used as a
# fallback download location if a given letter's PDF is missing.
CACHE_DIR = (Path.home() / "Github" / "uesz").resolve()

# Hungarian alphabet -> PDF filename code, per downloads.html
LETTER_CODES = [
    ("A", "A"), ("Á", "A"),
    ("B", "B"),
    ("C", "C"),
    ("CS", "CS"),
    ("D", "D"),
    ("DZS", "DZS"),
    ("E", "E"), ("É", "E"),
    ("F", "F"),
    ("G", "G"),
    ("GY", "GY"),
    ("H", "H"),
    ("I", "I"), ("Í", "I"),
    ("J", "J"),
    ("K", "K"),
    ("L", "L"),
    ("LY", "LY"),
    ("M", "M"),
    ("N", "N"),
    ("NY", "NY"),
    ("O", "O"), ("Ó", "O"),
    ("Ö", "Oe"), ("Ő", "Oe"),
    ("P", "P"),
    ("R", "R"),
    ("S", "S"),
    ("SZ", "SZ"),
    ("T", "T"),
    ("TY", "TY"),
    ("U", "U"), ("Ú", "U"),
    ("Ü", "Ue"), ("Ű", "Ue"),
    ("V", "V"),
    ("X", "X"),
    ("Z", "Z"),
    ("ZS", "ZS"),
]

# Two headword patterns. Entries look like:
#   ágens ∆ A: 1616 Agense ...          (normal entry, "A:" starts the data section)
#   aggály → aggik¹                      (redirect entry, no data section)
#
# The "A:" marker is unambiguous (it never appears elsewhere in the running
# prose), so it's safe to match anywhere in the text -- important because
# PDF line-wrap hyphenation can split a headword across two lines (e.g.
# "se-\nhovatovább" for "sehovatovább"), which would otherwise fool a
# line-anchored pattern into detecting the fragment "hovatovább" as its
# own (wrong) headword. Dehyphenation (see dehyphenate()) reunites the
# word before this pattern runs.
#
# The "→" redirect marker, by contrast, is NOT unambiguous: this
# dictionary also uses "→" for inline cross-reference citations inside
# ordinary entries (e.g. "...azonos etimonra visszavezethető →ó."). Those
# must not be mistaken for redirect entries, so this pattern stays
# anchored to the start of a line, which is where genuine redirect
# entries begin.
HEADWORD_DATA_RE = re.compile(
    r"(?P<word>[A-Za-zÁÉÍÓÖŐÚÜŰáéíóöőúüű][A-Za-zÁÉÍÓÖŐÚÜŰáéíóöőúüű\-]*)"
    r"(?P<hom>[¹²³⁴⁵])?"
    r"\s*(?P<status>[†∆×])?"
    r"\s*A:\s"
)
HEADWORD_REDIRECT_RE = re.compile(
    r"(?m)^(?P<word>[A-Za-zÁÉÍÓÖŐÚÜŰáéíóöőúüű][A-Za-zÁÉÍÓÖŐÚÜŰáéíóöőúüű\-]*)"
    r"(?P<hom>[¹²³⁴⁵])?"
    r"\s*(?P<status>[†∆×])?"
    r"\s*\u2192"
)


def dehyphenate(text: str) -> str:
    """Rejoin words that PDF line-wrapping split with a hyphen (e.g.
    "se-\\nhovatovább" -> "sehovatovább"). This can occasionally over-join
    a genuine compound headword (e.g. ág-bog) if its hyphen happens to
    fall exactly at a line-wrap point, dropping the hyphen -- a rare edge
    case, worth spot-checking results for compound headwords."""
    return re.sub(r"(\w)-\n(\w)", r"\1\2", text)


def find_headword_matches(text: str):
    """Combine both headword patterns into one position-sorted list of
    match objects."""
    matches = list(HEADWORD_DATA_RE.finditer(text)) + list(HEADWORD_REDIRECT_RE.finditer(text))
    matches.sort(key=lambda m: m.start())
    return matches


def letter_code_for_word(word: str) -> str:
    """Work out which PDF a headword lives in, respecting Hungarian
    digraphs (cs, dzs, gy, ly, ny, sz, ty, zs), which sort as single
    letters."""
    w = word.strip().upper()
    for prefix_len in (3, 2, 1):
        prefix = w[:prefix_len]
        for letter, code in LETTER_CODES:
            if letter == prefix:
                return code
    raise ValueError(f"Could not determine letter/PDF for word: {word!r}")


def download_pdf(code: str) -> Path:
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    dest = CACHE_DIR / f"uesz-{code}.pdf"
    if dest.exists() and dest.stat().st_size > 0:
        return dest
    url = BASE_URL.format(code=code)
    print(f"  downloading {url} -> {dest}", file=sys.stderr)
    resp = requests.get(url, timeout=60)
    resp.raise_for_status()
    dest.write_bytes(resp.content)
    return dest


def extract_full_text(pdf_path: Path) -> str:
    texts = []
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            texts.append(page.extract_text() or "")
    full_text = "\n".join(texts)
    return dehyphenate(full_text)


def find_entries(full_text: str, target_words):
    """Split full_text into entries at each headword match, then return
    {original_target_word: entry_text} for whichever targets matched.
    If several homonyms match the same normalized headword (e.g. aggik1
    / aggik2 both matching a request for "aggik"), their texts are
    concatenated with a separator."""
    matches = find_headword_matches(full_text)
    targets_norm = {w.strip().lower(): w for w in target_words}
    found = {}
    for i, m in enumerate(matches):
        start = m.start()
        end = matches[i + 1].start() if i + 1 < len(matches) else len(full_text)
        word = m.group("word")
        norm = word.strip().lower()
        if norm in targets_norm:
            orig = targets_norm[norm]
            entry_text = full_text[start:end].strip()
            if orig in found:
                found[orig] += "\n\n---\n\n" + entry_text
            else:
                found[orig] = entry_text
    return found


def extract_first_year(entry_text: str) -> str:
    """First attestation, taken from right after the 'A:' data marker.
    Usually a plain year, e.g. "bakter A: 1638 Pakter ..." -> "1638".
    Some entries instead give a century, e.g. "hozzá A: 13. sz. eleje/
    ..." -> "13. sz. eleje" (beginning of the 13th c.)."""
    m = re.search(
        r"A:\s*(\d{1,2}\.\s*sz\.(?:\s*(?:eleje|közepe|vége))?|\d{3,4})",
        entry_text,
    )
    return m.group(1).strip() if m else ""


def extract_origin_summary(entry_text: str) -> str:
    """Text of the origin/etymology classification, i.e. the first
    sentence after the '■' marker (e.g. "Latin jövevényszó" or "Örökség
    a finnugor korból")."""
    m = re.search(r"■\s*(.+?)(?:\||$)", entry_text, re.S)
    if not m:
        return ""
    flat = re.sub(r"\s+", " ", m.group(1)).strip()
    return flat.split(".")[0].strip()


def extract_source_language(entry_text: str) -> str:
    """Best-effort source language, derived from the origin summary.
    Loanwords ("<Nyelv> jövevényszó") give the language name directly
    (e.g. "német", "latin"); other classifications (native formation,
    inherited vocabulary, compounds, international words) fall back to
    a short label or the raw classification text. This is a heuristic
    over free text, not a controlled vocabulary -- spot-check results."""
    origin = extract_origin_summary(entry_text)
    if not origin:
        return ""
    m = re.match(r"([A-Za-zÁÉÍÓÖŐÚÜŰáéíóöőúüű]+)\s+jövevényszó", origin, re.IGNORECASE)
    if m:
        return m.group(1).lower()
    if re.search(r"nemzetközi szó", origin, re.IGNORECASE):
        return "nemzetközi"
    m = re.search(r"örökség a (\w+) korból", origin, re.IGNORECASE)
    if m:
        return f"{m.group(1).lower()} örökség"
    if re.search(r"örökség", origin, re.IGNORECASE):
        return "örökség"
    return origin.lower()


def read_stem_column(tsv_path: Path, col_name: str):
    with open(tsv_path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f, delimiter="\t")
        if reader.fieldnames is None or col_name not in reader.fieldnames:
            raise SystemExit(
                f"Column '{col_name}' not found in {tsv_path}. "
                f"Columns present: {reader.fieldnames}"
            )
        words = [row[col_name].strip() for row in reader if row.get(col_name, "").strip()]
    seen = set()
    out = []
    for w in words:
        if w not in seen:
            seen.add(w)
            out.append(w)
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("wordlist", nargs="?", help="TSV file with a 'stem' column (tab-delimited, header row required)")
    ap.add_argument("--stem-col", default="stem", help="column name to read words from (default: stem)")
    ap.add_argument("--out", default="entries.csv")
    ap.add_argument("--format", choices=["csv", "json"], default="csv")
    ap.add_argument("--debug-letter", help="dump detected headwords for one letter code (e.g. A) and exit")
    args = ap.parse_args()

    if args.debug_letter:
        pdf_path = download_pdf(args.debug_letter)
        full_text = extract_full_text(pdf_path)
        matches = find_headword_matches(full_text)
        print(f"{len(matches)} headwords detected in uesz-{args.debug_letter}.pdf", file=sys.stderr)
        for m in matches[:200]:
            print(f"  {m.group('word')}{m.group('hom') or ''} {m.group('status') or ''}")
        return

    if not args.wordlist:
        ap.error("wordlist is required unless --debug-letter is given")

    words = read_stem_column(Path(args.wordlist), args.stem_col)
    print(f"{len(words)} words to look up", file=sys.stderr)
    print(f"PDF cache dir: {CACHE_DIR}", file=sys.stderr)

    by_code = {}
    for w in words:
        try:
            code = letter_code_for_word(w)
        except ValueError as e:
            print(f"  skip: {e}", file=sys.stderr)
            continue
        by_code.setdefault(code, []).append(w)

    all_found = {}
    all_missing = []
    for code, wlist in by_code.items():
        print(f"letter {code}: {len(wlist)} words", file=sys.stderr)
        pdf_path = download_pdf(code)
        full_text = extract_full_text(pdf_path)
        found = find_entries(full_text, wlist)
        all_found.update(found)
        for w in wlist:
            if w not in found:
                all_missing.append(w)

    print(f"found {len(all_found)}, missing {len(all_missing)}", file=sys.stderr)
    if all_missing:
        print("missing words (check spelling / headword form):", file=sys.stderr)
        for w in all_missing:
            print(f"  - {w}", file=sys.stderr)

    if args.format == "csv":
        with open(args.out, "w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow(["word", "source languages", "first year attested"])
            for w in words:
                entry = all_found.get(w, "")
                lang = extract_source_language(entry) if entry else ""
                year = extract_first_year(entry) if entry else ""
                writer.writerow([w, lang, year])
    else:
        out = {}
        for w in words:
            entry = all_found.get(w, "")
            out[w] = {
                "source languages": extract_source_language(entry) if entry else "",
                "first year attested": extract_first_year(entry) if entry else "",
            }
        with open(args.out, "w", encoding="utf-8") as f:
            json.dump(out, f, ensure_ascii=False, indent=2)

    print(f"written to {args.out}", file=sys.stderr)


if __name__ == "__main__":
    main()