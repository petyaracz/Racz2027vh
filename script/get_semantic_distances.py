"""
Extract pairwise cosine distances between words in dzsungel.tsv
using a word2vec embedding file.

Dependency: numpy  (pip install numpy)

Usage (from repo root):
    python script/get_semantic_distances.py

Output: dat/semantic_distances.csv
"""

import csv          # read TSV and write output CSV
import math         # for sqrt in fallback; not used directly but kept for clarity
import os           # path operations
import sys          # exit on fatal error
import tarfile      # extract the .tgz without a shell call
import tempfile     # temporary directory for the extracted embedding file

import numpy as np  # vector operations for cosine distance


# --- paths (relative to repo root, assumed cwd) ---
TSV_PATH = "dat/dzsungel.tsv"           # input word list
TGZ_PATH = "hunembed0.0.tgz"           # word2vec archive
OUT_PATH = "dat/semantic_distances.csv" # output pairwise distances


def load_target_words(tsv_path):
    """Read the stem column from the TSV, return a set of target words."""
    words = []                                          # ordered list to keep output stable
    with open(tsv_path, newline="", encoding="utf-8") as f:  # open file
        reader = csv.DictReader(f, delimiter="\t")      # tab-separated
        for row in reader:                              # iterate rows
            words.append(row["stem"].strip())           # collect stem column
    return words                                        # return list


def extract_w2v_from_tgz(tgz_path, tmpdir):
    """Extract the .w2v file from the archive into tmpdir, return its path."""
    with tarfile.open(tgz_path, "r:gz") as tar:        # open gzip tar
        members = tar.getnames()                        # list archive contents
        w2v_members = [m for m in members if m.endswith(".w2v")]  # find embedding file
        if not w2v_members:                             # abort if missing
            sys.exit(f"No .w2v file found in {tgz_path}")
        tar.extract(w2v_members[0], path=tmpdir)        # extract to temp dir
        return os.path.join(tmpdir, w2v_members[0])    # return full path


def scan_embeddings(w2v_path, target_set):
    """
    Scan the word2vec text file line by line.
    Return a dict {word: np.array} for words found in target_set.
    """
    vectors = {}                                        # accumulate found vectors
    with open(w2v_path, encoding="utf-8") as f:        # open embedding file
        header = f.readline()                           # first line: "n_words n_dims"
        n_words, n_dims = map(int, header.split())      # parse header
        print(f"Embedding: {n_words} words, {n_dims} dims")  # progress info
        found = 0                                       # counter for progress
        for i, line in enumerate(f):                   # iterate all lines
            if not line.strip():                        # skip blank lines
                continue
            parts = line.split()                        # split on whitespace
            word = parts[0]                             # first token is the word
            if word in target_set:                      # only store if needed
                vec = np.array(parts[1:], dtype=np.float32)  # parse vector
                vectors[word] = vec                     # store it
                found += 1                              # bump counter
            if (i + 1) % 100_000 == 0:                 # print progress every 100k lines
                print(f"  scanned {i + 1:,} / {n_words:,} lines, found {found} targets so far")
            if found == len(target_set):                # stop early if all found
                print(f"  all {found} targets found at line {i + 1:,}, stopping early")
                break
    return vectors                                      # return word -> vector dict


def cosine_distance(a, b):
    """Return cosine distance (1 - cosine similarity) between two vectors."""
    dot = np.dot(a, b)                                  # dot product
    norm_a = np.linalg.norm(a)                          # magnitude of a
    norm_b = np.linalg.norm(b)                          # magnitude of b
    if norm_a == 0 or norm_b == 0:                      # guard against zero vectors
        return float("nan")
    return float(1.0 - dot / (norm_a * norm_b))         # cosine distance


def main():
    # --- load target word list ---
    words = load_target_words(TSV_PATH)                 # read stems from TSV
    target_set = set(words)                             # set for O(1) lookup
    print(f"Target words: {len(words)} stems from {TSV_PATH}")

    # --- extract embedding file ---
    print(f"Extracting {TGZ_PATH} ...")
    with tempfile.TemporaryDirectory() as tmpdir:       # auto-cleaned temp dir
        w2v_path = extract_w2v_from_tgz(TGZ_PATH, tmpdir)  # unpack archive
        print(f"Scanning {w2v_path} ...")
        vectors = scan_embeddings(w2v_path, target_set)  # collect needed vectors

    # --- report missing words ---
    missing = [w for w in words if w not in vectors]   # words not in embedding
    if missing:                                         # warn if any missing
        print(f"WARNING: {len(missing)} words not found in embedding:")
        for w in missing:                               # list them
            print(f"  {w}")

    # --- compute pairwise cosine distances ---
    found_words = [w for w in words if w in vectors]   # only words we have vectors for
    print(f"Computing pairwise distances for {len(found_words)} words ...")
    rows = []                                           # accumulate output rows
    for i, w1 in enumerate(found_words):               # outer loop
        for w2 in found_words[i + 1:]:                 # upper triangle only
            dist = cosine_distance(vectors[w1], vectors[w2])  # compute distance
            rows.append((w1, w2, dist))                # store result

    # --- write output ---
    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)  # ensure dat/ exists
    with open(OUT_PATH, "w", newline="", encoding="utf-8") as f:  # open output
        writer = csv.writer(f)                          # CSV writer
        writer.writerow(["word1", "word2", "semantic_distance"])  # header
        writer.writerows(rows)                          # all pairs
    print(f"Written {len(rows)} pairs to {OUT_PATH}")


if __name__ == "__main__":                             # entry point
    main()
