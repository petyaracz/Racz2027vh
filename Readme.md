# VH

Code and data for a paper on vowel harmony in Hungarian loanwords using kernel ridge regression (KRR).

Hungarian loanwords with neutral vowels show variable back/front suffix selection (e.g. *hotelnak* vs *hotelnek*). The project tests whether phonological or distributional-semantic similarity to other loanwords predicts this variation, and whether either similarity space also recovers the etymological stratum (educated Latin/French vs colloquial German/Yiddish). Models are fitted with KRR under leave-one-out cross-validation and evaluated with GLMMs against corpus counts.

## Running the code

Scripts resolve paths with the [`here`](https://here.r-lib.org/) package, so they can be run from anywhere in the repo without editing them. The repo root is marked by the `.here` file.

```r
source('script/dat_setup.R')
source('script/fit_krr.R')
source('script/eval_krr.R')
source('script/vis_krr.R')
```

or from a shell at the repo root:

```sh
Rscript script/dat_setup.R
Rscript script/fit_krr.R
Rscript script/eval_krr.R
Rscript script/vis_krr.R
```

`eval_krr.R` prints its model comparison and coefficient tables to the console as LaTeX; it writes no files. `vis_krr.R` writes four PDFs into `viz/`. Running the pipeline reproduces every tracked file in `dat/` byte for byte.

### Environment

Built and checked under R 4.5.2. All packages are on CRAN except `krr`, which is a companion package:

```r
remotes::install_github('petyaracz/KRR')
```

`janet` is needed only to regenerate the phonological distances, not to run the pipeline:

```r
remotes::install_github('petyaracz/JANET')
```

| package | version |
|---|---|
| tidyverse | 2.0.0 |
| here | 1.0.2 |
| glue | 1.8.1 |
| krr | 0.1.0 |
| glmmTMB | 1.1.14 |
| performance | 0.17.0 |
| MuMIn | 1.48.19 |
| broom.mixed | 0.2.9.7 |
| ggeffects | 2.3.2 |
| sjPlot | 2.9.0 |
| ggthemes | 5.2.0 |
| ggrepel | 0.9.8 |
| patchwork | 1.3.2 |
| knitr | 1.51 |
| scales | 1.4.0 |

The two prep scripts are Python and need `requests` and `pdfplumber` (`uesz_scrape.py`), and `numpy` (`get_semantic_distances.py`).

Seeds are set to 1337 in every script that needs one. Figures avoid non-ASCII glyphs in axis labels, since the default R `pdf` device cannot encode them and `cairo_pdf` needs XQuartz.

### External inputs not redistributed here

- Word embeddings: http://corpus.nytud.hu/efnilex-vect/ (`hunembed0.0.tgz`, needed only to re-run `get_semantic_distances.py`)
- Word frequencies: https://github.com/petyaracz/Webcorpus2FrequencyList
- UESz letter-PDFs, needed only to re-run `uesz_scrape.py`

The outputs of both prep steps are tracked in `dat/`, so the main pipeline runs without either.

## Provenance of `dat/dzsungel.tsv`

`dat/dzsungel.tsv` is the root input and is supplied as-is. It was built in an earlier project from the stem-level corpus table of [RaczRebrus2024](https://github.com/petyaracz/RaczRebrus2024) (`dat/dat_wide_stems.tsv`), by keeping only stems that vary in suffix choice (`stem_varies`), excluding `komplett`, and adding an IPA-like `transcribed` column and `p_back = plogis(log_odds_back)`.

Two consequences worth knowing. The 162 stems are already restricted to *variable* stems, so the modelling set is not a random sample of loanwords. And `dat_setup.R` hand-flags `komplett`, `korrekt` and `ómen` for removal, but only `ómen` is actually present: `komplett` was excluded at this earlier stage, so two of the three entries are inert leftovers rather than active filters.

## Provenance of the distance matrices

Both distance matrices ship with the repo, so the main pipeline runs without regenerating either.

**Phonological** (`dat/word_distances.tsv.gz`). Distances are alignment-based, computed by [janet](https://github.com/petyaracz/JANET) with `gap_penalty = 1.0` over the binary phonological feature matrix in `dat/siptar_torkenczy_toth_racz_hungarian.tsv`. Words are keyed on the IPA-like `transcribed` form, not orthography. To see exactly how, read `script/get_phon_distances.R`.

One caveat if you re-run that script. The shipped matrix covers 213 words: the 162 loanword stems plus 51 nonwords from an experiment that is not reported in the paper. `get_phon_distances.R` builds the loanword half only, so it produces a 162-word matrix rather than an identical copy of the shipped file. This does not change any result: the pipeline only ever uses loanword stems, all 162 are present either way, and the phonological filter in `dat_setup.R` drops nothing (all 7 stems it removes are dropped for missing *semantic* coverage). Because janet scores each pair independently, the 162-word submatrix is the same in both.

**Semantic** (`dat/semantic_distances_ignore_colname.tsv`). Cosine distances over the embeddings, via `get_semantic_distances.py` and then `complete_semantic_distances.R`.

## Pipeline

Prep, already run once, outputs tracked in `dat/`:

- `script/uesz_scrape.py` — pulls etymology entries for the loanword list from the Új magyar etimológiai szótár (UESz) letter-PDFs into `dat/entries_raw.csv`, hand-corrected into `dat/entries_hand_edited.csv`
- `script/get_phon_distances.R` — alignment-based phonological distances; writes `dat/word_distances.tsv.gz`. See *Provenance of the distance matrices* below
- `script/get_semantic_distances.py` — pairwise cosine distances between loanwords from word embeddings; writes `dat/semantic_distances.csv`
- `script/complete_semantic_distances.R` — reshapes those into the long, symmetric form KRR needs; writes `dat/semantic_distances_ignore_colname.tsv`

Main pipeline, run in order:

1. `script/dat_setup.R` — joins etymology from `dat/entries_hand_edited.csv` and `dat/entries_raw.csv` onto `dat/dzsungel.tsv`, drops hand-flagged stems and any stem missing from either distance matrix; writes `dat/real_words.tsv` (154 rows)
2. `script/fit_krr.R` — fits four KRR models under leave-one-out cross-validation, tuning sigma and alpha on a grid: log-odds and stratum label, each against a phonological and a semantic kernel. Also runs classical MDS on both distance matrices. Writes `dat/real_words_krr_lo_preds.tsv` and `dat/real_words_krr_label_preds.tsv`
3. `script/eval_krr.R` — GLMMs (`glmmTMB`) comparing phonological vs semantic vs combined predictors, for both the stratum label (m1–m4, binomial) and the corpus back/front counts (m5–m10, binomial with a by-stem random intercept). Prints comparison and coefficient tables
4. `script/vis_krr.R` — descriptive, MDS and prediction figures; writes the four PDFs in `viz/`

## Files

```
script/
  uesz_scrape.py                 Scrapes UESz etymology entries (prep)
  get_phon_distances.R           Phonological distances via janet (prep)
  get_semantic_distances.py      Pairwise semantic distances from embeddings (prep)
  complete_semantic_distances.R  Reshapes semantic distances for KRR (prep)
  dat_setup.R                    Builds the modelling table
  fit_krr.R                      KRR fitting, LOO prediction, MDS
  eval_krr.R                     GLMM model comparison
  vis_krr.R                      Figures

dat/
  dzsungel.tsv                   Source table: 162 loanwords, corpus back/front counts
  entries_hand_edited.csv        Hand-corrected UESz etymology entries
  entries_raw.csv                Unedited uesz_scrape.py output
  word_distances.tsv.gz          Pairwise phonological distances (IPA-transcribed)
  siptar_torkenczy_toth_racz_hungarian.tsv  Binary phonological feature matrix, input to janet
  semantic_distances.csv         Pairwise semantic distances (raw)
  semantic_distances_ignore_colname.tsv  Semantic distances, long format for KRR
  real_words.tsv                 Modelling table, output of dat_setup.R
  real_words_krr_lo_preds.tsv    KRR LOO predictions, log-odds outcome
  real_words_krr_label_preds.tsv KRR LOO predictions, stratum-label outcome

viz/
  data_descriptive_stats.pdf     Outcome distribution and etymology breakdown
  mds_pref.pdf                   MDS spaces, and density by suffix-preference bin
  mds_lang.pdf                   MDS spaces, density by source language
  predictions.pdf                Phonological vs semantic prediction, two colourings
```

An earlier version of this project also ran two suffix-selection experiments, with real words and with nonwords. Neither is reported in the paper and neither is included here.

## Data dictionary

### `dat/dzsungel.tsv`

162 rows, one per loanword. See *Provenance* above.

| column | description |
|--------|-------------|
| `stem` | Hungarian orthographic stem |
| `back` | corpus count of back-suffix forms |
| `front` | corpus count of front-suffix forms |
| `log_odds_back` | log((back + 0.001) / (front + 0.001)) |
| `stem_freq` | total corpus frequency |
| `stem_varies` | whether the stem shows both back and front forms. TRUE throughout, by construction |
| `sd_back` | standard deviation of back proportion across corpus instances |
| `transcribed` | IPA-like transcription, the key into `word_distances.tsv.gz` |
| `p_back` | proportion of back-suffix forms |

The file also carries etymology columns from an earlier run. `dat_setup.R` drops and recomputes all of them, so they are vestigial; the authoritative versions are in `real_words.tsv`.

### `dat/real_words.tsv`

154 rows. Output of `dat_setup.R`, input to `fit_krr.R`. Rows are the 162 stems of `dzsungel.tsv`, less `ómen` (hand-flagged) and the 7 stems with no entry in one of the two distance matrices. Columns as `dzsungel.tsv` plus:

| column | description |
|--------|-------------|
| `borrowing_language` | source language from the hand-edited UESz entries: German, English, French, Latin, Yiddish, else `other` |
| `borrowing_period` | first-attestation bucket: `before 16th c`, `16th c` … `20th c`, else `unknown` |
| `borrowing_label` | German and English split by period (e.g. `German; 19th c`); French, Latin, Yiddish unsplit |
| `borrowing_category` | `French/Latin (educated)` vs `German/Yiddish (colloquial)`, else `NA` |
| `category_educated` | logical, `borrowing_category == 'French/Latin (educated)'`. The outcome of the label models |
| `category_educated_other` | three-way version: `educated`, `colloquial`, `other` |
| `first_mention` | first-attestation year, numeric |
| `borrowing_original` | unedited source-language string from `entries_raw.csv` |
| `international` | logical, raw UESz entry tagged *nemzetközi* |
| `note` | `"international word"` where `international` is TRUE, else `NA` |
| `international2` | as `note`, empty string instead of `NA` |
| `stem_log10_freq` | log10(stem_freq / 9000), a per-million-ish scaling of a roughly 9 billion word corpus |
| `n1`, `n2` | group sizes by `borrowing_language` and `borrowing_period` |
| `borrowing_language2`, `borrowing_period2` | those two labels with `(n = …)` appended, for plot facets |

### `dat/real_words_krr_lo_preds.tsv`

154 rows. All columns of `real_words.tsv` plus:

| column | description |
|--------|-------------|
| `predicted_loo_phon` | KRR LOO prediction of `log_odds_back` from the phonological kernel |
| `predicted_loo_sem` | same from the semantic kernel |
| `phonological_x`, `phonological_y` | classical MDS coordinates of the phonological distance matrix, rescaled to [0, 1] |
| `semantic_x`, `semantic_y` | same for the semantic distance matrix |

### `dat/real_words_krr_label_preds.tsv`

94 rows, the subset of `real_words.tsv` with a non-missing `category_educated`. All columns of `real_words.tsv` plus:

| column | description |
|--------|-------------|
| `observed` | observed `category_educated`, as returned by the fit |
| `predicted_loo_phon` | KRR LOO prediction of `category_educated` from the phonological kernel, on [0, 1] |
| `predicted_phon` | `predicted_loo_phon >= 0.5` |
| `predicted_loo_sem`, `predicted_sem` | same for the semantic kernel |
| `phonological_x`, `phonological_y`, `semantic_x`, `semantic_y` | MDS coordinates, as above |

### `dat/word_distances.tsv.gz` and `dat/semantic_distances_ignore_colname.tsv`

Long-format pairwise distance tables, columns `word1`, `word2`, `phon_dist`. Both are symmetric and include the zero diagonal, which is what `krr` expects. `word_distances.tsv.gz` is keyed on `transcribed`, the semantic table on `stem`. The `phon_dist` column name in the semantic file is a misnomer retained for compatibility with `krr`, hence the file name.

`word_distances.tsv.gz` covers 213 words (162 loanwords plus 51 unreported nonwords); the semantic table covers the loanwords only. See *Provenance of the distance matrices* above.

## Licence

MIT, see `LICENSE`.
