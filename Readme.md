# VH

Code and data for a paper on vowel harmony in Hungarian loanwords using kernel ridge regression (KRR).

Hungarian loanwords with neutral vowels show variable back/front suffix selection (e.g. *hotelnak* vs *hotelnek*). The project tests whether phonological or semantic similarity to words with stable harmony predicts this variation, using KRR models trained on corpus counts, evaluated with GLMMs and visualised against etymology.

Word embeddings: http://corpus.nytud.hu/efnilex-vect/

Word frequencies: https://github.com/petyaracz/Webcorpus2FrequencyList

## Pipeline

Data prep, already run once, outputs tracked in `dat/`:

- `script/uesz_scrape.py` — pulls etymology entries for the loanword list from the Új magyar etimológiai szótár (UESz) letter-PDFs; output hand-edited into `dat/entries_hand_edited.csv`
- `script/get_semantic_distances.py` — pairwise cosine distances between loanwords from word embeddings; writes `dat/semantic_distances.csv`
- `script/complete_semantic_distances.R` — reshapes `dat/semantic_distances.csv` into the long, symmetric format KRR needs; writes `dat/semantic_distances_ignore_colname.tsv`

Main pipeline, run in order:

1. `script/uesz_setup.R` — joins etymology (source language, borrowing period) from `dat/entries_hand_edited.csv` onto `dat/dzsungel.tsv`, overwriting it in place
2. `script/fit_krr.R` — fits KRR models (phonological and semantic kernels) on real loanwords with leave-one-out cross-validation; writes predictions and MDS coordinates to `dat/`
3. `script/eval_krr.R` — fits GLMMs comparing phonological vs semantic vs combined predictors against corpus counts; writes a model comparison table to `glmm_comparisons.txt`
4. `script/vis_krr.R` — MDS and observation-space figures; writes `viz/obs_spaces.png` (some figures are saved outside the repo, into a companion LaTeX project)

Dormant / archival (not part of the current pipeline; kept for reference):

- `real_word_exp/`, `dat/*_real_word.tsv` — real-word suffix-selection experiment. Data collected and parsed, but no longer modelled by `eval_krr.R`/`vis_krr.R`.
- `non_word_exp/`, `dat/*_nonword.tsv` — nonword suffix-selection experiment, retired. `fit_krr.R` still fits a nonword KRR model from `dat/filtered_data_nonword.tsv` and writes `dat/nonwords_phon_preds.tsv`, kept for the desk drawer in case the experiment is revived; nothing downstream reads it.
- `dzsungel/` — earlier exploratory setup/visualisation scripts, superseded by `uesz_setup.R` and `vis_krr.R`; reference an external repo (`RaczRebrus2024`) and won't run standalone.
- `script/docens.R` — figures for a talk, reproducible from tracked data.
- `script/parse_results.R`, `script/check_results.R` — parsing/sanity checks for the experiment trial data.

## Files

```
script/
  uesz_scrape.py                 Scrapes UESz etymology entries (prep)
  get_semantic_distances.py      Pairwise semantic distances from word embeddings (prep)
  complete_semantic_distances.R  Reshapes semantic distances for KRR (prep)
  uesz_setup.R                   Joins etymology onto dzsungel.tsv
  fit_krr.R                      KRR model fitting and prediction
  eval_krr.R                     GLMM model comparison
  vis_krr.R                      MDS and observation-space figures
  docens.R                       Figures for a talk
  parse_results.R                Parses raw experimental results
  check_results.R                Sanity checks on results

non_word_exp/
  generate_non_words.R           Nonword stimulus generation (v1), archival
  generate_non_words_updated.R   Nonword stimulus generation (v2), archival

real_word_exp/
  make_reaLwords.R               Real-word stimulus generation, archival
  parse_exp.R                    Parses real-word experiment output, archival
  dat/unfiltered_data.tsv        Raw real-word experiment output

dzsungel/
  script/setup.R                 Earlier setup for the Dzsungel corpus subset, archival
  script/viz.R                   Earlier visualisations, archival
  dat/dzsungel_forms.tsv         Loanword forms
  dat/dzsungel_pred.tsv          Predictions for the Dzsungel subset

dat/
  dzsungel.tsv                   Training set: 163 loanwords with corpus back/front counts and etymology
  entries_hand_edited.csv        Hand-edited UESz etymology entries
  word_distances.tsv.gz          Pairwise phonological distances (IPA-transcribed forms)
  semantic_distances.csv         Pairwise semantic distances (raw)
  semantic_distances_ignore_colname.tsv  Semantic distances in long format for KRR
  real_words_phon_preds.tsv      KRR LOO predictions, phonological kernel, real words
  real_words_semantic_preds.tsv  KRR LOO predictions, semantic kernel, real words
  real_words_both_preds.tsv      Phonological and semantic predictions joined, one row per word
  nonwords_phon_preds.tsv        KRR predictions, phonological kernel, nonwords (dormant, see Pipeline)
  train_test_forms.tsv           Lemma list
  filtered_data_real_word.tsv    Filtered trial data, real-word experiment (dormant)
  filtered_data_nonword.tsv      Filtered trial data, nonword experiment (dormant)
  unfiltered_data_real_word.tsv  Raw trial data, real-word experiment (dormant)
  unfiltered_data_nonword.tsv    Raw trial data, nonword experiment (dormant)
  master.tsv                     Full stimulus file with jsPsych trial fields
  baseline_tidy_proc.tsv         Baseline nonce-word response data, legacy
  gcm_param_search.tsv           GCM parameter search output, legacy
  siptar_torkenczy_toth_racz_hungarian.tsv  Hungarian phonological inventory reference
  stim.js                        jsPsych stimulus file

viz/
  obs_spaces.png                 Phonological/semantic MDS density by etymology, current output of vis_krr.R

glmm_comparisons.txt             Model comparison table (AIC/BIC/RMSE/R2/Bayes factors), current output of eval_krr.R
corpus_loo.tsv                   LOO comparison for Bayesian corpus models (b1–b3), legacy
exp_loo.tsv                      LOO comparison for Bayesian experiment models (b4–b6), legacy
```

## Data dictionary: files read by `eval_krr.R` and `vis_krr.R`

### `dat/dzsungel.tsv`

164 rows (one per loanword; `fit_krr.R` drops 2 known-bad stems before fitting). Corpus-derived back/front counts plus etymology, produced by `uesz_setup.R`.

| column | description |
|--------|-------------|
| `stem` | Hungarian orthographic stem |
| `back` | corpus count of back-suffix forms |
| `front` | corpus count of front-suffix forms |
| `log_odds_back` | log((back + 0.001) / (front + 0.001)) |
| `stem_freq` | total corpus frequency |
| `stem_varies` | whether the stem shows both back and front forms |
| `sd_back` | standard deviation of back proportion across corpus instances |
| `transcribed` | IPA-like transcription used for distance lookup |
| `p_back` | proportion of back-suffix forms |
| `borrowing_language` | source language, from UESz (German, English, French, Latin, Yiddish) |
| `borrowing_period` | first-attestation period, `-1900` or `1901-` |
| `borrowing_label` | German/English also split by period (e.g. `German; -1900`); French/Latin/Yiddish unsplit |
| `borrowing_category` | `German/Yiddish` vs `French/Latin`, or `NA` |

### `dat/real_words_phon_preds.tsv`

163 rows. KRR fitted with a phonological distance kernel, leave-one-out predictions. Same columns as `dzsungel.tsv` plus:

| column | description |
|--------|-------------|
| `predicted_loo` | KRR LOO prediction on the log-odds scale |
| `phonological_x` | MDS dimension 1 of phonological distance space |
| `phonological_y` | MDS dimension 2 of phonological distance space |
| `sigma` | KRR bandwidth hyperparameter |
| `alpha` | KRR regularisation hyperparameter |
| `rmse` | LOO RMSE of the fitted model |

### `dat/real_words_semantic_preds.tsv`

155 rows (real words with word-embedding coverage). KRR fitted with a semantic distance kernel, leave-one-out predictions. Columns as above except `semantic_x`/`semantic_y` replace `phonological_x`/`phonological_y`.

### `dat/real_words_both_preds.tsv`

155 rows. Inner join of the two prediction files above on `stem`, `sigma`/`alpha`/`rmse` dropped, `predicted_loo` renamed to `predicted_loo_phon`/`predicted_loo_sem`. This is what `eval_krr.R` and most of `vis_krr.R` read.

### `dat/nonwords_phon_preds.tsv`

51 rows (one per nonword). KRR trained on real words, applied to held-out nonwords. Still produced by `fit_krr.R` for the desk drawer; not consumed by `eval_krr.R` or `vis_krr.R`.

| column | description |
|--------|-------------|
| `stem` | orthographic nonword stem |
| `transcribed` | IPA-like transcription |
| `n_front` | experimental count of front-suffix acceptances |
| `n_back` | experimental count of back-suffix acceptances |
| `log_odds_back` | log((n_back + 0.001) / (n_front + 0.001)) |
| `predicted` | KRR prediction on the log-odds scale |
| `phonological_x` | MDS dimension 1 (full distance matrix including nonwords) |
| `phonological_y` | MDS dimension 2 |
| `sigma` | KRR bandwidth hyperparameter |
| `alpha` | KRR regularisation hyperparameter |
| `rmse` | LOO RMSE of the real-word model |

### `dat/unfiltered_data_real_word.tsv`

2500 rows (one per trial). Trial-level data from the real-word suffix-selection experiment. Dormant: not currently read by the active pipeline.

| column | description |
|--------|-------------|
| `id` | participant ID |
| `yob` | year of birth |
| `age` | age |
| `gender` | gender |
| `total_time` | total experiment time (ms) |
| `i` | trial index |
| `stem` | target stem |
| `target` | inflected form presented |
| `suffix` | suffix type (e.g. Dat, Pl) |
| `accept` | whether participant accepted the form (TRUE/FALSE) |
| `rt` | reaction time (ms) |
| `language` | source language of the stem |
| `log_odds_adj` | log odds of back suffix from corpus (adjusted) |
| `neighbourhood_size` | phonological neighbourhood size |
| `llfpm10` | log lemma frequency per 10 million |
| `stem_length` | stem length in characters |
| `stem_final` | final segment of the stem |
| `stem_phonology` | phonological class of stem-final segment |
| `stem_final_consonant_cluster` | consonant cluster size at stem end |
| `log_odds_back_suffix` | log odds of the specific back suffix form |
| `upper_time` | upper RT boundary |
| `lower_time` | lower RT boundary |
| `date` | trial timestamp |
| `transcription` | IPA-like transcription |
| `svm_weight_01` | SVM model weight (C=0.1) |
| `svm_weight_1` | SVM model weight (C=1) |
| `knn_2_weight` | k-NN (k=2) model weight |
| `yi_la_weight` | Yiddish/Latin neighbourhood weight |
| `x_phon` | phonological MDS dimension 1 |
| `y_phon` | phonological MDS dimension 2 |

### `dat/unfiltered_data_nonword.tsv`

2700 rows (one per trial). Trial-level data from the nonword suffix-selection experiment. Dormant.

| column | description |
|--------|-------------|
| `id` | participant ID |
| `yob` | year of birth |
| `age` | age |
| `gender` | gender |
| `total_time` | total experiment time (ms) |
| `i` | trial index |
| `target` | nonword form presented |
| `accept` | whether participant accepted the form (TRUE/FALSE) |
| `rt` | reaction time (ms) |
| `upper_time` | upper RT boundary |
| `lower_time` | lower RT boundary |
