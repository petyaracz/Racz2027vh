# VH

Code and data for a paper on vowel harmony in Hungarian loanwords using kernel ridge regression (KRR).

Hungarian loanwords with neutral vowels show variable back/front suffix selection (e.g. *hotelnak* vs *hotelnek*). The project tests whether phonological or semantic similarity to words with stable harmony predicts this variation, using KRR models trained on corpus counts and evaluated against suffix-selection experiments with real words and nonwords.

Word embeddings: http://corpus.nytud.hu/efnilex-vect/

Word frequencies: https://github.com/petyaracz/Webcorpus2FrequencyList

## Pipeline

1. `script/fit_krr.R` — fits KRR models (phonological and semantic kernels) on real loanwords, predicts nonwords; writes predictions and MDS coordinates to `dat/`
2. `script/eval_krr.R` — reads predictions, runs GLMs and Bayesian models (glmmTMB, rstanarm), produces correlation tests, model comparisons, and figures

## Files

```
script/
  fit_krr.R                      KRR model fitting and prediction
  eval_krr.R                     Model evaluation, statistical tests, figures
  docens.R                       Figures for a talk
  get_semantic_distances.py      Computes pairwise semantic distances from word embeddings
  complete_semantic_distances.R  Completes the semantic distance matrix
  parse_results.R                Parses raw experimental results
  check_results.R                Sanity checks on results
  generate_non_words.R           Generates nonword stimuli (v1)
  generate_non_words_updated.R   Generates nonword stimuli (v2)
  write_nonwords_to_exp.R        Writes nonword stimuli to experiment format

non_word_exp/
  make_non_words.R               Nonword generation for the experiment

real_word_exp/
  parse_exp.R                    Parses real-word experiment output
  dat/unfiltered_data.tsv        Raw real-word experiment output

dzsungel/
  script/setup.R                 Setup for the Dzsungel corpus subset
  script/viz.R                   Visualisations for the Dzsungel corpus subset
  dat/dzsungel_forms.tsv         Loanword forms
  dat/dzsungel_pred.tsv          Predictions for the Dzsungel subset

dat/
  dzsungel.tsv                   Training set: 163 loanwords with corpus back/front counts
  word_distances.tsv.gz          Pairwise phonological distances (IPA-transcribed forms)
  semantic_distances.csv         Pairwise semantic distances (raw)
  semantic_distances_ignore_colname.tsv  Semantic distances in long format for KRR
  stemlanguage.tsv               Etymology (source language) for each stem
  train_test_forms.tsv           Lemma list
  real_words_phon_preds.tsv      KRR LOO predictions, phonological kernel, real words
  real_words_semantic_preds.tsv  KRR LOO predictions, semantic kernel, real words
  nonwords_phon_preds.tsv        KRR predictions, phonological kernel, nonwords
  filtered_data_real_word.tsv    Filtered trial data, real-word experiment
  filtered_data_nonword.tsv      Filtered trial data, nonword experiment
  unfiltered_data_real_word.tsv  Raw trial data, real-word experiment
  unfiltered_data_nonword.tsv    Raw trial data, nonword experiment
  master.tsv                     Full stimulus file with jsPsych trial fields
  baseline_tidy_proc.tsv         Baseline nonce-word response data
  siptar_torkenczy_toth_racz_hungarian.tsv  Hungarian phonological inventory reference
  stim.js                        jsPsych stimulus file

viz/
  mds_real_phon.png              MDS of real words in phonological space, coloured by log odds
  mds_real_phon_labels.png       Same, faceted by etymology
  mds_real_sem.png               MDS of real words in semantic space, coloured by log odds
  mds_real_sem_labels.png        Same, faceted by etymology
  mds_nonwords_phon.png          MDS of nonwords in phonological space, coloured by KRR prediction
  obs_pred_real_phon.png         Observed vs predicted log odds, phonological KRR, real words
  obs_pred_real_sem.png          Observed vs predicted log odds, semantic KRR, real words
  obs_pred_nonwords_phon.png     Observed vs predicted log odds, phonological KRR, nonwords
  model_predictions.png          Bayesian model predictions across corpus and experiment data

corpus_loo.tsv                   LOO comparison for Bayesian corpus models (b1–b3)
exp_loo.tsv                      LOO comparison for Bayesian experiment models (b4–b6)
```

## Data dictionary: files read by `eval_krr.R`

### `dat/real_words_phon_preds.tsv`

163 rows (one per loanword). KRR fitted with a phonological distance kernel, leave-one-out predictions.

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
| `language` | source language (etymology) |
| `predicted_loo` | KRR LOO prediction on the log-odds scale |
| `phonological_x` | MDS dimension 1 of phonological distance space |
| `phonological_y` | MDS dimension 2 of phonological distance space |
| `sigma` | KRR bandwidth hyperparameter |
| `alpha` | KRR regularisation hyperparameter |
| `rmse` | LOO RMSE of the fitted model |

### `dat/nonwords_phon_preds.tsv`

50 rows (one per nonword). KRR trained on real words, applied to held-out nonwords.

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

### `dat/real_words_semantic_preds.tsv`

~155 rows (real words with word-embedding coverage). KRR fitted with a semantic distance kernel, leave-one-out predictions. Columns identical to `real_words_phon_preds.tsv` except `semantic_x`/`semantic_y` replace `phonological_x`/`phonological_y`.

### `dat/unfiltered_data_real_word.tsv`

2500 rows (one per trial). Trial-level data from the real-word suffix-selection experiment.

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

2700 rows (one per trial). Trial-level data from the nonword suffix-selection experiment.

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
