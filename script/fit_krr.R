# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(krr)

# -- functions -- #

# Hungarian orthography to IPA-like transcription for distance lookup
transcribeIPA = function(dat) {
  dat |> stringr::str_replace_all(c(
    'x' = 'ks', 'cs' = 'č', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ',
    'ny' = 'ṉ', 'sz' = 'ß', 's' = 'š', 'ß' = 's', 'ck' = 'kk',
    'codec' = 'kodek', 'ch' = 'h', 'ly' = 'j'
  ))
}

# Build square symmetric matrix from long format, run cmdscale k=2, return tibble with word x y
do_mds = function(dist_df, dist_col = 'phon_dist') {
  mat = dist_df |>
    pivot_wider(names_from = word2, values_from = all_of(dist_col)) |>
    column_to_rownames('word1') |>
    as.matrix()
  mat[is.na(mat)] = t(mat)[is.na(mat)]
  res = cmdscale(mat, k = 2)
  as_tibble(res) |>
    rename(x = V1, y = V2) |>
    mutate(word = rownames(res))
}

# -- read -- #

train = read_tsv('dat/dzsungel.tsv')
test_non = read_tsv('dat/filtered_data_nonword.tsv')
ph_dist = read_tsv('dat/word_distances.tsv.gz')
s_dist = read_tsv('dat/semantic_distances_ignore_colname.tsv')
etym = read_tsv('dat/stemlanguage.tsv')

# -- prepare nonwords -- #

test_non = test_non |>
  mutate(transcribed = transcribeIPA(target))

test_non2 = test_non |>
  count(stem = target, transcribed, accept) |>
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |>
  rename(n_back = `TRUE`, n_front = `FALSE`) |>
  mutate(log_odds_back = log((n_back + 0.001) / (n_front + 0.001)))

# -- 1. real words + phonological distance (LOO KRR) -- #

check_krr_inputs(train = train, dist = ph_dist, word_col = 'transcribed', outcome_col = 'p_back')

m_phon = train_krr(
  train, ph_dist,
  word_col = 'transcribed', outcome_col = 'log_odds_back',
  link = 'identity', criterion = 'rmse'
)

# MDS on training words only
ph_dist_train = ph_dist |>
  filter(word1 %in% train$transcribed, word2 %in% train$transcribed)

mds_phon = do_mds(ph_dist_train) |>
  rename(phonological_x = x, phonological_y = y, transcribed = word)

real_words_phon = train |>
  left_join(etym, by = 'stem') |>
  left_join(m_phon$predictions |> select(transcribed, predicted_loo), by = 'transcribed') |>
  left_join(mds_phon, by = 'transcribed') |>
  mutate(sigma = m_phon$sigma, alpha = m_phon$alpha, rmse = m_phon$best_score)

write_tsv(real_words_phon, 'dat/real_words_phon_preds.tsv')

# -- 2. nonwords + phonological distance -- #

preds_non = predict_krr(
  train_data = train, test_data = test_non2, dist_df = ph_dist,
  word_col = 'transcribed', outcome_col = 'log_odds_back',
  sigma = m_phon$sigma, alpha = m_phon$alpha, link = 'identity'
)

# big MDS: all words in ph_dist (includes both train and nonwords)
mds_phon_big = do_mds(ph_dist) |>
  rename(phonological_x = x, phonological_y = y, transcribed = word)

nonwords_phon = test_non2 |>
  left_join(preds_non |> select(transcribed, predicted), by = 'transcribed') |>
  left_join(mds_phon_big |> filter(transcribed %in% test_non2$transcribed), by = 'transcribed') |>
  mutate(sigma = m_phon$sigma, alpha = m_phon$alpha, rmse = m_phon$best_score)

write_tsv(nonwords_phon, 'dat/nonwords_phon_preds.tsv')

# -- 3. real words + semantic distance (LOO KRR) -- #

s_train = train |>
  filter(stem %in% s_dist$word1, stem %in% s_dist$word2) |>
  filter(stem != 'ómen')

check_krr_inputs(train = s_train, dist = s_dist, word_col = 'stem', outcome_col = 'p_back')

m_sem = train_krr(
  s_train, s_dist,
  word_col = 'stem', outcome_col = 'log_odds_back',
  link = 'identity', criterion = 'rmse'
)

mds_sem = do_mds(s_dist) |>
  rename(semantic_x = x, semantic_y = y, stem = word)

real_words_semantic = s_train |>
  left_join(etym, by = 'stem') |>
  left_join(m_sem$predictions |> select(stem, predicted_loo), by = 'stem') |>
  left_join(mds_sem, by = 'stem') |>
  mutate(sigma = m_sem$sigma, alpha = m_sem$alpha, rmse = m_sem$best_score)

write_tsv(real_words_semantic, 'dat/real_words_semantic_preds.tsv')
