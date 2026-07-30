# take output of dat_setup.R and fit KRR, get predictions, write to disk
# two sets of predictions: lo / category_educated ~ phon / sem

# -- head -- #

set.seed(1337)
library(tidyverse)
library(krr)
library(here)

# -- functions -- #

# Build square symmetric matrix from long format, run cmdscale k=2, return tibble with word x y
do_mds = function(dist_df, dist_col = 'phon_dist') {
  mat = dist_df |>
    pivot_wider(names_from = word2, values_from = all_of(dist_col)) |>
    column_to_rownames('word1') |>
    as.matrix()
  mat[is.na(mat)] = t(mat)[is.na(mat)]
  res = cmdscale(mat, k = 2)
  as_tibble(res) |>
    mutate(x = scales::rescale(V1), y = scales::rescale(V2)) |>
    mutate(word = rownames(res)) |> 
    select(-V1,-V2)
}

# -- read -- #

train = read_tsv(here('dat', 'real_words.tsv')) # output of dat_setup.R
ph_dist = read_tsv(here('dat', 'word_distances.tsv.gz'))
s_dist = read_tsv(here('dat', 'semantic_distances_ignore_colname.tsv'))

# -- filter for label fit -- #

# dat_setup already filters for words with phon and sem dist available; words with category_educated available are subset, so need further filt

train_label = train |> 
  filter(
    !is.na(category_educated)
  )

# -- 1. real words: lo ~ phonological distance (LOO KRR) -- #

check_krr_inputs(train = train, dist = ph_dist, word_col = 'transcribed', outcome_col = 'p_back')

m_phon = train_krr(
  train, ph_dist,
  sigma_grid = c(0.25, 0.5, 1, 
                 2, 3, 4, 5, 8, 16, 32, 64), 
  alpha_grid = c(0.01, 0.1, 1, 10, 100, 1000),
  word_col = 'transcribed', outcome_col = 'log_odds_back',
  link = 'identity', criterion = 'rmse'
)

# MDS on training words only
ph_dist_train = ph_dist |>
  filter(word1 %in% train$transcribed, word2 %in% train$transcribed)

# generate coordinates
mds_phon = do_mds(ph_dist_train) |>
  rename(phonological_x = x, phonological_y = y, transcribed = word)

# merge back into train
real_words_phon_lo = train |>
  left_join(m_phon$predictions |> select(transcribed, predicted_loo), by = 'transcribed') |>
  left_join(mds_phon, by = 'transcribed') |>
  mutate(sigma = m_phon$sigma, alpha = m_phon$alpha, rmse = m_phon$best_score)

# -- 2. real words, lo ~ semantic distance (LOO KRR) -- #

check_krr_inputs(train = train, dist = s_dist, word_col = 'stem', outcome_col = 'p_back')

m_sem = train_krr(
  train, s_dist,
  sigma_grid = c(0.25, 0.5, 1, 
                 2, 3, 4, 5, 8, 16, 32, 64), 
  alpha_grid = c(0.01, 0.1, 1, 10, 100, 1000),
  word_col = 'stem', outcome_col = 'log_odds_back',
  link = 'identity', criterion = 'rmse'
)

mds_sem = do_mds(s_dist) |>
  rename(semantic_x = x, semantic_y = y, stem = word)

real_words_semantic_lo = train |>
  left_join(m_sem$predictions |> select(stem, predicted_loo), by = 'stem') |>
  left_join(mds_sem, by = 'stem') |>
  mutate(sigma = m_sem$sigma, alpha = m_sem$alpha, rmse = m_sem$best_score)

# -- combine them to form singular lo pred dataset -- #

real_words_phon2 = real_words_phon_lo |> 
  select(-sigma,-alpha,-rmse) |> 
  rename(predicted_loo_phon = predicted_loo)

real_words_semantic2 = real_words_semantic_lo |> 
  select(-sigma,-alpha,-rmse) |> 
  rename(predicted_loo_sem = predicted_loo)

real_words_joined = inner_join(real_words_phon2,real_words_semantic2)

# -- 4. real words: label ~ phonological distance (LOO KRR) -- #

check_krr_inputs(train = train_label, dist = ph_dist, word_col = 'transcribed', outcome_col = 'category_educated')

m_phon_label_1 = train_krr(
  train_label, ph_dist,
  sigma_grid = c(0.25, 0.5, 1, 
                 2, 3, 4, 5, 8, 16, 32, 64), 
  alpha_grid = c(0.01, 0.1, 1, 10, 100, 1000),
  word_col = 'transcribed', outcome_col = 'category_educated',
  link = 'identity', criterion = 'accuracy', threshold = .5
)

real_words_phon_label = train_label |>
  left_join(m_phon_label_1$predictions |> select(transcribed, observed, predicted_loo), by = 'transcribed') |>
  mutate(predicted = predicted_loo >= 0.5) |> 
  left_join(mds_phon, by = 'transcribed') |>
  mutate(sigma = m_phon_label_1$sigma, alpha = m_phon_label_1$alpha, accuracy = m_phon_label_1$best_score)

# -- 3. real words, lo ~ semantic distance (LOO KRR) -- #

check_krr_inputs(train = train_label, dist = s_dist, word_col = 'stem', outcome_col = 'category_educated')

m_sem_label_1 = train_krr(
  train_label, s_dist,
  sigma_grid = c(0.25, 0.5, 1, 
                 2, 3, 4, 5, 8, 16, 32, 64), 
  alpha_grid = c(0.01, 0.1, 1, 10, 100, 1000),
  word_col = 'stem', outcome_col = 'category_educated', # incredible sloppiness
  link = 'identity', criterion = 'accuracy', threshold = .5
)

real_words_sem_label = train_label |> 
  left_join(m_sem_label_1$predictions |> select(stem, observed, predicted_loo), by = 'stem') |>
  mutate(predicted = predicted_loo >= 0.5) |> 
  left_join(mds_sem, by = 'stem') |>
  mutate(sigma = m_sem_label_1$sigma, alpha = m_sem_label_1$alpha, accuracy = m_sem_label_1$best_score)

# -- combine -- #

real_words_phon_l_2 = real_words_phon_label |> 
  select(-sigma,-alpha,-accuracy) |> 
  rename(
    predicted_loo_phon = predicted_loo,
    predicted_phon = predicted
  )

real_words_semantic_l_2 = real_words_sem_label |> 
  select(-sigma,-alpha,-accuracy) |> 
  rename(
    predicted_loo_sem = predicted_loo,
    predicted_sem = predicted
  )

real_words_joined_l = inner_join(real_words_phon_l_2,real_words_semantic_l_2)

# -- parameters -- #

m_phon
m_sem
m_phon_label_1
m_sem_label_1

# -- write -- #

write_tsv(real_words_joined, here('dat', 'real_words_krr_lo_preds.tsv'))
write_tsv(real_words_joined_l, here('dat', 'real_words_krr_label_preds.tsv'))
