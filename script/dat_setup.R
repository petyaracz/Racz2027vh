# combine Racz Rebrus data with etymological information and match against distance matrices (word2vec don't have all forms), write combined filtered data to disk

# -- head -- #

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(glue)

# -- read -- #

d = read_tsv('dat/dzsungel.tsv')
e = read_csv('dat/entries_hand_edited.csv')
raw = read_csv('dat/entries_raw.csv')
ph_dist = read_tsv('dat/word_distances.tsv.gz')
s_dist = read_tsv('dat/semantic_distances_ignore_colname.tsv')

# -- this is bad sorry -- #

d$borrowing_original = NULL
d$borrowing_label = NULL
d$borrowing_language = NULL
d$borrowing_period = NULL
d$borrowing_category = NULL
d$international = NULL
d$note = NULL
d$first_mention = NULL

# -- code -- #

e2 = e |> 
  mutate(
    first_mention = `first year attested`,
    borrowing_language = case_when(
      `source languages` == 'német' ~ 'German',
      `source languages` == 'angol' ~ 'English',
      `source languages` == 'francia' ~ 'French',
      `source languages` == 'latin' ~ 'Latin',
      `source languages` == 'jiddis' ~ 'Yiddish'
    ),
    borrowing_period = case_when(
      `first year attested` < 1501 ~ 'before 16th c',
      `first year attested` %in% 1501:1600 ~ '16th c',
      `first year attested` %in% 1601:1700 ~ '17th c',
      `first year attested` %in% 1701:1800 ~ '18th c',
      `first year attested` %in% 1801:1900 ~ '19th c',
      `first year attested` > 1901 ~ '20th c',
    ),
    stem = word,
    borrowing_label = case_when(
      borrowing_language %in% c('German','English') & !is.na(borrowing_period) ~ glue::glue('{borrowing_language}; {borrowing_period}'),
      borrowing_language %in% c('French','Latin','Yiddish') ~ borrowing_language
    ),
    borrowing_category = case_when(
      borrowing_language %in% c('German','Yiddish') ~ 'German/Yiddish (colloquial)',
      borrowing_language %in% c('French','Latin') ~ 'French/Latin (educated)'
    ),
    category_educated = borrowing_category == 'French/Latin (educated)'
  ) |> 
  select(stem,borrowing_language,borrowing_period,borrowing_label,borrowing_category,first_mention,category_educated)

r2 = raw |> 
  rename(
    borrowing_original = `source languages`,
    stem = word,
         ) |> 
  mutate(
    international = borrowing_original == 'nemzetközi',
    note = ifelse(international, 'international word', NA),
  ) |> 
  select(stem,borrowing_original,international,note)

e3 = left_join(e2,r2)

d2 = d |> 
  left_join(e3)

# -- other things -- #

d3 = d2 |> 
  mutate(
    borrowing_period = ifelse(is.na(borrowing_period), 'unknown', borrowing_period) |> 
      fct_relevel('unknown', after = Inf),
    borrowing_language = ifelse(is.na(borrowing_language), 'other', borrowing_language) |> 
      fct_relevel('other', after = Inf)
  ) |>  
  mutate(
    n1 = n(),
    .by = borrowing_language
  ) |> 
  mutate(
    n2 = n(),
    .by = borrowing_period
  ) |> 
  mutate(
    borrowing_language2 = glue('{borrowing_language} (n = {n1})'),
    borrowing_period2 = glue('{borrowing_period} (n = {n2})'),
    stem_log10_freq = log10(stem_freq / 9000),
    borrowing_label = ifelse(is.na(borrowing_label), '', borrowing_label),
    borrowing_label = ifelse(is.na(borrowing_label), '', borrowing_label),
    international2 = case_when(
      is.na(international) ~ '',
      international ~ 'international word',
      !international ~ ''
      ),
    category_educated_other = case_when(
      is.na(category_educated) ~ 'other',
      category_educated ~ 'educated',
      !category_educated ~ 'colloquial'
    )
  )

# -- clean -- #

drop = c('komplett','korrekt', 'ómen')

d3 = d3 |> 
  filter(!stem %in% drop)

d4 = d3 |> # incredible consistencies all over
  filter(
    transcribed %in% ph_dist$word1,
    transcribed %in% ph_dist$word2,
    stem %in% s_dist$word1,
    stem %in% s_dist$word2
  )

# -- write -- #

write_tsv(d4, 'dat/real_words.tsv')
