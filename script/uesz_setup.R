# -- head -- #

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(glue)

# -- read -- #

d = read_tsv('dat/dzsungel.tsv')
e = read_csv('dat/entries_hand_edited.csv')

# -- this is bad sorry -- #

d$borrowing_label = NULL
d$borrowing_language = NULL
d$borrowing_period = NULL
d$borrowing_category = NULL

# -- code -- #

e |> count(`source languages`, sort = T)
# german english french latin yiddish
e |> ggplot(aes(`first year attested`)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(1200,2000,50))
# -1750; 1751-1910; 1910-

e2 = e |> 
  mutate(
    borrowing_language = case_when(
      `source languages` == 'német' ~ 'German',
      `source languages` == 'angol' ~ 'English',
      `source languages` == 'francia' ~ 'French',
      `source languages` == 'latin' ~ 'Latin',
      `source languages` == 'jiddis' ~ 'Yiddish'
    ),
    borrowing_period = case_when(
      `first year attested` < 1901 ~ '-1900',
      `first year attested` > 1901 ~ '1901-'
    ),
    stem = word,
    borrowing_label = case_when(
      borrowing_language %in% c('German','English') & !is.na(borrowing_period) ~ glue::glue('{borrowing_language}; {borrowing_period}'),
      borrowing_language %in% c('French','Latin','Yiddish') ~ borrowing_language
    ),
    borrowing_category = case_when(
      borrowing_language %in% c('German','Yiddish') ~ 'German/Yiddish',
      borrowing_language %in% c('French','Latin') ~ 'French/Latin'
    )
  ) |> 
  select(stem,borrowing_language,borrowing_period,borrowing_label,borrowing_category)

# -- join -- #

d |> 
  left_join(e2) |> 
  write_tsv('dat/dzsungel.tsv')
