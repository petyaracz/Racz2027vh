# -- head -- #

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(glue)

# -- read -- #

d = read_tsv('dat/dzsungel.tsv')
e = read_csv('dat/entries_hand_edited.csv')
raw = read_csv('dat/entries_raw.csv')

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

e |> count(`source languages`, sort = T)
# german english french latin yiddish
e |> ggplot(aes(`first year attested`)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(1200,2000,50))
# -1750; 1751-1910; 1910-

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
      borrowing_language %in% c('German','Yiddish') ~ 'German/Yiddish',
      borrowing_language %in% c('French','Latin') ~ 'French/Latin'
    )
  ) |> 
  select(stem,borrowing_language,borrowing_period,borrowing_label,borrowing_category,first_mention)

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

# -- write -- #

write_tsv(d2, 'dat/dzsungel.tsv')
