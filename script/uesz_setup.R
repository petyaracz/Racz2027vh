# -- head -- #

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(glue)

# -- read -- #

d = read_tsv('dat/dzsungel.tsv')
e = read_csv('dat/entries_hand_edited.csv')

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
      `first year attested` < 1751 ~ '-1750',
      `first year attested` > 1750 & `first year attested` < 1911 ~ '1751-1910',
      `first year attested` > 1910 ~ '1911-'
    ),
    stem = word
  ) |> 
  select(stem,borrowing_language,borrowing_period)

# -- join -- #

d |> 
  left_join(e2) |> 
  write_tsv('dat/dzsungel.tsv')
