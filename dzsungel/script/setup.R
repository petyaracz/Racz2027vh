# setup for JANET then KRR

# -- head -- #

setwd('~/Documents/Misc/habilitation/dzsungel/')

library(tidyverse)

# -- read -- #

d = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')

# -- setup -- #

d2 = d |> 
  filter(stem_varies,stem != 'komplett') |> 
  mutate(
    transcribed = stringr::str_replace_all(stem, c('x' = 'ks', 'cs' = 'č', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'sz' = 'ß', 's' = 'š', 'ß' = 's', 'ck' = 'kk', 'codec' = 'kodek', 'ch' = 'h')),
    p_back = plogis(log_odds_back)
  )

# d2 |> distinct(stem,stem_freq) |> arrange(stem_freq) |> View()

d3 = d2 |> 
  distinct(transcribed) |> 
  rename(lemma = transcribed)

# -- write -- #

d2 |> 
  write_tsv('~/Documents/Misc/habilitation/dzsungel/dat/dzsungel.tsv')
d3 |> 
  write_tsv('~/Documents/Misc/habilitation/dzsungel/dat/dzsungel_forms.tsv')
