# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)

# -- read -- #

trials_nonwords = read_tsv('dat/unfiltered_data_nonword.tsv')
trials_real_words = read_tsv('dat/unfiltered_data_real_word.tsv')

# -- real words -- #

trials_real_words |> 
  distinct(id) |> 
  nrow()

trials_real_words |> 
  filter(accept) |> 
  distinct(id) |> 
  nrow()

n_long = trials_real_words |> 
  filter(rt > 4000) |> 
  nrow()

n_long / nrow(trials_real_words)

# -- non words -- #

trials_nonwords |> 
  distinct(id) |> 
  nrow()

trials_nonwords |> 
  filter(accept) |> 
  distinct(id) |> 
  nrow()

n_long = trials_nonwords |> 
  filter(rt > 4000) |> 
  nrow()

n_long / nrow(trials_nonwords)
