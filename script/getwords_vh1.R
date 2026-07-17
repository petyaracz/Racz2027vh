# fit gcm on nonword data, use latinfrench/germanyiddish as two categories

# -- head -- #

setwd('~/Github/Racz2027vh1')

library(tidyverse)

trials_nonwords = read_tsv('dat/filtered_data_nonword.tsv')   # nonword trial data
racz_rebrus     = read_tsv('dat/racz_rebrus.tsv')   # real word phonological predictions
words = read_tsv('~/Downloads/vh álszavak - Sheet2.tsv')

real_stems = racz_rebrus |> 
  arrange(log_odds_back) |> 
  pull(stem)
  
words2 = words |> 
  select(class,s0) |> 
  mutate(target = s0 |> 
           str_remove('^Ez egy ') |> 
           str_remove('\\.$')
         ) |> 
  select(class,target)
  

words3 = trials_nonwords |> 
  left_join(words2) |> 
  mutate(category = ifelse(class == 'művelt', 'front','back')) |> 
  summarise(
    mean = mean(accept),
    .by = c(target,category)
  ) |> 
  arrange(mean)

words3 |> 
  filter(category == 'back') |> 
  pull(target) |> 
  paste(collapse = ', ')

words3 |> 
  filter(category == 'front') |> 
  pull(target) |> 
  paste(collapse = ', ')

real_stems |> 
  paste(collapse = ', ')
