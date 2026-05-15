library(tidyverse)

s_dist = read_csv('dat/semantic_distances.csv')

s_dist = s_dist |> 
  mutate(phon_dist = semantic_distance)
s_dist$semantic_distance = NULL
s_dist2 = tibble(word1 = unique(s_dist$word1),word2 = unique(s_dist$word1)) |> 
  mutate(phon_dist = 0)
s_dist3 = tibble(word1 = s_dist$word2, word2 = s_dist$word1, phon_dist = s_dist$phon_dist)
s_dist_final = bind_rows(s_dist,s_dist2,s_dist3)

write_tsv(s_dist_final, 'dat/semantic_distances_ignore_colname.tsv')
