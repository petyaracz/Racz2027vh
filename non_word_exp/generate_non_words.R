# -- head -- #

set.seed(1337)

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(googlesheets4)

# -- fun -- #

makeWord = function(df) {
  attempt = ''
  tries = 0
  while (attempt == '') {
    tries = tries + 1
    if (tries > 1000) { warning('Too many tries'); return(NA_character_) }
    c1 = sample(df$onset,  1)
    c2 = sample(df$vowel,  1)
    c3 = sample(df$middle, 1)
    c4 = sample(df$coda,   1)
    out = paste0(c1, c2, c3, 'e', c4)
    if (!out %in% dictionary$stem) attempt = out
  }
  attempt
}

makeNonWords = function(dat,n_per_bin){
  dat |> 
  group_by(lo_bins) |> 
  nest() |> 
  mutate(
    nonwords = map(data, \(df) replicate(n_per_bin, makeWord(df)))
  ) |> 
  select(lo_bins, nonwords) |> 
  unnest(nonwords) |> 
  ungroup() |> 
  distinct(lo_bins,nonwords) |> 
  arrange(lo_bins)
}
  
# -- read -- #

d = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')

# -- makewords -- #

dictionary = d |> 
  mutate(
    log_odds_adj = log((back+1)/(front+1)),
    lo_bins = ntile(log_odds_adj, 3),
    o_m = round(log10(stem_freq)),
    o_m_n = round(log(stem_freq)),
    coda = str_extract(stem, '[^aáeéiíoóöőuúüű]+$'),
    onset = str_extract(stem, '^[^aáeéiíoóöőuúüű]+'),
    onset = ifelse(is.na(onset), '', onset),
    thing = str_remove(stem, paste0('^',onset)),
    vowel = str_extract(thing, '^.'),
    thing2 = str_remove(thing, paste0('^',vowel)),
    middle = str_extract(thing2, '^[^aáeéiíoóöőuúüű]+'),
    middle = ifelse(is.na(middle), '', middle),
    e = 'e'
  ) |> 
  select(stem_freq,o_m,o_m_n,lo_bins,stem,onset,vowel,middle,e,coda,log_odds_adj)

# -- count stuff -- #

dictionary |> 
  count(lo_bins,coda) |> 
  arrange(lo_bins,-n) |> 
  filter(lo_bins == 1 | lo_bins == 3) |> 
  slice(1:10, .by = lo_bins)
  
# -- lengthen it -- #

dictionary_2 = dictionary |> 
  uncount(stem_freq)

dictionary_3 = dictionary |> 
  uncount(o_m)

dictionary_4 = dictionary |> 
  uncount(o_m_n)

# -- bin the whole thing and look at subsets -- #

# Generate n nonwords per bin
n_per_bin = 20

nonwords = dictionary |> 
  makeNonWords(n_per_bin)
# nonwords_2 = dictionary_2 |> 
#   makeNonWords(n_per_bin)
# nonwords_3 = dictionary_3 |> 
#   makeNonWords(n_per_bin)
# nonwords_4 = dictionary_4 |> 
#   makeNonWords(n_per_bin)

# -- woowee zoowee -- #

pull(nonwords,nonwords)
# pull(nonwords_2,nonwords)
# pull(nonwords_3,nonwords)
# pull(nonwords_4,nonwords)

# -- generate larger batch -- #

muvelt = '(n|tt|ns|x|m)$'
bizalmas = '(r|sz|k|c|l)$'

words = makeNonWords(dictionary, 1000) |> 
  filter(
    lo_bins %in% c(1,3),
    (lo_bins == 1 & str_detect(nonwords, muvelt)) | (lo_bins == 3 & str_detect(nonwords, bizalmas))
         ) |> 
  mutate(
    class = case_when(
      lo_bins == min(lo_bins) ~ 'művelt',
      lo_bins == max(lo_bins) ~ 'bizalmas'
    ),
    s0 = paste0('Ez egy ', nonwords, '.'),
    s1 = paste0('Azok ott ', nonwords, 'ok.'),
    s2 = paste0('Elneveztem a kutyámat ', nonwords, 'nak.')
  ) |> 
  select(class,s0,s1,s2)

# -- write -- #

# write_sheet(words, 'https://docs.google.com/spreadsheets/d/1U0HUTrINAZLPFPIHse-4yVAa_xlmwozxRqKCYYLbz9E/edit?usp=sharing', 'Sheet1')
