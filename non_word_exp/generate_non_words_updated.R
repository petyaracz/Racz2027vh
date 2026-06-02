set.seed(1337)

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(googlesheets4)

# -- fun -- #

makeWord = function(df, fixed_coda) {
  attempt = ''
  tries = 0
  ending = paste0('e', fixed_coda)
  while (attempt == '') {
    tries = tries + 1
    if (tries > 1000) { warning('Too many tries'); return(NULL) }
    c1 = sample(df$onset,  1)
    c2 = sample(df$vowel,  1)
    c3 = sample(df$middle, 1)
    out = paste0(c1, c2, c3, ending)
    if (!out %in% dictionary$stem) attempt = out
  }
  tibble(nonword = attempt, c1 = c1, c2 = c2, c3 = c3, coda = fixed_coda, ending = ending)
}

makeNonWordsFixed = function(dat, n_per_ending, endings_by_bin) {
  results = list()
  for (bin in names(endings_by_bin)) {
    bin_int = as.integer(bin)
    df = dat |> filter(lo_bins == bin_int)
    for (coda in endings_by_bin[[bin]]) {
      words = list()
      seen  = character(0)
      tries_outer = 0
      while (length(words) < n_per_ending) {
        tries_outer = tries_outer + 1
        if (tries_outer > n_per_ending * 200) {
          warning(paste('Could not generate enough distinct nonwords for bin', bin, 'coda', coda))
          break
        }
        result = makeWord(df, coda)
        if (!is.null(result) && !result$nonword %in% seen) {
          seen  = c(seen, result$nonword)
          words = c(words, list(result))
        }
      }
      bin_coda_tbl = bind_rows(words) |>
        mutate(lo_bins = bin_int)
      results = c(results, list(bin_coda_tbl))
    }
  }
  bind_rows(results)
}

# -- read -- #

d = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')

# -- make dictionary -- #

dictionary = d |>
  mutate(
    log_odds_adj = log((back + 1) / (front + 1)),
    lo_bins      = ntile(log_odds_adj, 3),
    o_m          = round(log10(stem_freq)),
    o_m_n        = round(log(stem_freq)),
    coda         = str_extract(stem, '[^aáeéiíoóöőuúüű]+$'),
    onset        = str_extract(stem, '^[^aáeéiíoóöőuúüű]+'),
    onset        = ifelse(is.na(onset), '', onset),
    thing        = str_remove(stem, paste0('^', onset)),
    vowel        = str_extract(thing, '^.'),
    thing2       = str_remove(thing, paste0('^', vowel)),
    middle       = str_extract(thing2, '^[^aáeéiíoóöőuúüű]+'),
    middle       = ifelse(is.na(middle), '', middle),
    e            = 'e'
  ) |>
  filter(lo_bins %in% c(1, 3)) |>
  select(stem_freq, o_m, o_m_n, lo_bins, stem, onset, vowel, middle, e, coda, log_odds_adj)

# -- define endings -- #

muvelt_endings   = c('n', 'tt', 'ns', 'x', 'm')
bizalmas_endings = c('r', 'sz', 'k', 'c', 'l')

endings_by_bin = list(
  '1' = muvelt_endings,
  '3' = bizalmas_endings
)

# -- generate nonwords -- #

n_per_ending = 25   # total = n_per_ending * 5 endings * 2 bins = 200

nonwords = makeNonWordsFixed(dictionary, n_per_ending, endings_by_bin)

# -- add class and tidy -- #

nonwords = nonwords |>
  mutate(
    class = case_when(
      lo_bins == 1 ~ 'művelt',
      lo_bins == 3 ~ 'bizalmas'
    )
  ) |>
  select(lo_bins, class, nonword, coda, c1, c2, c3, ending)

examples = nonwords |> 
  mutate(
    s0 = paste0('Ez egy ', nonword, '.'),
    s1 = paste0('Azok ott ', nonword, 'ok.'),
    s2 = paste0('Elneveztem a kutyámat ', nonword, 'nak.')
  ) |> 
  select(class,ending,s0,s1,s2)

# -- write -- #

write_sheet(examples, 'https://docs.google.com/spreadsheets/d/1U0HUTrINAZLPFPIHse-4yVAa_xlmwozxRqKCYYLbz9E/edit?usp=sharing', 'Sheet2')
