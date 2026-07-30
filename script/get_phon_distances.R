# Regenerates dat/word_distances.tsv.gz, the phonological distance matrix.
#
# Not part of the main pipeline: the distance matrix ships with the repo, so
# dat_setup.R and fit_krr.R run without this script. It is here to document how
# the distances were produced and to let them be rebuilt from scratch.
#
# Distances are alignment-based, computed by janet over a binary phonological
# feature matrix. janet is not on CRAN:
#   remotes::install_github('petyaracz/JANET')

# -- head -- #

set.seed(1337)
library(tidyverse)
library(janet)
library(here)

# -- functions -- #

# Hungarian orthography to the IPA-like transcription janet is keyed on. This
# reproduces the `transcribed` column of dzsungel.tsv exactly for the current
# word list. The 'ly' rule is inert here (no stem contains 'ly') but is kept
# because it was present when the shipped matrix was built.
transcribeIPA = function(dat) {
  dat |> stringr::str_replace_all(c(
    'x' = 'ks', 'cs' = 'č', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ',
    'ny' = 'ṉ', 'sz' = 'ß', 's' = 'š', 'ß' = 's', 'ck' = 'kk',
    'codec' = 'kodek', 'ch' = 'h', 'ly' = 'j'
  ))
}

# -- read -- #

train = read_tsv(here('dat', 'dzsungel.tsv'))

# segment column plus binary feature columns; empty cells are genuine NAs
features = read.delim(
  here('dat', 'siptar_torkenczy_toth_racz_hungarian.tsv'),
  na.strings = ''
)

# -- transcribe -- #

words = transcribeIPA(train$stem)

# -- run janet -- #

result = run_janet(features, words, gap_penalty = 1.0)

# -- write -- #

write_tsv(result$word_distances, here('dat', 'word_distances.tsv.gz'))
