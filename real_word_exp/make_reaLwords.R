# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

set.seed(1337)

library(tidyverse)
library(patchwork)
library(ggthemes)
library(broom)
library(glue)
library(jsonlite)

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}

# -- read -- #

d = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
f = read_tsv('~/Github/Raczrebrus2024/dat/dat_wide.tsv')
# c = read_tsv('~/Github/published/Racz2024b/dat/tests.tz')

# -- build master list -- #

d2 = d |>
  mutate(
    log_odds_adj = log((back+1)/(front+1)),  
    stem = fct_reorder(stem, log_odds_adj),
    quantile_stem_freq = ntile(stem_freq, 4)
  ) |> 
  arrange(log_odds_adj)

master = d2 |> 
  filter(
    quantile_stem_freq > 1,
    !stem %in% c('mutter','charter','rocker','kadet')
  ) |> 
  slice(1:25,122:146) |> 
  select(-sd_back,-stem_varies,-quantile_stem_freq) |> 
  mutate(transcription = transcribeIPA(stem, 'single'))

# -- word data -- #

words = master |>
  select(stem,transcription,log_odds_back,stem_freq) |>
  rename(log_odds_back_total = log_odds_back)

words2 = f |> 
  filter(
    stem %in% words$stem,
    suffix %in% c('Pl','Dat')
  ) |> 
  mutate(log_odds_back_suffix = log((back+1)/(front+1))) |> 
  select(stem,suffix,llfpm10,form_varies,log_odds_back_suffix)

words = left_join(words,words2)

# -- stimulus, prompt, target, json -- #

# https://www.jspsych.org/latest/plugins/html-keyboard-response/

master = master |> 
  rowwise() |> 
  mutate(
    pl = glue('{stem}ok'),
    dat = glue('{stem}nak')
  ) |> 
  pivot_longer(c(pl,dat), names_to = 'trial_type', values_to = 'target') |> 
  ungroup()

master2 = master |> 
  mutate(
    prompt = case_when(
      trial_type == 'pl' ~ glue('<p>Ez egy {stem}. Azok ott {target}.</p>'),
      trial_type == 'dat' ~ glue('<p>Ez egy {stem}. Elneveztem a kutyámat {target}.</p>')
    ),
    prompt = glue('{prompt}<p><span style="font-size:24px; color:red;">nem: "f"</span>&emsp;<span style="font-size:24px; color:green;">igen: "j"</span></p>'),
    stimulus = glue('<p style="font-size:48px;">{target}</p>'),
    choices = list(c('f','j'))
  )

# -- write -- #

write_tsv(master2, 'dat/master.tsv')
write_tsv(words, 'dat/words.tsv')
stim = master2 |> 
  toJSON(pretty = TRUE)
stim = paste0('stim = ', stim)
write_lines(stim, 'dat/stim.js')
write_lines(stim, '~/Github/Pavlovia/noun_task/stim.js')