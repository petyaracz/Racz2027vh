# -- head -- #

setwd('~/Github/Racz2027vh/')

set.seed(1337)

library(tidyverse)
library(googlesheets4)
library(glue)
library(jsonlite)

# -- fun -- #

# -- read -- #

words = read_sheet('https://docs.google.com/spreadsheets/d/1U0HUTrINAZLPFPIHse-4yVAa_xlmwozxRqKCYYLbz9E/edit?usp=sharing', 'Sheet2')

# -- check -- #

words |> 
  filter(!is.na(akkor)) |> 
  count(ending)

# -- stimulus, prompt, target, json -- #

# https://www.jspsych.org/latest/plugins/html-keyboard-response/

master = words |> 
  filter(!is.na(akkor)) |> # !!!! grrr
  select(-h,-én,-akkor) |> 
  pivot_longer(-c(class,ending,s0)) |> 
  mutate(
    target = str_extract(s0, '(?<=Ez egy )[^ ]+(?=\\.$)'),
    trial_type = ifelse(str_detect(value, 'ok$'), 'pl', 'dat'),
    prompt = glue('<p>{s0} {value}</p>'),
    prompt = glue('{prompt}<p><span style="font-size:24px; color:red;">nem: "f"</span>&emsp;<span style="font-size:24px; color:green;">igen: "j"</span></p>'),
    stimulus = glue('<p style="font-size:48px;">{target}</p>'),
    choices = list(c('f','j'))
  )

# -- write -- #

write_tsv(master, 'dat/master.tsv')
stim = master |> 
  toJSON(pretty = TRUE)
stim = paste0('stim = ', stim)
write_lines(stim, 'dat/stim.js')
write_lines(stim, '~/Gitlab/noun-task/stim.js')
