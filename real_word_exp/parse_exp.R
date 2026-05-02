# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

library(tidyverse)
library(glue)

# -- read files -- #

path = '~/Github/Pavlovia/noun_task/data/'

d = tibble(
  path = glue('{path}{list.files(path)}'),
  start_time = str_extract(path, '(?<=SESSION_).*(?=\\.csv$)')
) |> 
  filter(str_detect(path, 'noun_guy')) |>  # this label is inserted in the filename upon completion
  mutate(
    data = map(path,read_csv)
  )

w = read_tsv('dat/word_metadata.tsv')

# -- add info -- #

doPart = function(dat){
  observations = dat |> 
    filter(
      trial_type == 'html-keyboard-response',
      str_detect(stimulus, '(\\+|narancs|billentyűzet|gyorsan|türelmet)', negate = T), # gaze fixes and practice trials and intro and outro
           ) |>
    arrange(trial_index) |> 
    mutate(
      i = 1:n(),
      target = str_extract(stimulus, '(?<=<p style="font-size:48px;">).*(?=</p>)'),
      suffix = case_when(
        str_detect(target, 'nak') ~ 'Dat',
        str_detect(target, 'ok') ~ 'Pl'
        ),
      stem = case_when(
        suffix == 'Dat' ~ str_replace(target, 'nak$', ''),
        suffix == 'Pl' ~ str_replace(target, 'ok$', '')
      ),
      accept = case_when(
        response == 'f' ~ F,
        response == 'j' ~ T
      ),
      rt = as.double(rt),
      total_time = sum(rt),
    ) |> 
    select(stem,target,suffix,accept,i,rt,total_time)

  metadata = dat |> 
    filter(trial_type == 'survey-text') |> 
    pull(response)
  
  observations |> 
    mutate(
      id = str_extract(metadata, '(?<=\\{\\\"Q0\\\"\\:\\\").*(?=\\\",\\\"Q1)'),
      yob = str_extract(metadata, '(?<=Q1\\\"\\:\\\")[0-9]+') |> as.double(),
      gender = str_extract(metadata, '(?<=Q2\\\"\\:\\\").*(?=\\\"\\})'),
      age = 2024 - yob
    ) |> 
    select(id,yob,age,gender,total_time,i,stem,target,suffix,accept,rt)
}

# -- format d -- #

d = d |> 
  mutate(
    data2 = map(data, doPart)
  ) |> 
  select(data2) |> 
  unnest(data2) |> 
  left_join(w)

# -- drop things -- #

# https://aspredicted.org/hyzd-xj7r.pdf
# trials over 4s excluded, participants faster / slower than median completion time +- 3 mean absolute deviations excluded, participants rejecting every form excluded

unfilt = d |> 
  summarise(
    median_time = median(total_time),
    mad_time = mad(total_time)
            ) |> 
  mutate(
    upper_time = median_time + 3 * mad_time,
    lower_time = median_time - 3 * mad_time
  ) |>
  select(upper_time,lower_time) |> 
  bind_cols(d) |> 
  relocate(lower_time, .after = log_odds_back_suffix) |> 
  relocate(upper_time, .after = log_odds_back_suffix)

keep_ids = unfilt |> 
  count(id,accept) |> 
  filter(accept) |> 
  pull(id)

filt = unfilt |> 
  filter(
    total_time > lower_time,
    total_time < upper_time,
    rt < 4000,
    id %in% keep_ids
  )

# -- write -- #

write_tsv(unfilt, 'dat/unfiltered_data.tsv')
write_tsv(filt, 'dat/filtered_data.tsv')