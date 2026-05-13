# -- head -- #

setwd('~/Github/Racz2027vh/')

library(tidyverse)
library(glue)

# -- read files -- #

path = '~/Gitlab/noun-task/data/'

d = tibble(
  path = glue('{path}{list.files(path)}'),
  start_time = str_extract(path, '(?<=SESSION_).*(?=\\.csv$)')
) |> 
  filter(str_detect(path, 'noun_guy')) |>  # this label is inserted in the filename upon completion
  mutate(
    data = map(path,read_csv)
  )

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
      accept = case_when(
        response == 'f' ~ F,
        response == 'j' ~ T
      ),
      rt = as.double(rt),
      total_time = sum(rt),
    ) |> 
    select(target,accept,i,rt,total_time)

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
    select(id,yob,age,gender,total_time,i,target,accept,rt)
}

# -- format d -- #

d = d |> 
  mutate(
    data2 = map(data, doPart)
  ) |> 
  select(data2) |> 
  unnest(data2) #|>
  # left_join(w)

# -- drop things -- #

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
  relocate(lower_time, .after = rt) |> 
  relocate(upper_time, .after = rt)

glue('{length(unique(unfilt$id))} participants in total')

keep_ids = unfilt |> 
  count(id,accept) |> 
  filter(accept) |> 
  pull(id)

glue('{length(unique(keep_ids))} participants who accepted anything')

# unfilt |> 
#   distinct(id,total_time) |> 
#   ggplot(aes(total_time)) +
#   geom_histogram()

weird_ids = unfilt |> 
  distinct(id,total_time) |> 
  filter(total_time > 1000000) |> 
  pull(id)

unfilt |> 
  filter(id %in% weird_ids) |> 
  ggplot(aes(i,rt)) +
  geom_point() +
  facet_wrap( ~ id)

unfilt |> 
  ggplot(aes(i,rt)) +
  geom_point() +
  geom_hline(aes(yintercept = 4000))

filt = unfilt |> 
  filter(
    total_time > lower_time,
    total_time < upper_time,
    # rt < 4000, # too serious and also I feel like some participants walked away from the screen and got very long RTs and the part-level filter should do away with them
    id %in% keep_ids
  )

# -- write -- #

write_tsv(unfilt, 'dat/unfiltered_data_nonword.tsv')
write_tsv(filt, 'dat/filtered_data_nonword.tsv')

glue('{length(unique(filt$id))} participants remain (23 participants in real-word exp)')
