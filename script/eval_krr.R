# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(glmmTMB)
library(performance)
library(sjPlot)

# -- read -- #

real_phon = read_tsv('dat/real_words_phon_preds.tsv')
nonwords_phon = read_tsv('dat/nonwords_phon_preds.tsv')
real_sem = read_tsv('dat/real_words_semantic_preds.tsv')
trials_real_words = read_tsv('dat/filtered_data_real_word.tsv')

# -- glm setup -- #

real_phon_b = real_phon |> 
  select(stem,back,front,predicted_loo) |> 
  rename(phonological_model = predicted_loo)

real_sem_b = real_sem |> 
  select(stem,predicted_loo) |> 
  rename(semantic_model = predicted_loo)

real_combined = inner_join(real_phon_b,real_sem_b)

trials_real_words_combined = left_join(trials_real_words,real_combined)

# -- lang cats -- #

real_phon = real_phon |> 
  mutate(
    language2 = ifelse(language %in% c('de','en','fr','yi','la'), language, 'other') |> 
      fct_relevel('yi','de','en','fr','la','other')
  )

real_sem = real_sem |> 
  mutate(
    language2 = ifelse(language %in% c('de','en','fr','yi','la'), language, 'other') |> 
      fct_relevel('yi','de','en','fr','la','other')
  )

# -- MDS visualisations -- #

# real words: phonological MDS coloured by corpus log odds
real_phon |>
  ggplot(aes(x = phonological_x, y = phonological_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
    name = 'log(back/front)'
  ) +
  labs(
    title = 'MDS: phonological distances, real words',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_bw()

ggsave('viz/mds_real_phon.png', dpi = 900, width = 7, height = 5)

# etymology
real_phon |>
  ggplot(aes(x = phonological_x, y = phonological_y)) +
  stat_density_2d(bins = 5, linewidth = 0.8) +
  labs(
    title = 'MDS: phonological distances, real words',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_bw() +
  facet_wrap( ~ language2)

ggsave('viz/mds_real_phon_labels.png', dpi = 900, width = 7, height = 5)

# nonwords: phonological MDS coloured by KRR prediction
nonwords_phon |>
  ggplot(aes(x = phonological_x, y = phonological_y, fill = predicted)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
    name = 'predicted\nlog(back/front)'
  ) +
  labs(
    title = 'MDS: phonological distances, nonwords',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_bw()

ggsave('viz/mds_nonwords_phon.png', dpi = 900, width = 7, height = 5)

# real words: semantic MDS coloured by corpus log odds
real_sem |>
  ggplot(aes(x = semantic_x, y = semantic_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
    name = 'log(back/front)'
  ) +
  labs(
    title = 'MDS: semantic distances, real words',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_bw()

ggsave('viz/mds_real_sem.png', dpi = 900, width = 7, height = 5)

# etymology
real_sem |> 
  ggplot(aes(x = semantic_x, y = semantic_y)) +
  stat_density_2d(bins = 5, linewidth = 0.8) +
  labs(
    title = 'MDS: phonological distances, real words',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_bw() +
  facet_wrap( ~ language2)

ggsave('viz/mds_real_sem_labels.png', dpi = 900, width = 7, height = 5)

# -- observed x predicted visualisations -- #

# real words: phonological KRR LOO
real_phon |>
  ggplot(aes(log_odds_back, predicted_loo)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_smooth() +
  theme_bw() +
  xlab('observed log(back/front)') +
  ylab('predicted log(back/front)') +
  ggtitle('KRR LOO: real words, phonological distance')

ggsave('viz/obs_pred_real_phon.png', dpi = 900, width = 6.5, height = 4.5)

# real words: semantic KRR LOO
real_sem |>
  ggplot(aes(log_odds_back, predicted_loo)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_smooth() +
  theme_bw() +
  xlab('observed log(back/front)') +
  ylab('predicted log(back/front)') +
  ggtitle('KRR LOO: real words, semantic distance')

ggsave('viz/obs_pred_real_sem.png', dpi = 900, width = 6.5, height = 4.5)

# nonwords: phonological KRR
nonwords_phon |>
  ggplot(aes(log_odds_back, predicted)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_smooth() +
  theme_bw() +
  xlab('observed log(back/front)') +
  ylab('predicted log(back/front)') +
  ggtitle('KRR: nonwords, phonological distance')

ggsave('viz/obs_pred_nonwords_phon.png', dpi = 900, width = 6.5, height = 4.5)

# -- correlation tests -- #

with(real_phon, cor.test(predicted_loo, log_odds_back))
with(real_sem, cor.test(predicted_loo, log_odds_back))
with(nonwords_phon, cor.test(predicted, log_odds_back))

# -- glm, corpus -- #

fit1 = glmmTMB(
  cbind(back,front) ~ phonological_model + semantic_model,
  family = binomial,
  data = real_combined
)

fit2 = glmmTMB(
  cbind(back,front) ~ phonological_model,
  family = binomial,
  data = real_combined
)

fit3 = glmmTMB(
  cbind(back,front) ~ semantic_model,
  family = binomial,
  data = real_combined
)

compare_performance(fit1,fit2,fit3,metrics = 'common')
test_performance(fit1,fit2)
test_performance(fit1,fit3)
test_performance(fit2,fit3)

# -- glm, exp -- #

trials_real_words_combined$accept = as.double(trials_real_words_combined$accept)

fit4 = glmmTMB(
  accept ~ phonological_model + semantic_model + (1|id) + (1|target),
  family = binomial,
  data = trials_real_words_combined
)

fit5 = glmmTMB(
  accept ~ phonological_model + (1|id) + (1|target),
  family = binomial,
  data = trials_real_words_combined
)

fit6 = glmmTMB(
  accept ~ semantic_model + (1|id) + (1|target),
  family = binomial,
  data = trials_real_words_combined
)

compare_performance(fit4,fit5,fit6,metrics = 'common')
test_performance(fit4,fit5)
test_performance(fit4,fit6)
test_performance(fit5,fit6)

plot_model(fit1, 'pred')
plot_model(fit4, 'pred')

summary(fit1)
summary(fit4)
