# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(glmmTMB)
library(ggthemes)
library(rstanarm)
library(bayestestR)
library(bridgesampling)
library(performance)
library(sjPlot)
library(patchwork)

# -- read -- #

real_phon = read_tsv('dat/real_words_phon_preds.tsv')
nonwords_phon = read_tsv('dat/nonwords_phon_preds.tsv')
real_sem = read_tsv('dat/real_words_semantic_preds.tsv')
trials_real_words = read_tsv('dat/unfiltered_data_real_word.tsv')
trials_nonwords = read_tsv('dat/unfiltered_data_nonword.tsv')

# -- glm setup -- #

real_phon_b = real_phon |> 
  select(stem,back,front,predicted_loo) |> 
  rename(phonological_model = predicted_loo)

real_sem_b = real_sem |> 
  select(stem,predicted_loo) |> 
  rename(semantic_model = predicted_loo)

real_combined = inner_join(real_phon_b,real_sem_b) |> 
  mutate(
    s_phonological_model = scales::rescale(phonological_model),
    s_semantic_model = scales::rescale(semantic_model)
  )

trials_real_words_combined = left_join(trials_real_words,real_combined) |> 
  mutate(
    s_phonological_model = scales::rescale(phonological_model),
    s_semantic_model = scales::rescale(semantic_model),
    accept = as.double(accept)
  ) |> 
  filter(!is.na(s_semantic_model),!is.na(s_phonological_model))

trials_nonwords_combined = nonwords_phon |> 
  select(stem,transcribed,predicted) |> 
  rename(target = stem) |> 
  right_join(trials_nonwords) |> 
  mutate(
    s_phonological_model = scales::rescale(predicted),
    accept = as.double(accept)
  )

# -- counts -- #

real_phon |> 
  summarise(sum = sum(back) + sum(front))

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

theme_void_box = function(...) {
  theme_void(...) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
}

# real words: phonological MDS coloured by corpus log odds
p1 = real_phon |>
  arrange(log_odds_back) |> 
  ggplot(aes(x = phonological_x, y = phonological_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.9) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'darkred', midpoint = 0,
    name = 'log(back/front)'
  ) +
  labs(
    title = 'MDS: phonological distances, corpus',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# real words: semantic MDS coloured by corpus log odds
p2 = real_sem |>
  arrange(log_odds_back) |> 
  ggplot(aes(x = semantic_x, y = semantic_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.9) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'darkred', midpoint = 0,
    name = 'log(back/front)'
  ) +
  labs(
    title = 'MDS: semantic distances, corpus',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p1 + p2 + plot_layout(guides = 'collect')

ggsave('~/Documents/latex/vh_krr_hun/viz/mds_loocv.pdf', width = 7, height = 3)

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
    low = 'grey', mid = 'white', high = 'darkred', midpoint = 0,
    name = 'predicted\nlog(back/front)'
  ) +
  labs(
    title = 'MDS: phonological distances, nonwords',
    x = 'MDS dimension 1', y = 'MDS dimension 2'
  ) +
  theme_bw()

ggsave('viz/mds_nonwords_phon.png', dpi = 900, width = 7, height = 5)

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
  cbind(back,front) ~ s_phonological_model + s_semantic_model + (1|stem),
  family = binomial,
  data = real_combined
)

fit2 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + (1|stem),
  family = binomial,
  data = real_combined
)

fit3 = glmmTMB(
  cbind(back,front) ~ s_semantic_model + (1|stem),
  family = binomial,
  data = real_combined
)


MuMIn::r.squaredGLMM(fit1)
MuMIn::r.squaredGLMM(fit2)
MuMIn::r.squaredGLMM(fit3)

check_model(fit1)
check_overdispersion(fit1)
check_autocorrelation(fit1)
check_residuals(fit1)

plot(compare_performance(fit1,fit2,fit3,metrics = 'common'))
compare_performance(fit1,fit2,fit3,metrics = 'common') |> 
  select(Name,AIC,BIC,RMSE) |> 
  arrange(AIC) |> 
  knitr::kable(digits = 3)
test_performance(fit1,fit2)
test_performance(fit1,fit3)
test_performance(fit2,fit3)

# -- glm, exp -- #

fit4 = glmmTMB(
  accept ~ s_phonological_model + s_semantic_model + (1|id) + (1|target),
  family = binomial,
  data = trials_real_words_combined
)

fit5 = glmmTMB(
  accept ~ s_phonological_model + (1|id) + (1|target),
  family = binomial,
  data = trials_real_words_combined
)

fit6 = glmmTMB(
  accept ~ s_semantic_model + (1|id) + (1|target),
  family = binomial,
  data = trials_real_words_combined
)

compare_performance(fit4,fit5,fit6,metrics = 'common') |> 
  select(Name,AIC,BIC,R2_marginal,RMSE) |> 
  arrange(AIC)
test_performance(fit4,fit5)
test_performance(fit4,fit6)
test_performance(fit5,fit6)

plots1 = plot_model(fit1, 'pred')
plots4 = plot_model(fit4, 'pred')

wrap_plots(plots1) / wrap_plots(plots4)

broom.mixed::tidy(fit1, conf.int = T)
broom.mixed::tidy(fit4, conf.int = T)

# -- viz glmm -- #

p1 = plot_model(fit1, 'pred', terms = 's_phonological_model') + ylim(0,1) + theme_bw() + xlab('phonological\nsimilarity') + ylab('p(back suffix)') + ggtitle('corpus data') + geom_rug()
p2 = plot_model(fit1, 'pred', terms = 's_semantic_model') + ylim(0,1) + theme_bw() + xlab('semantic\nsimilarity') + ggtitle('') + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

p1 + p2

ggsave('~/Documents/latex/vh_krr_hun/viz/model_predictions_loocv.pdf', width = 5, height = 2.5)


p3 = plot_model(fit4, 'pred', terms = 's_phonological_model') + ylim(0,1) + theme_bw() + xlab('phonological similarity') + ylab('p(back)') + ggtitle('exp data, real words') + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
p4 = plot_model(fit4, 'pred', terms = 's_semantic_model') + ylim(0,1) + theme_bw() + xlab('semantic similarity') + ylab('p(back)') + ggtitle('') + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p5 = plot_model(fit7, 'pred', terms = 's_phonological_model') + ylim(0,1) + theme_bw() + xlab('phonological similarity') + ylab('p(back)') + ggtitle('exp data, nonwords')
