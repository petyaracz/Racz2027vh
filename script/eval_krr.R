# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(glmmTMB)
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

# -- bglm, aggregate (cbind) -- #

b1 = stan_glm(
  cbind(back, front) ~ s_phonological_model + s_semantic_model,
  family = binomial,
  data = real_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b1.csv")
)

b2 = stan_glm(
  cbind(back, front) ~ s_phonological_model,
  family = binomial,
  data = real_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b2.csv")
)

b3 = stan_glm(
  cbind(back, front) ~ s_semantic_model,
  family = binomial,
  data = real_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b3.csv")
)

loo1 = kfold(b1, K = 10, seed = 42)
loo2 = kfold(b2, K = 10, seed = 42)
loo3 = kfold(b3, K = 10, seed = 42)

loo_compare(loo1, loo2, loo3) |> 
  as_tibble() |> 
  write_tsv('corpus_loo.tsv')

# elpd_diff se_diff  
# b1       0.0       0.0
# b3  -87437.9   44205.0
# b2 -249503.8  141705.8

ml1 = bridge_sampler(b1, silent = TRUE)
ml2 = bridge_sampler(b2, silent = TRUE)
ml3 = bridge_sampler(b3, silent = TRUE)

# look at log marg likelihood
print(ml1)
print(ml2)
print(ml3)
# it's just this big

# check estimation error
error_measures(ml1)
error_measures(ml2)
error_measures(ml3)

bayesfactor_models(b1, b2, denominator = b2)
bayesfactor_models(b1, b3, denominator = b3)
bayesfactor_models(b2, b3, denominator = b3)
# these are big because counts are big

posterior_interval(b1, prob = 0.95)
posterior_interval(b2, prob = 0.95)
posterior_interval(b3, prob = 0.95)

# -- bglmer, trial-level -- #

b4 = stan_glmer(
  accept ~ s_phonological_model + s_semantic_model + (1 | id) + (1 | target),
  family = binomial,
  data = trials_real_words_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b4.csv"),
  iter = 4000
)

b4b = stan_glmer(
  accept ~ s_phonological_model + s_semantic_model + (1 + s_phonological_model | id) + (1 | target),
  family = binomial,
  data = trials_real_words_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b4b.csv"),
  iter = 4000
)

b4c = stan_glmer(
  accept ~ s_phonological_model + s_semantic_model + (1 + s_semantic_model | id) + (1 | target),
  family = binomial,
  data = trials_real_words_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b4c.csv"),
  iter = 4000
)

# b4d = stan_glmer(
#   accept ~ s_phonological_model + s_semantic_model + (1 + s_phonological_model + s_semantic_model | id) + (1 | target),
#   family = binomial,
#   data = trials_real_words_combined,
#   seed = 42,
#   cores = 4,
#   diagnostic_file = file.path(tempdir(), "b4d.csv"),
#   iter = 4000,
#   control = list(adapt_delta = 0.99, max_treedepth = 15)
# ) # no

b5 = stan_glmer(
  accept ~ s_phonological_model + (1 | id) + (1 | target),
  family = binomial,
  data = trials_real_words_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b5.csv"),
  iter = 4000
)

b6 = stan_glmer(
  accept ~ s_semantic_model + (1 | id) + (1 | target),
  family = binomial,
  data = trials_real_words_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b6.csv"),
  iter = 4000
)

loo4 = loo(b4)
loo5 = loo(b5)
loo6 = loo(b6)

loo4b = loo(b4b)
loo4c = loo(b4c)
# loo4d = loo(b4d)

loo_compare(loo4, loo5, loo6) |> 
  as_tibble() |> 
  write_tsv('exp_loo.tsv')

# elpd_diff se_diff
# b4  0.0       0.0   
# b5 -2.5       2.9   
# b6 -3.0       3.2   

loo_compare(loo4,loo4b,loo4c)
# elpd_diff se_diff
# b4b   0.0       0.0  
# b4c -12.8       5.4  
# b4  -18.7       5.8  

ml4 = bridge_sampler(b4, silent = TRUE)
ml5 = bridge_sampler(b5, silent = TRUE)
ml6 = bridge_sampler(b6, silent = TRUE)

# look at log marg likelihood
print(ml4)
print(ml5)
print(ml6)

# check estimation error
error_measures(ml4)
error_measures(ml5)
error_measures(ml6) # < 10%

bayesfactor_models(b4, b5, denominator = b5) # b4, BF = 8e+04
bayesfactor_models(b4, b6, denominator = b6) # b4, BF = 4e+06
bayesfactor_models(b6, b5, denominator = b6) # b5, BF = 49

posterior_interval(b4, prob = 0.95)
posterior_interval(b5, prob = 0.95)
posterior_interval(b6, prob = 0.95)

# -- nonwords -- #

b7 = stan_glmer(
  accept ~ s_phonological_model + (1 | id) + (1 | target),
  family = binomial,
  data = trials_nonwords_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b7.csv"),
  iter = 4000
)

b7b = stan_glmer(
  accept ~ s_phonological_model + (1 + s_phonological_model | id) + (1 | target),
  family = binomial,
  data = trials_nonwords_combined,
  seed = 42,
  cores = 4,
  diagnostic_file = file.path(tempdir(), "b7b.csv"),
  iter = 4000
)

loo7 = loo(b7)
loo7b = loo(b7b)

loo_compare(loo7,loo7b)

# -- compare var -- #

VarCorr(fit4)
posterior_interval(b4, regex_pars = "Sigma")

# -- viz stan models -- #

p1 = plot_model(b1, 'pred', terms = 's_phonological_model') + ylim(0,1) + theme_bw() + xlab('phonological similarity') + ylab('p(back)') + ggtitle('corpus data') + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
p2 = plot_model(b1, 'pred', terms = 's_semantic_model') + ylim(0,1) + theme_bw() + xlab('semantic similarity') + ylab('p(back)') + ggtitle('') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
p3 = plot_model(b4, 'pred', terms = 's_phonological_model') + ylim(0,1) + theme_bw() + xlab('phonological similarity') + ylab('p(back)') + ggtitle('exp data, real words') + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
p4 = plot_model(b4, 'pred', terms = 's_semantic_model') + ylim(0,1) + theme_bw() + xlab('semantic similarity') + ylab('p(back)') + ggtitle('') + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p5 = plot_model(b7, 'pred', terms = 's_phonological_model') + ylim(0,1) + theme_bw() + xlab('phonological similarity') + ylab('p(back)') + ggtitle('exp data, nonwords')

(p1 + p2) / (p3 + p4) / (p5 + plot_spacer())
ggsave('viz/model_predictions.png', dpi = 900, width = 7, height = 7)

posterior_interval(b1, prob = 0.95)
posterior_interval(b4, prob = 0.95, regex_pars = '_model')
posterior_interval(b7, prob = 0.95, regex_pars = '_model')

# within-model: is phonology > semantics?
brms::hypothesis(b1, "s_phonological_model > s_semantic_model")
brms::hypothesis(b4, "s_phonological_model > s_semantic_model")
