# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(glmmTMB)
library(ggthemes)
library(ggeffects)
library(performance)
library(sjPlot)
library(patchwork)

# -- read -- #

lo_preds = read_tsv('dat/real_words_krr_lo_preds.tsv')
label_preds = read_tsv('dat/real_words_krr_label_preds.tsv')

# -- glm setup -- #

lo_preds = lo_preds |> 
  mutate(
    category_educated_other = fct_relevel(category_educated_other, 'educated'),
    s_phonological_model = scales::rescale(predicted_loo_phon),
    s_semantic_model = scales::rescale(predicted_loo_sem)
  )

label_preds = label_preds |> 
  mutate(
    s_phonological_model = scales::rescale(predicted_loo_phon),
    s_semantic_model = scales::rescale(predicted_loo_sem)
  )

######################################################
# lo predictions
######################################################

fit0 = glmmTMB(
  cbind(back,front) ~ 1 + (1|stem),
  family = binomial,
  data = lo_preds
)

fit1 = glmmTMB(
  cbind(back,front) ~ category_educated_other + (1|stem),
  family = binomial,
  data = lo_preds
)

fit2 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + (1|stem),
  family = binomial,
  data = lo_preds
)

fit3 = glmmTMB(
  cbind(back,front) ~ s_semantic_model + (1|stem),
  family = binomial,
  data = lo_preds
)

fit4 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + s_semantic_model + (1|stem),
  family = binomial,
  data = lo_preds
)

fit5 = glmmTMB(
  cbind(back,front) ~ category_educated_other + s_phonological_model + s_semantic_model + (1|stem),
  family = binomial,
  data = lo_preds
)

# -- diagnostics -- #

check_model(fit5)
check_model(fit4)
check_model(fit3)
check_model(fit2)
check_model(fit1)

# -- tidy table -- #

# basic indices
perf = compare_performance(fit0, fit1, fit2, fit3, fit4, fit5, metrics = "common") |>
  as.data.frame() |>
  select(Name, AIC, BIC)

r21 = MuMIn::r.squaredGLMM(fit1)[1,1]
r22 = MuMIn::r.squaredGLMM(fit2)[1,1]
r23 = MuMIn::r.squaredGLMM(fit3)[1,1]
r24 = MuMIn::r.squaredGLMM(fit4)[1,1]
r25 = MuMIn::r.squaredGLMM(fit5)[1,1]

# note that test_bf uses frequentist approximation
# BF ≈ exp((BIC_ref - BIC_model)/2)

get_bf_row2 = function(test_result, label, col_name) {
  df = as.data.frame(test_result)
  data.frame(Name = label, val = df$BF[2]) |>
    rename(!!col_name := val)
}

get_stat_row2 = function(test_result, label, stat_col, col_name) {
  df = as.data.frame(test_result)
  data.frame(Name = label, val = df[[stat_col]][2]) |>
    rename(!!col_name := val)
}

# fit1 as numerator, compared against fit2 and fit3
bf_fit4_vs = bind_rows(
  get_bf_row2(test_bf(fit0, fit4), "fit0", "BF_fit4_vs"),
  get_bf_row2(test_bf(fit1, fit4), "fit1", "BF_fit4_vs"), 
  get_bf_row2(test_bf(fit2, fit4), "fit2", "BF_fit4_vs"),
  get_bf_row2(test_bf(fit3, fit4), "fit3", "BF_fit4_vs"),
  get_bf_row2(test_bf(fit5, fit4), "fit5", "BF_fit4_vs"),
) |> 
  mutate(
    log_BF_fit4_vs = log(BF_fit4_vs)
  ) |> 
  select(Name,log_BF_fit4_vs)

lr_fit4_vs = bind_rows(
  get_stat_row2(test_likelihoodratio(fit2, fit4), "fit2", "Chi2", "Chisq_fit4_vs"),
  get_stat_row2(test_likelihoodratio(fit3, fit4), "fit3", "Chi2", "Chisq_fit4_vs"),
  get_stat_row2(test_likelihoodratio(fit5, fit4), "fit5", "Chi2", "Chisq_fit4_vs")
)

# join together
summary_table = perf |>
  left_join(bf_fit4_vs, by = "Name") |>
  left_join(lr_fit4_vs, by = "Name") |>
  bind_cols(r2m = c(NA,r21,r22,r23,r24,r25))

summary_table |> 
  select(Name,AIC,BIC,r2m,log_BF_fit4_vs,Chisq_fit4_vs) |> 
  knitr::kable(digits = 2, 'latex') |> 
  write_lines('glmm_comparisons_lo_preds.txt')

# -- coefs -- #

broom.mixed::tidy(fit4, conf.int = T) |> 
  filter(effect == 'fixed') |> 
  select(term,estimate,std.error,conf.low,conf.high) |> 
  knitr::kable(digits = 2, 'latex') |> 
  write_lines('best_glmm_coefs_lo_preds.txt')

cor.test(lo_preds$predicted_loo_phon,lo_preds$predicted_loo_sem)

######################################################
# label predictions
######################################################

lfit0 = glmmTMB(
  category_educated ~ 1,
  family = binomial,
  data = label_preds
)

lfit2 = glmmTMB(
  category_educated ~ s_phonological_model,
  family = binomial,
  data = label_preds
)

lfit3 = glmmTMB(
  category_educated ~ s_semantic_model,
  family = binomial,
  data = label_preds
)

lfit4 = glmmTMB(
  category_educated ~ s_phonological_model + s_semantic_model,
  family = binomial,
  data = label_preds
)

# -- diagnostics -- #

check_model(lfit4)
check_model(lfit3)
check_model(lfit2)
check_model(lfit0)

# -- tidy table -- #

# basic indices
lperf = compare_performance(lfit0, lfit2, lfit3, lfit4, metrics = "common") |>
  as.data.frame() |>
  select(Name, AIC, BIC)

lr2 = r2(lfit2)
lr3 = r2(lfit3)
lr4 = r2(lfit4)

bf_lfit4_vs = bind_rows(
  get_bf_row2(test_bf(lfit4, lfit2), "lfit2", "BF_lfit4_vs"), 
  get_bf_row2(test_bf(lfit4, lfit3), "lfit3", "BF_lfit4_vs")
) |> 
  mutate(
    log_BF_lfit4_vs = log(BF_lfit4_vs)
  ) |> 
  select(Name,log_BF_lfit4_vs)

log(get_bf_row2(test_bf(lfit3,lfit2), 'lfit3', 'BF_lfit2_vs')[2])
log(get_bf_row2(test_bf(lfit0,lfit3), 'lfit3', 'BF_lfit2_vs')[2])

lr_lfit4_vs = bind_rows(
  get_stat_row2(test_likelihoodratio(lfit2, lfit4), "lfit2", "Chi2", "Chisq_lfit4_vs"),
  get_stat_row2(test_likelihoodratio(lfit3, lfit4), "lfit3", "Chi2", "Chisq_lfit4_vs")
)

# join together
l_summary_table = lperf |>
  left_join(bf_lfit4_vs, by = "Name") |>
  left_join(lr_lfit4_vs, by = "Name") |>
  bind_cols(r2m = c(NA,lr2[[1]],lr3[[1]],lr4[[1]]))

l_summary_table |> 
  knitr::kable(digits = 2, 'latex') |> 
  write_lines('glmm_comparisons_label_preds.txt')

# -- coefs -- #

broom.mixed::tidy(lfit2, conf.int = T) |> 
  filter(effect == 'fixed') |> 
  select(term,estimate,std.error,conf.low,conf.high) |> 
  knitr::kable(digits = 2, 'latex') |> 
  write_lines('best_glmm_coefs_label_preds.txt')
