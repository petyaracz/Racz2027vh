# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(glmmTMB)
library(ggthemes)
library(performance)
library(sjPlot)
library(patchwork)

# -- read -- #

combined = read_tsv('dat/real_words_both_preds.tsv')

# -- glm setup -- #

combined = combined |> 
  mutate(
    s_phonological_model = scales::rescale(predicted_loo_phon),
    s_semantic_model = scales::rescale(predicted_loo_sem)
  )

# -- counts -- #

combined |> 
  summarise(sum = sum(back) + sum(front))

# -- glm, corpus -- #

fit1 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + s_semantic_model + (1|stem),
  family = binomial,
  data = combined
)

fit2 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + (1|stem),
  family = binomial,
  data = combined
)

fit3 = glmmTMB(
  cbind(back,front) ~ s_semantic_model + (1|stem),
  family = binomial,
  data = combined
)

# -- diagnostics -- #

check_model(fit1)
check_overdispersion(fit1)
check_autocorrelation(fit1)
check_residuals(fit1)

# -- hmm -- #

# hmm
BIC(fit1) - BIC(fit2)
# exp((BIC_ref - BIC_model)/2)
nobs(fit1); nobs(fit2); nobs(fit3)
logLik(fit1); logLik(fit2); logLik(fit3)
BIC(fit1); BIC(fit2); BIC(fit3)

# -- tidy table -- #

# basic indices
perf = compare_performance(fit1, fit2, fit3, metrics = "common") |>
  as.data.frame() |>
  select(Name, AIC, BIC, RMSE)

r21 = MuMIn::r.squaredGLMM(fit1)[1,1]
r22 = MuMIn::r.squaredGLMM(fit2)[1,1]
r23 = MuMIn::r.squaredGLMM(fit3)[1,1]

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
bf_fit1_vs = bind_rows(
  get_bf_row2(test_bf(fit2, fit1), "fit2", "BF_fit1_vs"),   # fit1 over fit2
  get_bf_row2(test_bf(fit3, fit1), "fit3", "BF_fit1_vs")    # fit1 over fit3
) |> 
  mutate(
    log_BF_fit1_vs = log(BF_fit1_vs)
  ) |> 
  select(Name,log_BF_fit1_vs)

lr_fit1_vs = bind_rows(
  get_stat_row2(test_likelihoodratio(fit2, fit1), "fit2", "Chi2", "Chisq_fit1_vs"),
  get_stat_row2(test_likelihoodratio(fit3, fit1), "fit3", "Chi2", "Chisq_fit1_vs")
)

# fit3 as numerator vs fit2
bf_fit3_vs_fit2 = get_bf_row2(test_bf(fit2, fit3), "fit2", "BF_fit3_vs_fit2") |> 
  mutate(
    log_BF_fit3_vs_fit2 = log(BF_fit3_vs_fit2)
  ) |> 
  select(Name,log_BF_fit3_vs_fit2)

# join together
summary_table = perf |>
  left_join(bf_fit1_vs, by = "Name") |>
  left_join(lr_fit1_vs, by = "Name") |>
  left_join(bf_fit3_vs_fit2, by = "Name") |> 
  bind_cols(r2m = c(r21,r22,r23)) |> 
  relocate(r2m, .after = RMSE)

summary_table |> 
  knitr::kable(digits = 3, 'latex') |> 
  write_lines('glmm_comparisons.txt')
