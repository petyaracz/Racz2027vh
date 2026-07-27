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
# label predictions
######################################################

m1 = glmmTMB(
  category_educated ~ 1 + stem_log10_freq,
  family = binomial,
  data = label_preds
)

m2 = glmmTMB(
  category_educated ~ s_phonological_model + stem_log10_freq,
  family = binomial,
  data = label_preds
)

m3 = glmmTMB(
  category_educated ~ s_semantic_model + stem_log10_freq,
  family = binomial,
  data = label_preds
)

m4 = glmmTMB(
  category_educated ~ s_phonological_model + s_semantic_model + stem_log10_freq,
  family = binomial,
  data = label_preds
)

# -- diagnostics -- #

check_model(m1)
check_model(m2)
check_model(m3)
check_model(m4)

# -- tidy table -- #

# basic indices
compare_performance(m1,m2,m3,m4, metrics = "common") |>
  as.data.frame() |>
  select(Name, AIC, BIC, R2_Tjur) |> 
  knitr::kable(digits = 2, 'latex', booktabs = T)

# -- model comparison -- #

test_likelihoodratio(m2,m1)
test_likelihoodratio(m2,m4)

######################################################
# lo predictions
######################################################

m5 = glmmTMB(
  cbind(back,front) ~ 1 + stem_log10_freq + (1|stem),
  family = binomial,
  data = lo_preds
)

m6 = glmmTMB(
  cbind(back,front) ~ category_educated_other + stem_log10_freq + (1|stem),
  family = binomial,
  data = lo_preds
)

m7 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + stem_log10_freq + (1|stem),
  family = binomial,
  data = lo_preds
)

m8 = glmmTMB(
  cbind(back,front) ~ s_semantic_model + stem_log10_freq + (1|stem),
  family = binomial,
  data = lo_preds
)

m9 = glmmTMB(
  cbind(back,front) ~ s_phonological_model + s_semantic_model + stem_log10_freq + (1|stem),
  family = binomial,
  data = lo_preds
)

m10 = glmmTMB(
  cbind(back,front) ~ category_educated_other + s_phonological_model + s_semantic_model + stem_log10_freq + (1|stem),
  family = binomial,
  data = lo_preds
)

# -- diagnostics -- #

check_model(m5)
check_model(m6)
check_model(m7)
check_model(m8)
check_model(m9)
check_model(m10)

# -- tidy table -- #

# basic indices
r2s = map_dbl(list(m5,m6,m7,m8,m9,m10), ~ MuMIn::r.squaredGLMM(.)[1,1])

compare_performance(m5,m6,m7,m8,m9,m10, metrics = "common") |>
  as.data.frame() |>
  bind_cols(R2 = r2s) |> 
  select(Name, AIC, BIC, R2) |> 
  knitr::kable(digits = 2, 'latex', booktabs = T)

# -- model comparison -- #

test_likelihoodratio(m9,m5)
test_likelihoodratio(m9,m7)
test_likelihoodratio(m9,m8)
test_likelihoodratio(m9,m10)

# besto modelo
broom.mixed::tidy(m9, conf.int = T) |> 
  filter(effect == 'fixed') |> 
  select(term,estimate,std.error,conf.low,conf.high)|> 
  knitr::kable(digits = 2, 'latex', booktabs = T)

