# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(glue)
library(ggthemes)
library(ggrepel)
library(patchwork)

# -- read -- #

d = read_tsv('dat/real_words_both_preds.tsv')

# -- etym -- #

detym = d |> 
  mutate(
    borrowing_period = ifelse(is.na(borrowing_period), 'unknown', borrowing_period) |> 
      fct_relevel('unknown', after = Inf),
    borrowing_language = ifelse(is.na(borrowing_language), 'other', borrowing_language) |> 
      fct_relevel('other', after = Inf)
  ) |>  
  mutate(
    n1 = n(),
    .by = borrowing_language
  ) |> 
  mutate(
    n2 = n(),
    .by = borrowing_period
  ) |> 
  mutate(
    borrowing_language2 = glue('{borrowing_language} (n = {n1})'),
    borrowing_period2 = glue('{borrowing_period} (n = {n2})')
  )

# -- words -- #

label_stems = c('kódex', 'duett', 'szoftver',  'fotel', 'haver', 'fater', 'macesz')

# -- list -- #

# a little over 9 billion words

d |> 
  mutate(
    stem_log10_freq = log10(stem_freq / 9000),
    borrowing_label = ifelse(is.na(borrowing_label), '', borrowing_label),
    borrowing_label = ifelse(is.na(borrowing_label), '', borrowing_label),
    international2 = case_when(
      is.na(international) ~ '',
      international ~ 'international word',
      !international ~ ''
    )
  ) |> 
  arrange(-log_odds_back) |> 
  select(stem,stem_log10_freq,log_odds_back,borrowing_label,international2) |> 
  knitr::kable('latex', digits = 2)

# -- counts -- #

d |> 
  summarise(sum = sum(back) + sum(front))

################################
# descriptive stats
################################

p0 = d |>
  ggplot(aes(y = -log_odds_back)) +
  geom_hline(aes(yintercept = 0), lty = 3) +
  geom_histogram(aes(fill = after_stat(y))) +
  geom_rug(aes(color = log_odds_back)) +
  geom_label_repel(
    data = d |> filter(stem %in% label_stems),
    aes(x = 0, label = stem),
    size = 2.5,
    fill = 'lightgrey'
  ) +
  scale_fill_viridis_c(na.value = "grey90", direction = 1) +
  coord_flip() +
  xlab('') +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99),
                        name = 'back →  front (p(front)'),
    name = 'log (front / back)',
    breaks = c(-9:5)
  ) +
  guides(fill="none",colour = 'none') +
  theme_few()

p1 = detym |> 
  mutate(borrowing_language = fct_relevel(borrowing_language, 'other', 'Yiddish','German','English','French','Latin')) |> 
  ggplot(aes(y = borrowing_language)) +
  geom_bar() +
  theme_few() +
  ylab('source language')

p2 = detym |> 
  mutate(borrowing_period = borrowing_period |> fct_relevel('before 16th c') |> fct_rev()) |> 
  ggplot(aes(y = borrowing_period)) +
  geom_bar() +
  theme_few() +
  ylab('first mention')

p3 = detym |> 
  ggplot(aes(x = first_mention)) +
  geom_histogram() +
  theme_few() +
  facet_wrap( ~ borrowing_language, ncol = 2) +
  xlab('first mention') +
  scale_y_continuous(breaks = c(1,3,5))

p0 / (p1 + p2) / p3 + plot_layout(heights = c(1,1,3)) + plot_annotation(tag_levels = 'i')
ggsave('viz/data_descriptive_stats.pdf', width = 4, height = 8)
ggsave('~/Documents/latex/vh_krr_hun/viz/data_descriptive_stats.pdf', width = 6, height = 9)

################################
# MDS maps
################################

# top row, similarity space

p4 = d |> 
  ggplot(aes(phonological_x,phonological_y,fill = log_odds_back)) +
  geom_point(size = 3, pch = 21, alpha = .75) +
  geom_label_repel(
    data = d |> filter(stem %in% label_stems),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  scale_fill_viridis_c(na.value = "grey90", direction = -1) +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(
    fill = 'log(back/front)'
    ) +
  ggtitle('phonological space')

p5 = d |> 
  ggplot(aes(semantic_x,semantic_y,fill = log_odds_back)) +
  geom_point(size = 3, pch = 21, alpha = .75) +
  geom_label_repel(
    data = d |> filter(stem %in% label_stems),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  scale_fill_viridis_c(na.value = "grey90", direction = -1) +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(
    fill = 'log(back/front)'
  ) +
  ggtitle('distributional semantic space')

# mid row, behaviour bin densities (no GAM smoothing: raw KDE per bin, same
# technique as the language row below so the whole figure shares one visual
# grammar); bins are a 25%-50%-25% split on log_odds_back, not equal thirds

d2 = d |>
  mutate(
    behaviour_bin = cut(
      log_odds_back,
      breaks = quantile(log_odds_back, probs = c(0, .25, .75, 1)),
      labels = c('front-preferring (0-25%)', 'mid (26-75%)', 'back-preferring (76-100%)'),
      include.lowest = TRUE
    )
  )

p6 = d2 |>
  ggplot(aes(phonological_x, phonological_y)) +
  geom_point(data = d, colour = 'grey85', alpha = .5, size = 1) +
  geom_density_2d(colour = 'black') +
  facet_wrap(~ behaviour_bin, nrow = 1) +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle('phonological space')

p7 = d2 |>
  ggplot(aes(semantic_x, semantic_y)) +
  geom_point(data = d, colour = 'grey85', alpha = .5, size = 1) +
  geom_density_2d(colour = 'black') +
  facet_wrap(~ behaviour_bin, nrow = 1) +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle('distributional semantic space')

((p4 + p5) + plot_layout(guides = 'collect') )/ p6 / p7  + plot_annotation(tag_levels = 'i')

ggsave('viz/mds_pref.pdf', width = 8.5, height = 9)
ggsave('~/Documents/latex/vh_krr_hun/viz/mds_pref.pdf', width = 8.5, height = 9)

# bottom rows, source language clustering, restricted to the four languages
# with enough words to say anything about; grey backdrop = full word set for
# context; one density contour per language (German/French/Latin only, n = 9
# for Yiddish is too few for a KDE to mean anything); points coloured by
# century of first mention

d_lang = d |>
  filter(borrowing_language %in% c('Latin', 'French', 'German', 'Yiddish')) |>
  mutate(
    borrowing_language = factor(borrowing_language, levels = c('Latin', 'French', 'German', 'Yiddish')),
    century = ((first_mention - 1) %/% 100) + 1
  )

p8 = d_lang |>
  ggplot(aes(phonological_x, phonological_y)) +
  geom_point(data = d |> select(-borrowing_language), colour = 'grey85', alpha = .5, size = 1) +
  geom_density_2d(data = d_lang |> filter(borrowing_language != 'Yiddish'), colour = 'black') +
  geom_point(aes(colour = century), size = 1.5, alpha = .75) +
  facet_wrap(~ borrowing_language, nrow = 1) +
  scale_colour_viridis_c(option = 'plasma', na.value = 'grey50') +
  theme_few() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  labs(colour = 'century') +
  ggtitle('phonological space')

p9 = d_lang |>
  ggplot(aes(semantic_x, semantic_y)) +
  geom_point(data = d |> select(-borrowing_language), colour = 'grey85', alpha = .5, size = 1) +
  geom_density_2d(data = d_lang |> filter(borrowing_language != 'Yiddish'), colour = 'black') +
  geom_point(aes(colour = century), size = 1.5, alpha = .75) +
  facet_wrap(~ borrowing_language, nrow = 1) +
  scale_colour_viridis_c(option = 'plasma', na.value = 'grey50') +
  theme_few() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  labs(colour = 'century') +
  ggtitle('distributional semantic space')

p8 / p9 + plot_layout(guides = 'collect') + plot_annotation(tag_level = 'i')

ggsave('viz/mds_lang.pdf', width = 8, height = 4.5)
ggsave('~/Documents/latex/vh_krr_hun/viz/mds_lang.pdf', width = 8, height = 4.5)
