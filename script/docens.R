# figs for docens.md in markdown_talks

# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(ggrain)
library(ggthemes)
library(patchwork)

# -- functions -- #

# Hungarian orthography to IPA-like transcription for distance lookup
transcribeIPA = function(dat) {
  dat |> stringr::str_replace_all(c(
    'x' = 'ks', 'cs' = 'č', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ',
    'ny' = 'ṉ', 'sz' = 'ß', 's' = 'š', 'ß' = 's', 'ck' = 'kk',
    'codec' = 'kodek', 'ch' = 'h', 'ly' = 'j'
  ))
}

# Build square symmetric matrix from long format, run cmdscale k=2, return tibble with word x y
do_mds = function(dist_df, dist_col = 'phon_dist') {
  mat = dist_df |>
    pivot_wider(names_from = word2, values_from = all_of(dist_col)) |>
    column_to_rownames('word1') |>
    as.matrix()
  mat[is.na(mat)] = t(mat)[is.na(mat)]
  res = cmdscale(mat, k = 2)
  as_tibble(res) |>
    rename(x = V1, y = V2) |>
    mutate(word = rownames(res))
}

# -- read -- #

real_phon = read_tsv('dat/real_words_phon_preds.tsv')
nonwords_phon = read_tsv('dat/nonwords_phon_preds.tsv')
real_sem = read_tsv('dat/real_words_semantic_preds.tsv')
trials_real_words = read_tsv('dat/unfiltered_data_real_word.tsv')
trials_nonwords = read_tsv('dat/unfiltered_data_nonword.tsv')

# -- fig -- #  

## etym

etym = real_phon |> 
  mutate(
    n = n(),
    origin = case_when(
      n > 10 ~ language
    ),
    .by = language
  ) |>
  filter(origin %in% c('yi','de','fr','la')) |> 
  mutate(origin = factor(origin, levels = c('yi','de','fr','la')) |> fct_rev()) |> 
  inner_join(real_phon)

# choose your fighter
label_stems_1 = c("korvett", "kódex", "mutter", "haver") 

etym |>
  ggplot(aes(origin, log_odds_back)) +
  geom_rain() +
  geom_label(
    data = etym |> filter(stem %in% label_stems_1),
    aes(label = stem),
    hjust = -0.15,
    vjust = 1.5,
    size = 3,
    fill = 'lightgrey'
  ) +
  coord_flip() +
  scale_x_discrete(name = 'forrásnyelv', labels = c('latin','francia','német','jiddis')) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(0.01,0.1,0.5,0.9,0.99), name = 'p(fotelnak)'), limits = c(-8,6), name = 'log (fotelnak / fotelnek)', breaks = c(-5:5)) +
  theme_few()

ggsave('~/Documents/markdown/markdown_talks/viz/docens_etymology.png', dpi = 900, width = 5, height = 4)

## phono mds

# cyf
label_stems_2 = c("stukker", "suszter", "tróger", "korvett", "parkett", "plakett") 

# real words: phonological MDS coloured by corpus log odds
real_phon |>
  ggplot(aes(x = phonological_x, y = phonological_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
    name = 'log(fotelnak/fotelnek)'
  ) +
  geom_label(
    data = real_phon |> filter(stem %in% label_stems_2),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  labs(
    title = 'MDS: alaki távolságok',
    x = 'MDS dim 1', y = 'MDS dim 2'
  ) +
  theme_bw()

ggsave('~/Documents/markdown/markdown_talks/viz/docens_fon_mds.png', dpi = 900, width = 5, height = 4)

# real words: phonological MDS coloured by corpus log odds
real_sem |>
  ggplot(aes(x = semantic_x, y = semantic_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
    name = 'log(fotelnak/fotelnek)'
  ) +
  geom_label(
    data = real_sem |> filter(stem %in% label_stems_2),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  labs(
    title = 'MDS: szemantikai távolságok',
    x = 'MDS dim 1', y = 'MDS dim 2'
  ) +
  theme_bw()

ggsave('~/Documents/markdown/markdown_talks/viz/docens_szem_mds.png', dpi = 900, width = 5, height = 4)

## krr

# real words: phonological KRR LOO
p1 = real_phon |>
  ggplot(aes(log_odds_back, predicted_loo)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_label(
    data = real_phon |> filter(stem %in% label_stems_1),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  geom_smooth() +
  theme_few() +
  xlab('megfigyelt log(fotelnak/fotelnek)') +
  ylab('várt log(fotelnak/fotelnek)') +
  ggtitle('KRR LOO: alaki hasonlóság')

# real words: semantic KRR LOO
p2 = real_sem |>
  ggplot(aes(log_odds_back, predicted_loo)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_label(
    data = real_phon |> filter(stem %in% label_stems_1),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  ylim(-7,2) +
  geom_smooth() +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  ylim(-7,2) +
  xlab('megfigyelt log(fotelnak/fotelnek)') +
  ylab('várt log(fotelnak/fotelnek)') +
  ggtitle('szemantikai hasonlóság')

p1 + p2 + plot_layout(guides = 'collect')

ggsave('~/Documents/markdown/markdown_talks/viz/docens_krr.png', dpi = 900, width = 8, height = 4)

## nonwords

real_phon_2 = real_phon |> 
  select(stem,phonological_x,phonological_y,log_odds_back,predicted_loo) |> 
  rename(predicted = predicted_loo) |> 
  mutate(type = 'létező szó')

nonwords_phon_2 = nonwords_phon |> 
  select(stem,phonological_x,phonological_y,log_odds_back,predicted) |> 
  mutate(type = 'álszó')

nonwords_3 = bind_rows(real_phon_2,nonwords_phon_2)

label_stems_3 = c('kumek','ojett','kajler','bórcer')
label_stems_4 = c(label_stems_1,label_stems_3)

nonwords_3 |> 
  ggplot(aes(x = phonological_x, y = phonological_y, fill = type)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_label(
    data = nonwords_3 |> filter(stem %in% label_stems_4),
    aes(label = stem, colour = type),
    size = 3,
    fill = 'white'
  ) +
  labs(
    title = 'MDS: alaktani távolságok',
    x = 'MDS dim 1', y = 'MDS dim 2',
    fill = 'típus', colour = 'típus'
  ) +
  scale_fill_grey() +
  scale_colour_grey() +
  theme_bw()

ggsave('~/Documents/markdown/markdown_talks/viz/docens_alszo_mds.png', dpi = 900, width = 5, height = 4)

nonwords_phon |>
  ggplot(aes(log_odds_back, predicted)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_label(
    data = nonwords_phon |> filter(stem %in% label_stems_3),
    aes(label = stem),
    size = 3,
    fill = 'lightgrey'
  ) +
  geom_smooth() +
  theme_bw() +
  xlab('megfigyelt log(fotelnak/fotelnek)') +
  ylab('várt log(fotelnak/fotelnek)') +
  ggtitle('alaktani hasonlóság\nlétező szavakhoz')

ggsave('~/Documents/markdown/markdown_talks/viz/docens_alszo_pred.png', dpi = 900, width = 4, height = 4)
