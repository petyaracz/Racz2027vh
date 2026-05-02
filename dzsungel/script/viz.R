# -- head -- #

set.seed(1337)

setwd('~/Documents/Misc/habilitation/dzsungel/')

library(tidyverse)
library(ggrain)
library(ggthemes)
library(patchwork)

pbreaks = c(.01,.05,.1,.25,.5,.75,.9,.95,.99)

# -- sample set -- #

sample_set = c("klapec", "poszter", "zsorzsett", "szovjet", "pátens", "rulett", "dodzsem", "tróger", "ájer", "zsakett", "bróker", "koncert", "bovden", "karter", "lasztex", "subler", "nonszensz", "haver", "bunker", "majszter", "kupec", "muter", "sláger", "macher", "vátesz", "panel", "gólem", "lumpen", "vamzer", "dzsungel", "macesz", "modell", "bukfenc", "jampec", "skanzen", "halef", "korvett", "zsáner", "szubrett", "malter", "drukker", "samesz", "klozet", "falzett", "moped", "parkett", "flaszter", "parszek", "szoftver", "hardver")

# -- read -- #

raw = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
d = read_tsv('dat/dzsungel.tsv')
pred = read_tsv('dat/dzsungel_pred.tsv')
dist = read_tsv('dat/word_distances.tsv.gz')
etym = read_tsv('~/Github/RaczRebrus2024/dat/stemlanguage.tsv')

# -- combine -- #

d = left_join(d,pred)

# -- ex -- #

d |> 
  filter(stem %in% c("haver", "matek", "balek", "pajesz", "krapek", "muter", "kolesz", "mágnes", "gólem", "modell", "kódex", "szoftver", "projekt", "koncert")) |> 
  mutate(stem = fct_reorder(stem, log_odds_back)) |> 
  ggplot(aes(log_odds_back,stem)) +
  geom_point() +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.01,.1,.5,.9,.99), name = 'p(back)'), name = 'log (back / front)\nkódex → haver') + 
  theme_bw() + 
  ylab('') +
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_hline(aes(yintercept = 7.5), lty = 1)

ggsave('../viz/example.png', dpi = 900, width = 3, height = 3)

# -- r -- #

with(pred, cor.test(predicted_loo,observed))

# -- ex -- #

my_ex = c('haver','matek','fater','balek')

dist |> 
  filter(word1 %in% my_ex,word2 %in% my_ex) |> 
  pivot_wider(names_from = word1, values_from = phon_dist) |> 
  select(word2,fater,haver,matek,balek) |> 
  knitr::kable(digits = 2)

d |> 
  filter(stem %in% my_ex)

# -- barplot + rain plot -- #

p1 = raw |> 
  mutate(
    category = case_when(
      back == 0 ~ 'disharmonic\n(front suffix only)',
      front == 0 ~ 'true neutral\n(back suffix only)',
      back > 0 & front > 0 ~ 'vacillating\n(both back / front suffixes)'
    ) |> fct_rev()
  ) |> 
  ggplot(aes(y=category)) +
  geom_bar() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle('back V + <e> stems, filtered\n(n = 200)')

p2 = d |> 
  ggplot(aes(x = log_odds_back)) +
  geom_histogram() +
  geom_vline(aes(xintercept = 0), lty = 2) +
  scale_y_continuous(breaks = c(0,5,10)) +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.01,.1,.5,.9,.99), name = 'p(back)'), limits = c(-6,6), name = 'log (back / front)\nkódex → haver', breaks = c(-5:5)) +
  theme_bw() +
  ggtitle('back V + <e> stems, varying\n(n = 163)')

p1 + p2 + plot_annotation(tag_levels = '1')
ggsave('../viz/newfig1.png', dpi = 900, width = 8, height = 4)

# -- etym plot -- #

d |> 
  left_join(etym) |> 
  mutate(
    language2 = case_when(
      language == 'yi' ~ 'Yiddish',
      language == 'de' ~ 'German',
      language == 'fr' ~ 'French',
      language == 'la' ~ 'Latin'
    ) |> fct_relevel('Latin','French','German')
         ) |> 
  filter(!is.na(language2)) |> 
  ggplot(aes(language2,log_odds_back)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_rain() +
  coord_flip() +
  xlab('source language') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = pbreaks, name = 'p(back)'), limits = c(-6,6), name = 'log (back / front)\nkódex → haver', breaks = c(-5:5)) +
  theme_bw() +
  ggtitle('back V + <e> stems, varying (n = 106)') +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 15)        # axis tick labels (default ~11, doubled to 22)
    # axis.title = element_text(size = 24),       # axis titles (default ~12, doubled to 24)
    # plot.title = element_text(size = 28)        # plot title (default ~14, doubled to 28)
  )

ggsave('../viz/newfig1b.png', dpi = 900, width = 7, height = 5)

# -- distance plots -- #

# Perform classical MDS on the distance matrix
# First, convert the pairwise distances to a distance matrix

# create example
dist_matrix2 = dist |>
  filter(word1 %in% sample_set[1:20], word2 %in% sample_set[1:20]) |> 
  arrange(word1,word2) |> 
  # Make sure we have symmetric pairs
  # Pivot to wide format
  pivot_wider(names_from = word2, values_from = phon_dist) |>
  column_to_rownames("word1") |>
  as.matrix()
dist_matrix2 |> 
  knitr::kable(digits = 2)


# Create a square distance matrix from the long format
dist_matrix = dist |>
  # Make sure we have symmetric pairs
  # Pivot to wide format
  pivot_wider(names_from = word2, values_from = phon_dist) |>
  column_to_rownames("word1") |>
  as.matrix()

# Ensure the matrix is symmetric
dist_matrix[is.na(dist_matrix)] = t(dist_matrix)[is.na(dist_matrix)]

dist_matrix[colnames(dist_matrix) %in% sample_set & rownames(dist_matrix) %in% sample_set]

# Perform MDS to get 2D coordinates
mds_result = cmdscale(dist_matrix, k = 2)

# Convert to tibble and join with original data
mds_coords = mds_result |>
  as_tibble() |>
  rename(x = V1, y = V2) |>
  mutate(transcribed = rownames(mds_result))

# Combine with d
plot_data = d |>
  left_join(mds_coords, by = "transcribed") |> 
  mutate(
    log_odds_group = ifelse(log_odds_back > 0, 
                            "Positive (> 0)", 
                            "Negative or Zero (≤ 0)")
  )

plot_data_sample = plot_data |> 
  filter(stem %in% sample_set)

p1 = plot_data_sample |> 
  ggplot(aes(x = x, y = y, label = stem)) +
  geom_label() +
  theme_few() +
  labs(
    title = "MDS of Phonetic Distances (n = 50)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  )

p1 + coord_cartesian(xlim = c(-2,3))
ggsave('../viz/newfig2a.png', dpi = 900, width = 8, height = 5)

p1 +
  coord_cartesian(xlim = c(-2,3)) +
  annotate("rect", 
           xmin = .5, xmax = 3, 
           ymin = -.5, ymax = 1.5,
           color = "red",      # border color
           fill = NA,          # no fill (transparent)
           linewidth = 1.5)    # thickness of the border
ggsave('../viz/newfig2a2.png', dpi = 900, width = 8, height = 5)

# Create the faceted plot
ggplot(plot_data, aes(x = x, y = y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 4, alpha = 0.8) +
  scale_fill_gradient2(
    low = "grey", 
    mid = "white", 
    high = "gold",
    midpoint = 0,
    name = "Log Odds\n(Back)\nkódex → haver"
  ) +
  facet_wrap(~ log_odds_group) +
  labs(
    title = "MDS of Phonetic Distances",
    subtitle = "Faceted by log odds back (positive vs. negative/zero)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave('../viz/newfig2b.png', dpi = 900, width = 9, height = 5)

plot_data2 = plot_data |> 
  inner_join(etym) |> 
  filter(language %in% c('de','yi','fr','la'))

plot_data2 |> 
  ggplot(aes(x = x, y = y, color = language)) +
  geom_point(alpha = 0.3, size = 2) +
  stat_density_2d(aes(color = language), bins = 3, linewidth = 0.8) +
  geom_label(
    data = plot_data2 |> 
      group_by(language) |> 
      summarise(x = mean(x), y = mean(y)),
    aes(label = language, fill = language),
    color = "white",
    fontface = "bold",
    size = 5,
    alpha = .5
  ) +
  labs(
    title = "MDS of Phonetic Distances",
    subtitle = "Big four etymological sources",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11)
  ) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

ggsave('../viz/newfig3.png', dpi = 900, width = 6, height = 5)

# -- krr plot -- #

pred |> 
  ggplot(aes(observed,predicted_loo)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_smooth() +
  theme_bw() +
  xlab('observed log(back/front)\nkódex → haver') +
  ylab('predicted log(back/front)\nkódex → haver') +
  ggtitle('kernel ridge regression with\nleave-one-out cross-validation')

ggsave('../viz/newfig4.png', dpi = 900, width = 6.5, height = 4.5)

p1 = d |> 
  filter(stem %in% sample_set) |> 
  ggplot(aes(observed,predicted_loo,label = stem)) +
  geom_label() +
  theme_few() +
  xlab('observed log(back/front)\nkódex → haver') +
  ylab('predicted log(back/front)\nkódex → haver') +
  ggtitle('sample of 50 stems')

p1 + coord_cartesian(xlim = c(-10,5))
ggsave('../viz/newfig5a.png', dpi = 900, width = 6.5, height = 4.5)

p1 +
  coord_cartesian(xlim = c(-10,5)) +
  annotate("rect", 
           xmin = -2, xmax = 4, 
           ymin = -2, ymax = 2,
           color = "red",      # border color
           fill = NA,          # no fill (transparent)
           linewidth = 1.5)    # thickness of the border

ggsave('../viz/newfig5b.png', dpi = 900, width = 6.5, height = 4.5)
