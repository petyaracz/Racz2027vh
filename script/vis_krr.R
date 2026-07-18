# -- head -- #

set.seed(1337)
setwd('~/Github/Racz2027vh/')
library(tidyverse)
library(ggthemes)
library(patchwork)

# -- read -- #

d = read_tsv('dat/real_words_both_preds.tsv')

# -- counts -- #

d |> 
  summarise(sum = sum(back) + sum(front))

# -- borrowings -- #

d |> 
  select(stem,p_back,borrowing_language,borrowing_period,note)

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

treemap::treemap(detym,
                 palette = 'Greys',
                 index = c("borrowing_language2"),
                 vSize = "n1",
                 title = ""
)

treemap::treemap(detym,
                 palette = 'Greys',
                 index = c("borrowing_period2"),
                 vSize = "n2",
                 title = ""
)

detym |> 
  ggplot(aes(x = first_mention)) +
  geom_histogram() +
  theme_few() +
  facet_wrap( ~ borrowing_language, ncol = 1) +
  xlab('first mention') +
  ggtitle('First mention and source language of back + <e> stems (n = 108)')

## -- corpus -- ##

# -- MDS visualisations -- #

# real words: phonological MDS coloured by corpus log odds
p1 = d |>
  arrange(log_odds_back) |> 
  ggplot(aes(x = phonological_x, y = phonological_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.9) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
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
p2 = d |>
  arrange(log_odds_back) |> 
  ggplot(aes(x = semantic_x, y = semantic_y, fill = log_odds_back)) +
  geom_point(shape = 21, size = 3, alpha = 0.9) +
  scale_fill_gradient2(
    low = 'grey', mid = 'white', high = 'gold', midpoint = 0,
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

ggsave('~/Documents/latex/vh_krr_hun/viz/mds_loocv.pdf', width = 8, height = 3.5)

# -- mds with language data -- #

# p3 = d |> 
#   filter(!is.na(borrowing_category)) |> 
#   ggplot(aes(phonological_x, phonological_y, colour = borrowing_category)) +
#   geom_point(alpha = .3) +
#   geom_density_2d() +
#   theme_bw() +
#   scale_colour_colourblind() +
#   labs(colour = 'source language') +
#   theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
#   ggtitle('similarity space')
# 
# p4 = d |> 
#   filter(!is.na(borrowing_category)) |> 
#   ggplot(aes(semantic_x, semantic_y, colour = borrowing_category)) +
#   geom_point(alpha = .3) +
#   geom_density_2d() +
#   theme_bw() +
#   scale_colour_colourblind() +
#   labs(colour = 'source language') +
#   theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
#   ggtitle('distributional space')

p3 = d |> 
  filter(!is.na(borrowing_category)) |> 
  ggplot(aes(phonological_x, phonological_y)) +
  geom_point(alpha = .3, colour = 'lightgrey') +
  geom_density_2d(colour = 'darkgrey') +
  theme_bw() +
  facet_wrap(~ borrowing_category) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle('similarity space')

p4 = d |> 
  filter(!is.na(borrowing_category)) |> 
  ggplot(aes(semantic_x, semantic_y)) +
  geom_point(alpha = .3, colour = 'lightgrey') +
  geom_density_2d(colour = 'darkgrey') +
  theme_bw() +
  facet_wrap(~ borrowing_category) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle('distributional space')

p3 / p4 + plot_layout(guides = 'collect')

ggsave('viz/obs_spaces.png', dpi = 900, width = 6.5, height = 6.5)
