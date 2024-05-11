# visualizing objects launched into space
# data downloaded from: https://ourworldindata.org/grapher/yearly-number-of-objects-launched-into-outer-space

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = "DM Mono", family = "DM")
showtext_auto(enable = T)

dat <- read.csv('yearly-number-of-objects-launched-into-outer-space.csv') |>
  select(-Code)
colnames(dat) <- c('Entity','Year','Number')

dat_world_sum <- dat |>
  filter(Entity == 'World') |>
  mutate(Decade = 10 * (Year %/% 10)) |>
  group_by(Decade) |>
  summarize(Num_Total = sum(Number))

dat_world_sum$Decade <- paste0(as.character(dat_world_sum$Decade), 's: ', dat_world_sum$Num_Total)

# uncount so each object gets its own row, so we can add xy coords to it
dat_world_long <- uncount(dat_world_sum, Num_Total)

set.seed(123) 
dat_world_long <- dat_world_long |>
  mutate(x = runif(n(), min = 0, max = 50), 
         y = runif(n(), min = 0, max = 50))

ggplot() +
  #geom_point(data = dat_world_long,
  #           aes(x = x, y = y),
  #           size = .1,
  #           color = '#fff8e7') +
  # add glow to points
  ggshadow::geom_glowpoint(data = dat_world_long,
                           aes(x = x, y = y),
                           size = .15,
                           color = '#fff8e7') +
  facet_grid(~Decade) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = '#121C22', color=NA),
        strip.text = element_text(color = 'grey92', family = 'DM', size = 40),
        panel.grid = element_line(color = 'grey20'),
        plot.title = element_text(color = 'grey92', family = 'DM', size = 150),
        plot.subtitle = element_text(color = 'grey92', family = 'DM', size = 50)
  ) +
  labs(title = 'Objects in Orbit',
       subtitle = 'Number of objects launched into space by decade')

ggsave('space_objects.png', dpi = 300, width = 16, height = 8)
