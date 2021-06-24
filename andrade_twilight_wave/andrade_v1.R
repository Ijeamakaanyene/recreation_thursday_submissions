library(ggplot2)
library(dplyr)
library(ggforce)

# Source of Art:
# https://www.locksgallery.com/exhibitions/edna-andrade-symmetries

# Shoutout to Miles Ott for these colors
colors = c(
  "#dfe2db",
  "#d3d5d2",
  "#c4c3c1",
  "#b7b2b8",
  "#a79da5",
  "#91ad9f",
  "#a1b6af",
  "#b1c6bf",
  "#c6d6cc",
  "#dae1d9",
  "#e8ebe4"
)

x = seq(1, by = .5, length.out = length(colors))
y = seq(1, by = .5, length.out = length(colors))


grids_1 = expand.grid(x0 = x[seq(1, 11, by = 2)],
                      y0 = y[seq(1, 11, by = 2)])

grids_2 = expand.grid(x0 = x[seq(2, 11, by = 2)],
                      y0 = y[seq(2, 11, by = 2)])

grids = bind_rows(grids_1, grids_2)

circles = grids %>%
  arrange(x0) %>%
  mutate(r = .5,
         fill = sample(colors, 
                       n(), 
                       replace = TRUE))

arcs = circles %>%
  # Bottom Right
  mutate(start1 = if_else(y0 == 1 | x0 == 6,
                          NA_real_,
                          2/4*pi),
         end1 = if_else(y0 == 1 | x0 == 6,
                        NA_real_,
                        pi)) %>%
  # Bottom Left
  mutate(start2 = if_else(x0 == 1 | y0 == 1,
                          NA_real_,
                          pi),
         end2 = if_else(x0 == 1 | y0 == 1,
                        NA_real_,
                        6/4*pi)) %>%
  # Top Left
  mutate(start3 = if_else(x0 == 1 | y0 == 6,
                          NA_real_,
                          6/4*pi),
         end3 = if_else(x0 == 1 | y0 == 6,
                        NA_real_,
                        2*pi)) %>%
  # Top Right
  mutate(start4 = if_else(x0 == 6 | y0 == 6,
                          NA_real_,
                          0),
         end4 = if_else(x0 == 6 | y0 == 6,
                        NA_real_,
                        2/4*pi))

ggplot() +
  geom_circle(data = circles,
              aes(x0 = x0, y0 = y0,
                  r = r,
                  fill = fill),
              alpha = 0.75,
              color = NA) +
  geom_arc(data = arcs,
           aes(x0 = x0, y0 = y0, r = r,
               start = start1, end = end1),
           size = .75,
           color = "orange") +
  geom_arc(data = arcs,
           aes(x0 = x0, y0 = y0, r = r,
               start = start2, end = end2),
           size = .75,
           color = "orange") +
  geom_arc(data = arcs,
           aes(x0 = x0, y0 = y0, r = r,
               start = start3, end = end3),
           size = .75,
           color = "orange") +
  geom_arc(data = arcs,
           aes(x0 = x0, y0 = y0, r = r,
               start = start4, end = end4),
           size = .75,
           color = "orange") +
  coord_equal(expand = FALSE) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")

ggsave(filename = "andrade_v1.png", 
       plot = last_plot(), 
       path = here::here("andrade_twilight_wave", "outputs"),
       device = "png", width = 12, height = 12, units = "in",
       dpi = 300)


