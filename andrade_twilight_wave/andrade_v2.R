library(ggplot2)
library(dplyr)
library(ggforce)

# Lets do 6 by 6

grid = expand.grid(
  x = seq(1, 16, by = 3),
  y = seq(1, 16, by = 3)) %>%
  mutate(
    group = paste0(LETTERS, 1:n()),
    type = rep(c("purple", "green", "purple", "green", "purple", "green",
             "green", "purple", "green", "purple", "green", "purple"), 
             3))

squares = list()

for(i in 1:nrow(grid)){
  squares[[i]] = tibble(
    x = c(grid$x[i],
          grid$x[i] + 3,
          grid$x[i] + 3,
          grid$x[i]),
    y = c(grid$y[i],
          grid$y[i],
          grid$y[i] + 3,
          grid$y[i] + 3),
    group = grid$group[i],
    type = grid$type[i]
  )
}

squares_grid = bind_rows(squares) %>%
  mutate(fill = if_else(type == "purple", 
                        "#e8eadf",  
                        "#e7ede4"))

circles = list()

for(i in 1:nrow(grid)){
  x_adj = sample(c(0, 1.5, 3), size = 1)
  y_adj = ifelse(x_adj == 0, 1.5, 
                    ifelse(x_adj == 1.5, 3, 
                           ifelse(x_adj == 3, 1.5, 0)))
  
  start = ifelse(x_adj == 0, 0, 
                 ifelse(x_adj == 1.5, 2/4*pi,
                        ifelse(x_adj == 3, pi, pi)))
  
  end = ifelse(x_adj == 0, pi, 
               ifelse(x_adj == 1.5, 6/4*pi,
                      ifelse(x_adj == 3, 2*pi, 2*pi)))
  
  circles[[i]] = tibble(
    x0 = grid$x[i] + x_adj,
    y0= grid$y[i] + y_adj,
    r0 = 0,
    start = start, 
    end = end,
    type = grid$type[i]
  )
}

circles_grid1 = bind_rows(circles) %>%
  mutate(r = 1.5,
         fill = if_else(type == "purple", "#dfe2db", "#d8dfd7"))

circles_grid2 = bind_rows(circles) %>%
  mutate(r = 1.25,
         fill = if_else(type == "purple", "#d1d4d1", "#c4d5cc"))

circles_grid3 = bind_rows(circles) %>%
  mutate(r = 1,
         fill = if_else(type == "purple", "#c5c2c1", "#b0c5be"))

circles_grid4 = bind_rows(circles) %>%
  mutate(r = .75,
         fill = if_else(type == "purple", "#b7b2b8", "#a1b9b1"))

circles_grid5 = bind_rows(circles) %>%
  mutate(r = .5,
         fill = if_else(type == "purple", "#a79da5", "#90ac9f"))

circles_grid6 = bind_rows(circles) %>%
  mutate(r = .25,
         fill = "#fe9b39")

ggplot() +
  geom_polygon(data = squares_grid,
               aes(x = x, y = y, group = group,
                   fill = fill),
               color = NA) +
  geom_arc_bar(data = circles_grid1,
               aes(x0 = x0, y0 = y0, 
                   r0 = r0, r = r,
                   start = start, end = end,
                   fill = fill),
               color = NA) +
  geom_arc_bar(data = circles_grid2,
               aes(x0 = x0, y0 = y0, 
                   r0 = r0, r = r,
                   start = start, end = end,
                   fill = fill),
               color = NA) +
  geom_arc_bar(data = circles_grid3,
               aes(x0 = x0, y0 = y0, 
                   r0 = r0, r = r,
                   start = start, end = end,
                   fill = fill),
               color = NA) +
  geom_arc_bar(data = circles_grid4,
               aes(x0 = x0, y0 = y0, 
                   r0 = r0, r = r,
                   start = start, end = end,
                   fill = fill),
               color = NA) +
  geom_arc_bar(data = circles_grid5,
               aes(x0 = x0, y0 = y0, 
                   r0 = r0, r = r,
                   start = start, end = end,
                   fill = fill),
               color = NA) +
  geom_arc_bar(data = circles_grid6,
               aes(x0 = x0, y0 = y0, 
                   r0 = r0, r = r,
                   start = start, end = end,
                   fill = fill),
               color = NA) +
  geom_polygon(data = squares_grid,
               aes(x = x, y = y, group = group),
               fill = NA,
               color = "white") +
  coord_equal(expand = FALSE) +
  scale_fill_identity() +
  theme_void()

ggsave(filename = "andrade_v3.png", 
       plot = last_plot(), 
       path = here::here("andrade_twilight_wave", "outputs"),
       device = "png", width = 12, height = 12, units = "in",
       dpi = 300)



