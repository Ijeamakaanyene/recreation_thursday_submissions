library(dplyr)
library(ggplot2)
library(purrr)


create_squares = function(input_x, input_y, group_opts){
  output = tibble(
    x = c(input_x, input_x + .5, input_x + .5, input_x,
          input_x + .5, input_x + 1, input_x + 1, input_x + .5,
          input_x + .5, input_x + 1, input_x + 1, input_x + .5,
          input_x, input_x + .5, input_x + .5, input_x),
    y = c(input_y, input_y, input_y + .5, input_y + .5,
          input_y, input_y, input_y + .5, input_y + .5,
          input_y + .5, input_y + .5, input_y + 1, input_y + 1,
          input_y + .5, input_y + .5, input_y + 1, input_y + 1),
    group = rep(paste0(group_opts, 1:4), each = 4)
  )
}


grid = expand.grid(x = 1:9,
                   y = 1:9)


letter_groups = paste0(letters, 1:100)

square_grid = map_dfr(1:nrow(grid),
                      ~bind_cols(
                        create_squares(
                          input_x = grid$x[.x],
                          input_y = grid$y[.x],
                          group_opts = letter_groups[.x]
                      )))

colors = c(rep("#00336A", length.out = 18),
           rep("#00336A", length.out = 18),
           rep(c("#00336A", "#004794"), length.out = 18),
           rep(c("#004794", "#00336A"), length.out = 18),
           rep(c("#00336A", "#004794", "#005fb8"), length.out = 18),
           rep(c("#004794", "#005fb8", "#00336A"), length.out = 18),
           rep(c("#005fb8", "white", "#00336A", "#004794"), length.out = 18),
           rep(c("white", "#00336A", "#004794", "#005fb8"), length.out = 18),
           rep(c("white", "#005fb8"), length.out = 18),
           rep(c("#005fb8", "white"), length.out = 18),
           rep(c("white", "#00336A", "#004794", "#005fb8"), length.out = 18),
           rep(c("#005fb8", "white", "#00336A", "#004794"), length.out = 18),
           rep(c("#004794", "#005fb8", "#00336A"), length.out = 18),
           rep(c("#00336A", "#004794", "#005fb8"), length.out = 18),
           rep(c("#004794", "#00336A"), length.out = 18),
           rep(c("#00336A", "#004794"), length.out = 18),
           rep("#00336A", length.out = 18),
           rep("#00336A", length.out = 18))

color_grid = square_grid %>%
  group_by(group) %>%
  slice_head() %>%
  ungroup() %>%
  arrange(y, x) %>%
  mutate(fill = colors)

square_grid = square_grid %>%
  left_join(., color_grid %>% 
              select(-c(x, y)), 
            by = c("group" = "group"))

ggplot() +
  geom_polygon(data = square_grid,
               aes(x = x,
                   y = y,
                   group = group,
                   fill = fill),
               color = "black") +
  scale_fill_identity() +
  coord_equal() +
  theme_void()

ggsave(filename = "creech_v2.png", 
       plot = last_plot(), 
       path = here::here("Creech_Cyanotype", "outputs"),
       device = "png", width = 15, height = 15, units = "in",
       dpi = 300,
       limitsize = FALSE)
