library(dplyr)
library(ggplot2)
library(purrr)

# lets make the rectangle 2 in length, and 1 in width

starting_grid = expand.grid(
  y = seq(1, by = 2, length.out = 31),
  x = seq(1, by = 1, length.out = 49)
)

create_rectangle = function(input_x, input_y){
  tibble(
    x = c(input_x, input_x + 1, input_x + 1, input_x),
    y = c(input_y, input_y, input_y + 2, input_y + 2)
  )
}

letter_grps = paste0(letters, 1:nrow(starting_grid))

starting_rectangle = map_dfr(1:nrow(starting_grid),
                             ~bind_cols(
                               create_rectangle(
                                 input_x = starting_grid$x[.x],
                                 input_y = starting_grid$y[.x]),
                               group = letter_grps[.x]))


altered_sq = alter_squares(starting_rectangle,
                           alter_value = .1,
                           alter_method = "fixed")
jittered_sq = alter_squares(altered_sq, 
                            alter_value = .1, 
                            alter_method = "random")

colpal = list(
  sage = "#BCCAB1",
  blue = "#294D82",
  green = "#639079",
  lightblue = "#6493BF",
  purple = "#54567E",
  yellow = "#E7CC3D",
  gold = "#C7AB3E",
  orange = "#AA5423",
  red = "#913220",
  lightyellow = "#f6eab4",
  pink = "#A65D8C",
  lightpink = "#C5A5AF",
  lightpurple = "#8E90B9",
  turqouise = "#66A1AB"
)


color_grid = tibble(
  group = letter_grps,
  color = c(
    # col 1
    rep(colpal$sage, 3), rep(colpal$blue, 28),
    # col 2
    rep(colpal$sage, 7), rep(colpal$green, 11), rep(colpal$lightblue, 13),
    # col 3
    rep(colpal$sage, 7), rep(colpal$blue, 24),
    # col 4
    rep(colpal$sage, 6), rep(colpal$blue, 15), rep(colpal$lightblue, 6), rep(colpal$blue, 4),
    # col 5
    rep(colpal$sage, 7), rep(colpal$blue, 24),
    # col 6
    rep(colpal$sage, 5), rep(colpal$purple, 26),
    # col 7
    rep(colpal$yellow, 19), rep(colpal$blue, 12),
    # col 8
    rep(colpal$yellow, 16), rep(colpal$green, 15),
    # col 9
    rep(colpal$gold, 31),
    # col 10
    rep(colpal$orange, 31),
    # col 11
    rep(colpal$red, 31),
    # col 12
    rep(colpal$purple, 31),
    # col 13
    rep(colpal$yellow, 2), rep(colpal$green, 5), rep(colpal$purple, 7),
    rep(colpal$blue, 12), rep(colpal$purple, 5),
    # col 14
    rep(colpal$yellow, 2), rep(colpal$green, 6), rep(colpal$blue, 20),
    rep(colpal$green, 3),
    # col 15
    rep(colpal$blue, 27), rep(colpal$green, 4),
    # col 16
    rep(colpal$blue, 27), rep(colpal$lightblue, 4),
    # col 17
    rep(colpal$blue, 28), rep(colpal$lightblue, 3),
    # col 18
    rep(colpal$red, 6), rep(colpal$blue, 25),
    # col 19
    rep(colpal$yellow, 3), rep(colpal$blue, 28),
    # col 20
    rep(colpal$yellow, 4), rep(colpal$green, 27),
    # col 21
    rep(colpal$yellow, 31),
    # col 22
    rep(colpal$red, 31),
    # col 23
    rep(colpal$red, 31),
    # col 24
    rep(colpal$lightblue, 31),
    # col 25
    rep(colpal$red, 31),
    # col 26
    rep(colpal$red, 31),
    # col 27
    rep(colpal$orange, 31),
    # col 28
    rep(colpal$green, 31),
    # col 29
    rep(colpal$blue, 31),
    # col 30
    rep(colpal$blue, 31),
    # col 31
    rep(colpal$blue, 31),
    # col 32 
    rep(colpal$turqouise, 24), rep(colpal$blue, 7),
    # col 33
    rep(colpal$orange, 12), rep(colpal$blue, 19),
    # col 34
    rep(colpal$sage, 15), rep(colpal$blue, 16),
    # col 35
    rep(colpal$sage, 31),
    # col 36
    rep(colpal$yellow, 31),
    # col 37
    rep(colpal$red, 31),
    # col 38
    rep(colpal$pink, 31),
    # col 39
    rep(colpal$blue, 31),
    # col 40
    rep(colpal$blue, 31),
    # col 41
    rep(colpal$lightblue, 31),
    # col 42
    rep(colpal$lightyellow, 31),
    # col 43
    rep(colpal$blue, 31),
    # col 44
    rep(colpal$red, 6), rep(colpal$blue, 25),
    # col 45
    rep(colpal$red, 14), rep(colpal$blue, 17),
    # col 46
    rep(colpal$red, 16), rep(colpal$lightpurple, 15),
    # col 47
    rep(colpal$red, 17), rep(colpal$turqouise, 14),
    # col 48
    rep(colpal$lightpink, 16), rep(colpal$blue, 15),
    # col 49
    rep(colpal$lightpink, 17), rep(colpal$green, 14)
  )
)

final_sq = jittered_sq %>%
  left_join(., color_grid, by = c("group" = "group"))


ggplot() +
  geom_polygon(data = jittered_sq,
               aes(x = x, 
                   y = y,
                   group = group),
               fill = "white",
               color = "black") +
  geom_polygon(data = final_sq,
               aes(x = x,
                   y = y,
                   group = group,
                   fill = color),
               size = .05,
               color = "black") +
  scale_fill_identity() +
  scale_color_identity() +
  coord_equal() +
  theme_void()


ggsave(filename = "thomas_iris_v1.png", 
       plot = last_plot(), 
       path = here::here("thomas_iris", "outputs"),
       device = "png", width = 9, height = 10.8, units = "in",
       dpi = 300)
