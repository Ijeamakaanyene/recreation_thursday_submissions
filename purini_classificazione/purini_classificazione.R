
# Square Selections -------------------------------------------------------

# 1, 2, 3, 
# 4, 6, 9, 
# 12, 13, 14, 
# 44


# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(patchwork)


# Functions ---------------------------------------------------------------

create_square_plot = function(df,
                              square_num){
  df %>%
    ggplot(.) +
    geom_segment(aes(x = x, 
                     xend = xend,
                     y = y,
                     yend = yend),
                 size = 1) +
    annotate("text", 
             x = 1.1,
             y = 0,
             hjust = 1,
             label = square_num,
             family = "Comfortaa",
             size = 6,
             fontface = "bold") +
    coord_equal() +
    xlim(1, 10) +
    ylim(0, 10) +
    theme_void()
}


# Square Setup ------------------------------------------------------------
sq44 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  2, 9, 2, 2,
  1, 3, 3, 3,
  8, 10, 3, 3,
  2, 9, 4, 4,
  1, 3, 5, 5,
  8, 10, 5, 5,
  2, 9, 6, 6,
  1, 3, 7, 7,
  8, 10, 7, 7,
  2, 9, 8, 8,
  1, 3, 9, 9,
  8, 10, 9, 9,
  2, 9, 10, 10
)

sq13 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1, 
  1, 3, 4, 4,
  3, 8, 8, 8,
  8, 10, 8, 8,
  8, 10, 6, 6,
  1, 10, 9, 9,
  1, 10, 10, 10
)


sq12 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  1, 9.5, 2, 2,
  1, 9, 3, 3,
  1, 8.5, 4, 4,
  1, 8, 5, 5,
  1, 7.5, 6, 6,
  1, 7, 7, 7,
  1, 6.5, 8, 8,
  1, 6, 9, 9,
  1, 5.5, 10, 10
)



sq9 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  1, 3, 2, 2,
  1, 3, 3, 3,
  2, 9, 4, 4,
  8, 10, 5, 5,
  8, 10, 6, 6,
  1, 3, 7, 7,
  8, 10, 7, 7,
  1, 10, 8, 8,
  1, 10, 9, 9,
  1, 10, 10, 10
)

sq6 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  1, 10, 2, 2,
  1, 10, 3, 3,
  7, 10, 4, 4,
  7, 10, 5, 5,
  7, 10, 6, 6,
  7, 10, 7, 7,
  1, 10, 8, 8,
  1, 10, 9, 9,
  1, 10, 10, 10
)


sq4 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  1, 10, 2, 2,
  5, 10, 3, 3,
  5, 10, 4, 4,
  1, 10, 5, 5,
  1, 10, 6, 6,
  1, 10, 7, 7,
  1, 10, 8, 8,
  1, 10, 9, 9,
  1, 10, 10, 10
)

sq3 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 5.5, 1, 1,
  1, 6, 2, 2,
  1, 6.5, 3, 3,
  1, 7, 4, 4,
  1, 7.5, 5, 5,
  1, 8, 6, 6,
  1, 8.5, 7, 7,
  1, 9, 8, 8,
  1, 9.5, 9, 9,
  1, 10, 10, 10
)

sq2 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  1, 5, 2, 2,
  1, 10, 3, 3,
  1, 10, 4, 4,
  1, 10, 5, 5,
  1, 10, 6, 6,
  1, 10, 7, 7,
  1, 10, 8, 8,
  1, 10, 9, 9,
  1, 10, 10, 10
)

sq1 = tribble(
  ~x, ~xend, ~y, ~yend,
  1, 10, 1, 1,
  1, 10, 2, 2,
  1, 10, 3, 3,
  1, 10, 4, 4,
  1, 10, 5, 5,
  1, 10, 6, 6,
  1, 10, 7, 7,
  1, 10, 8, 8,
  1, 10, 9, 9,
  1, 10, 10, 10
)


# Creating Plots ----------------------------------------------------------

psq1 = create_square_plot(sq1, 1)
psq2 = create_square_plot(sq2, 2)
psq3 = create_square_plot(sq3, 3)
psq4 = create_square_plot(sq4, 4)
psq6 = create_square_plot(sq6, 6)
psq9 = create_square_plot(sq9, 9)
psq12 = create_square_plot(sq12, 12)
psq13 = create_square_plot(sq13, 13)
psq44 = create_square_plot(sq44, 44)


psq1 + psq2 + psq3 + 
  psq4 + psq6 + psq9 +
  psq12 + psq13 + psq44 +
  plot_layout(ncol = 3)

ggsave(filename = "purini.png", 
       plot = last_plot(), 
       path = here::here("purini_classificazione", "outputs"),
       device = "png", width = 12, height = 12, units = "in",
       dpi = 300)




