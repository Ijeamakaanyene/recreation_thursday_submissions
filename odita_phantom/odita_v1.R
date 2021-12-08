library(dplyr)
library(ggplot2)
library(purrr)


palette = c("A" = "#00489a", 
            "B" = "#008392", 
            "C" = "#ffa400", 
            "D" = "#fa876f")

create_triangles = function(x, y, direction){
  
  if(direction == "right"){
    
    triangles = bind_rows(
      tibble(x = c(x, x + 1, x + 0.625, x),
             y = c(y, y, y + 0.625, y + 1),
             tri = "A"),
      tibble(x = c(x, x + .5, x + 0.3125, x),
             y = c(y, y, y + .3125, y + .5),
             tri = "B"),
      tibble(x = c(x, x + 1, x + 0.625, x),
             y = c(y, y,  y - 0.625, y - 1),
             tri = "C"),
      tibble(x = c(x, x + .5, x + .3125, x),
             y = c(y, y, y - .3125, y - .5),
             tri = "D")
    )
    
  } else if(direction == "left"){
    
    triangles = bind_rows(
      tibble(x = c(x, x - 1, x - 0.625, x),
             y = c(y, y, y + 0.625, y + 1),
             tri = "A"),
      tibble(x = c(x, x - .5, x - 0.3125, x),
             y = c(y, y, y + .3125, y + .5),
             tri = "B"),
      tibble(x = c(x, x - 1, x - 0.625, x),
             y = c(y, y,  y - 0.625, y - 1),
             tri = "C"),
      tibble(x = c(x, x - .5, x - .3125, x),
             y = c(y, y, y - .3125, y - .5),
             tri = "D")
    )
    
  }
  
  return(triangles)
}

letter_groups = paste0(LETTERS, 1:200)


grids = expand.grid(
  x = seq(0, by = 2, length.out = 5),
  y = seq(0, by = 2, length.out = 5)
)

triangle_grids = map_dfr(1:nrow(grids),
                         ~bind_cols(
                           create_triangles(grids$x[.x],
                                            grids$y[.x],
                                            direction = "right"))) 

grid_length = nrow(triangle_grids) / 4


triangle_grids = triangle_grids %>%
  mutate(group = rep(letter_groups[1:grid_length], each = 4))


grids2 = expand.grid(
  x = seq(0, by = 2, length.out = 5),
  y = seq(-1, by = 2, length.out = 6)
)

triangle_grids2 = map_dfr(1:nrow(grids2),
                         ~bind_cols(
                           create_triangles(grids2$x[.x],
                                            grids2$y[.x],
                                            direction = "left"))) 

grid_length = nrow(triangle_grids2) / 4


triangle_grids2 = triangle_grids2 %>%
  mutate(group = rep(letter_groups[1:grid_length], each = 4))


ggplot() +
  geom_polygon(data = triangle_grids,
               aes(x = x, y = y, 
                   group = group,
                   fill = tri)) +
  geom_polygon(data = triangle_grids2,
               aes(x = x, y = y, 
                   group = group,
                   fill = tri)) +
  scale_fill_manual(values = palette) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#91988a",
                                        color = "#91988a"),
        plot.background = element_rect(fill = "#91988a",
                                       color = "#91988a"))


ggsave(filename = "odita_v1.png", 
       plot = last_plot(), 
       path = here::here("odita_phantom", "outputs"),
       device = "png", width = 9, height = 10.8, units = "in",
       dpi = 300)
