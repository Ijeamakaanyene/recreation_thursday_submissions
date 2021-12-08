library(dplyr)
library(ggplot2)
library(purrr)


get_sides = function(df){
  
  sides = list(
    x1 = df$x[3],
    y1 = df$y[3],
    x2 = df$x[2],
    y2 = df$y[2],
    x3 = df$x[1],
    y3 = df$y[1],
    x4 = df$x[4],
    y4 = df$y[4]
    
  )
  
  return(sides)
}

create_noise = function(input_x, input_y, freq){
  noise = ambient::gen_perlin(input_x,
                              input_y,
                              frequency = freq,
                              interpolator = "quintic"
  )
}

# create nested squares

len = c(pi/4, (pi/2 + pi/4),
        (pi + pi/4), (3*pi/2 + pi/4))
radius = seq(1, 5, by = 1)
squares = list()

for(i in seq_along(radius)){
  squares[[i]] = tibble(
    x = sin(len) * radius[i],
    y = cos(len) * radius[i],
    group = letters[i],
    len = len
  )
}

squared_df = bind_rows(squares)

ggplot(squared_df) +
  geom_path(aes(x = x, 
                   y = y,
                   group = group))


# create lines between nested squares
num_squares = length(unique(squared_df$group))
all_groups = unique(squared_df$group)
nlines = 500
input_lines = list()
output_lines = list()
cyan_palette = c("white",
                 "#005fb8",
                 "#004794",
                 "#00336A")

for(i in 1:4){
  
  # input lines list
  input_lines[[i]] = squared_df %>%
    filter(group == all_groups[i]) %>%
    get_sides()
  
  input_lines[[i+1]] = squared_df %>%
    filter(group == all_groups[i+1]) %>%
    get_sides()
  
  # output lines
  output_lines[[i]] = tibble(
    x = c(seq(input_lines[[i]]$x1, input_lines[[i]]$x2, length.out = nlines) + runif(nlines, -.1, .1),
          rep(input_lines[[i]]$x2, nlines) + runif(nlines, -.1, .1),
          seq(input_lines[[i]]$x3, input_lines[[i]]$x4, length.out = nlines) + runif(nlines, -.1, .1),
          rep(input_lines[[i]]$x4, nlines) + runif(nlines, -.1, .1)),
    xend = c(seq(input_lines[[i+1]]$x1, input_lines[[i+1]]$x2, length.out = nlines) + runif(nlines, -.1, .1),
             rep(input_lines[[i+1]]$x2, nlines) + runif(nlines, -.1, .1),
             seq(input_lines[[i+1]]$x3, input_lines[[i+1]]$x4, length.out = nlines) + runif(nlines, -.1, .1),
             rep(input_lines[[i+1]]$x4, nlines) + runif(nlines, -.1, .1)),
    y = c(rep(input_lines[[i]]$y1, nlines) + runif(nlines, -.1, .1),
          seq(input_lines[[i]]$y2, input_lines[[i]]$y3, length.out = nlines) + runif(nlines, -.1, .1),
          rep(input_lines[[i]]$y3, nlines) + runif(nlines, -.1, .1),
          seq(input_lines[[i]]$y4, input_lines[[i]]$y1, length.out = nlines) + runif(nlines, -.1, .1)),
    yend = c(rep(input_lines[[i+1]]$y1, nlines) + runif(nlines, -.1, .1),
             seq(input_lines[[i+1]]$y2, input_lines[[i+1]]$y3, length.out = nlines) + runif(nlines, -.1, .1),
             rep(input_lines[[i+1]]$y3, nlines) + runif(nlines, -.1, .1),
             seq(input_lines[[i+1]]$y4, input_lines[[i+1]]$y1, length.out = nlines) + runif(nlines, -.1, .1)),
    color = cyan_palette[i]
    
  )
  
  
}

output_lines_df = bind_rows(output_lines) 

ggplot() +
  geom_segment(data = output_lines_df,
               aes(x = x,
                   xend = xend,
                   yend = yend,
                   y = y,
                   color = color),
               lineend = "round",
               size = .5) +
  scale_color_identity() +
  coord_equal() +
  theme_void()

ggsave(filename = "creech_v1.png", 
       plot = last_plot(), 
       path = here::here("Creech_Cyanotype", "outputs"),
       device = "png", width = 15, height = 15, units = "in",
       dpi = 300,
       limitsize = FALSE)


