library(ggplot2)
library(dplyr)
library(magick)

# Source of Art:
# https://www.moma.org/collection/works/205953

#Shoutout to Maya for these hex codes
green <- "#71A119"
red <- "#D5341B"
blue <- "#0F4999"
black <- "#2B2828"
yellow <- "#C5B856"
beige <- "#E5DECD"

left_lines = tibble(
  x = 2,
  xend = 3.5,
  y = c(2, 4.5, 4.75, 5, 6),
  yend = y + .01,
  color = c(red, black, blue, red, green)
)

right_lines = tibble(
  x = 7.5,
  xend = 9,
  y = c(5.8, 6.8, 7.05, 7.3),
  yend = y + .01,
  color = c(red, green, blue, black)
)

yellow_arc = tibble(
  len = seq(0, 3/4*pi, length.out = 45),
  x = (sin(len)*6) - .5,
  y = (cos(len)*5.5) + 7,
)

blue_arc = tibble(
  len = seq(5/4*pi, 7/4*pi, length.out = 45),
  x = (sin(len)*3) + 8,
  y = (cos(len)*3) + 2.5
)


ggplot() +
  geom_path(data = yellow_arc,
            aes(x = x, y = y),
            color = yellow,
            size = 1.25) +
  geom_path(data = blue_arc,
            aes(x = x, y = y),
            color = blue,
            size = 1.25) +
  # black horizontal line
  geom_segment(aes(x = 4.5, xend = 7.7,
                   y = 6, yend = 3),
               color = black,
               size = 1.25) +
  geom_segment(data = left_lines,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   color = color),
               size = 1.25) +
  geom_segment(data = right_lines,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   color = color),
               size = 1.25) +
  scale_color_identity() +
  coord_cartesian(expand = FALSE) +
  xlim(0, 10) +
  ylim(0, 10) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = beige, color = beige),
        plot.background = element_rect(fill = beige, colour = beige)) 

filename = paste0(stringr::str_remove_all(Sys.time(), "-| |:"),
                  ".png")

ggsave(filename = filename, plot = last_plot(), 
       device = "png", width = 6, height = 6, units = "in",
       dpi = 300)


### creating gif
imgs = list.files(here::here(), full.names = TRUE)
indices = stringr::str_detect(imgs, "png")
imgs = imgs[indices]

img_list = lapply(imgs, image_read)

## join the images together
img_joined = image_join(img_list)

## animate at 2 frames per second
img_animated = image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "hlito_curves_straight.gif")


