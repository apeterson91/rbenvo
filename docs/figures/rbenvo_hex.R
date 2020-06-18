library(hexSticker)


library(showtext)
library(ggplot2)
library(dplyr)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Raleway")
## Automatically use showtext to render text for future devices
showtext_auto()
p <- ggplot() + geom_line() + theme_void()

hexSticker::sticker(p, package="",
                    p_size = 7, p_x = 1, p_y = 1.5,
                    s_x=1,s_y=1,s_width=1.3,s_height=1,
                    h_fill = "#f9fcfc",h_color = "#060707",
                    p_color = "black",p_family = "Raleway",
                    filename="docs/figures/rbenvo_hex.png") ## combined with inkscape work
