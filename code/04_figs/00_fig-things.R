library(tidyverse)


# fonts -------------------------------------------------------------------
library(extrafont)
fonts()
# font_import()
#loadfonts(device = "win")
# #--check if the font is already installed
#fonts()
# #--if not, download and import
# # https://medium.com/analytics-vidhya/5-easy-steps-to-import-new-fonts-into-r-6651bf263a07
# #--not working for me
# #font_import("../../../AppData/Local/Microsoft/Windows/Fonts/Chaparral Pro Regular.ttf")
# # extrafont::font_import("C:/Users/gina/AppData/Local/Microsoft/Windows/Fonts/Chaparral Pro Regular.ttf")
# extrafont::font_import("C:/Windows/Fonts/Merriweather-Regular.ttf")
# extrafont::font_import("../../../Downloads/chaparral_3qRw4/Chaparral/Chaparral Display/Chaparral Display.otf")

#--try another method
# https://www.r-bloggers.com/2019/03/adding-custom-fonts-to-ggplot-in-r/
library(showtext)
#font_add(family = "Chaparral", regular = "C:/Windows/Fonts/Chaparral Pro Regular.ttf")
font_add(family = "PFI", regular = "C:/Windows/Fonts/Chaparral Pro Regular.ttf")

library(dplyr)
library(ggplot2)
iris %>% 
  ggplot(aes(Sepal.Length,Sepal.Width, color = Species)) + 
  geom_point(size = 2) + 
  theme(
    text = element_text(family = "PFI")
  )

fonts()
# colors ------------------------------------------------------------------


pfi_yellow <- "#ffca31"
pfi_red <- "#9e3e23"
pfi_tan <- "#e3d5cb"
pfi_blue <- "#00385f"
pfi_orange <- "#ca703d"
pfi_dkgreen <- "#1a431d"
pfi_green <- "#80921b"

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3))


my_yield_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    text = element_text(family = "Chaparral")
  ) 

my_combo_theme <- 
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.1)),
    strip.text = element_text(size = rel(1.2)),
    strip.background = element_rect(fill = pfi_tan),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.1)),
    panel.grid.minor = element_blank(),
    # panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 1),
    #panel.border = element_blank(),
    #plot.title.position = "plot",
    plot.caption.position =  "plot"
  )
