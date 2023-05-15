library(tidyverse)



# themes ------------------------------------------------------------------

theme_border <- 
  #theme_gray() + 
  theme(
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman"))


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



