library(ggplot2)

# colors ------------------------------------------------------------------


pfi_yellow <- "#ffca31"
pfi_red <- "#9e3e23"
pfi_tan <- "#e3d5cb"
pfi_blue <- "#00385f"
pfi_orange <- "#ca703d"
pfi_dkgreen <- "#1a431d"
pfi_green <- "#80921b"



# theme -------------------------------------------------------------------


my_wea_theme <- 
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) 


my_yld_theme <- 
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5),
        strip.text = element_text(size = rel(1.2)),
        panel.grid.minor.x = element_blank()) 