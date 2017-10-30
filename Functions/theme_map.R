theme_map <- function (base_size = 24, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace% 
    theme(
#      plot.title=element_blank() 
      
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(), 
      axis.line = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA)
    )   
}


