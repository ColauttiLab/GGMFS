# Simplified/clean theme for plotting
theme_black <- function (base_size = 12, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size=24),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=24,angle=90),
      axis.text.y = element_text(size=12),
      axis.ticks = element_blank(), 
      panel.background = element_rect(fill="black"),
      panel.border = element_blank(),
      plot.title=element_text(face="bold", size=24),
      legend.background = element_rect(fill="black"),
      legend.text = element_text(colour="white"),
      legend.title = element_text(colour="white")
    )   
}