theme_juan <- function (base_size = base_size) {
  theme_bw(base_size = base_size) %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black", size=rel(1)),
      axis.title.y = element_text(colour = "black", angle=90),
      
      strip.background = element_blank(), 
      strip.text = element_text(size = rel(1.1)),#,face = "bold"),
      
      # panel.border = element_blank(),
      axis.line    = element_line(color='black'),
      
      panel.grid.major.y = element_line(linetype =  "dotted"),
      panel.grid.minor = element_blank(),
      
      panel.grid.major.x = element_line(linetype =  "dotted") 
    )   
}

# ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + 
#   stat_smooth(method = 'lm') + facet_wrap(~ cyl) + theme_juan(9)
