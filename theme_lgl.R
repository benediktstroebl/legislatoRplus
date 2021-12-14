# Custom theme "legislatoR"
theme_lgl <- function() {
  
  # font
  font <- "Corbel"
  # base theme
  theme_few() %+replace%
    
    theme(
      
      # plot grid line
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = alpha("grey40", 0.2)),
      
      # plot text properties
      plot.title = element_text(            
        family = font,            
        size = 18,                        
        hjust = 0.5,
        margin = margin(0, 0, 5, 0)),
      
      plot.subtitle = element_text(          
        family = font,            
        size = 12,
        margin = margin(0, 0, 5, 0)),               
      
      plot.caption = element_text(
        family = font,            
        size = 10,                 
        hjust = 1,
        margin = margin(5, 0, 0, 0)),
      
      axis.title = element_text(            
        family = font,            
        size = 10),               
      
      axis.text.x = element_text(   
        family = font, 
        size = 8), 
      
      axis.text.y = element_text(   
        family = font, 
        size = 8)
      
    )
  
}