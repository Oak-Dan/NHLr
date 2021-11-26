theme_owen <- function () { 
  theme_minimal(base_size=11) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite',
                                     color = "floralwhite"),
      panel.grid = element_blank()
    )
}