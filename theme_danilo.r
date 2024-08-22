BACKGROUND_COLOR <- "floralwhite" # nolint
TEXT_COLOR_DEFAULT <- "black" # nolint

theme_danilo <- function() {
  theme_minimal(base_size = 10, base_family = "Oswald SemiBold") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(
        fill = BACKGROUND_COLOR,
        color = BACKGROUND_COLOR
      )
    )
}
