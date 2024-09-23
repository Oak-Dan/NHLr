suppressPackageStartupMessages({
  library(tidyverse)
  library(prismatic)
  library(ggimage)
  library(patchwork)
})

# Source the external script
source("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/src/theme_danilo.r")

# Load data
player_info <- readRDS("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/data/player_info.RDS")

# Convert birth_date to Date object
player_info$birth_date <- as.Date(player_info$birth_date, format = "%Y-%m-%d")

# Calculate age
player_info$age <- as.numeric(difftime(Sys.Date(), player_info$birth_date, units = "weeks")) / 52.25

# Round the age to a whole number
player_info$age <- floor(player_info$age)

measures_df <- player_info |>
  group_by(current_team) |>
  summarise(
    mean_height = mean(height_cm, na.rm = TRUE),
    mean_weight = mean(weight_kg, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE)
  ) |>
  ungroup()

# Calculate league averages
measures_df$league_height_average <- mean(player_info$height_cm, na.rm = TRUE)
measures_df$league_weight_average <- mean(player_info$weight_kg, na.rm = TRUE)
measures_df$league_age_average <- mean(player_info$age, na.rm = TRUE)

# spliting the data into 3 dataframes and calculating z-scores
height_df <- measures_df |>
  select(current_team, mean_height, league_height_average) |>
  rename(
    mean_measure = mean_height,
    league_average = league_height_average
  )

# Calculate z-scores for height
height_df$zscore <- (height_df$mean_measure - mean(player_info$height_cm)) /
  var(player_info$height_cm)

height_df$measure <- "Height"

weight_df <- measures_df |>
  select(current_team, mean_weight, league_weight_average) |>
  rename(
    mean_measure = mean_weight,
    league_average = league_weight_average
  )

# Calculate z-scores for weight
weight_df$zscore <- (weight_df$mean_measure - mean(player_info$weight_kg)) /
  var(player_info$weight_kg)

weight_df$measure <- "Weight"

age_df <- measures_df |>
  select(current_team, mean_age, league_age_average) |>
  rename(
    mean_measure = mean_age,
    league_average = league_age_average
  )

# Calculate z-scores for age
age_df$zscore <- (age_df$mean_measure - mean(player_info$age)) /
  var(player_info$age)

age_df$measure <- "Age"

measures_teams_nhl <- bind_rows(height_df, weight_df, age_df)

measures_teams_nhl$current_team_Duplicates <- measures_teams_nhl$current_team

base_path <- "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/imgs/Logos/" # nolint
measures_teams_nhl <- measures_teams_nhl |>
  mutate(logo_path = file.path(base_path, paste0(current_team, "_light.png")))




create_measure_plot <- function(data, measure_name) {
  filtered_data <- data |> filter(measure == measure_name)

  min_zscore <- min(filtered_data$zscore)
  max_zscore <- max(filtered_data$zscore)

  min_team <- filtered_data |> filter(zscore == min(zscore))
  max_team <- filtered_data |> filter(zscore == max(zscore))

  measure_plot <- filtered_data |>
    ggplot(aes(x = zscore, y = measure)) +
    geom_vline(
      xintercept = 0, linetype = "dashed",
      linewidth = .5, color = "gray50"
    ) +
    geom_image(aes(image = logo_path, group = current_team),
      size = 0.135, asp = 16 / 9,
      position = position_jitter(width = 0, height = 0.12, seed = 42)
    ) +
    geom_curve(
      data = min_team,
      aes(x = zscore, xend = zscore, y = 1.5, yend = 1.15),
      arrow = arrow(length = unit(0.1, "cm")),
      color = "red", curvature = 0.3, linewidth = 0.25
    ) +
    geom_label(
      data = min_team,
      aes(
        x = zscore, y = 1.35,
        label = paste0(
          current_team, "\n", round(mean_measure, 1),
          ifelse(measure_name == "Height", " cm",
            ifelse(measure_name == "Weight", " kg", " years")
          )
        )
      ),
      hjust = 0.5, vjust = 0, color = "red", size = 1.75
    ) +
    geom_curve(
      data = max_team,
      aes(x = zscore, xend = zscore, y = 1.5, yend = 1.16),
      arrow = arrow(length = unit(0.1, "cm")),
      color = "blue", curvature = -0.3, linewidth = 0.25
    ) +
    geom_label(
      data = max_team,
      aes(
        x = zscore, y = 1.35,
        label = paste0(
          current_team, "\n", round(mean_measure, 1),
          ifelse(measure_name == "Height", " cm",
            ifelse(measure_name == "Weight", " kg", " years")
          )
        )
      ),
      hjust = 0.5, vjust = 0, color = "blue", size = 1.75
    ) +
    xlim(min_zscore - 0.05, abs(min_zscore) + 0.05) +
    coord_cartesian(clip = "off", ylim = c(0.8, 1.6)) +
    theme_danilo() +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
      plot.subtitle = element_text(size = 8, hjust = 0.5),
      strip.text.x = element_text(size = 4),
      panel.spacing.x = unit(1, "lines"),
      plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"),
      axis.text.x = element_text(size = 6),
      axis.title.x = element_text(size = 7),
      axis.text.y = element_blank()
    ) +
    labs(
      x = "Z-Score",
      y = "",
      title = paste("Average Player", measure_name, "by Team"),
      subtitle = paste0(
        "Comparing team averages for ",
        tolower(measure_name), " (League average: ",
        round(filtered_data$league_average[1], 2),
        ifelse(measure_name == "Height", " cm)",
          ifelse(measure_name == "Weight", " kg)", " years)")
        )
      )
    )

  return(measure_plot)
}

# Create plots for each measure
height_plot <- create_measure_plot(measures_teams_nhl, "Height")
weight_plot <- create_measure_plot(measures_teams_nhl, "Weight")
age_plot <- create_measure_plot(measures_teams_nhl, "Age")

# Combine plots vertically
combined_plot <- height_plot / weight_plot / age_plot +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    caption = "Author: Danilo Carvalho"
  ) &
  theme(
    plot.caption = element_text(
      hjust = 1, size = 6,
      family = "Oswald SemiBold"
    ),
    plot.background = element_rect(
      fill = "floralwhite",
      color = "floralwhite"
    )
  )

# Save the combined plot
ggsave("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/imgs/Figures/roster_measures_combined.png",
  combined_plot,
  width = 8, height = 8, dpi = 600
)
