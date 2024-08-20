suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(prismatic)
  library(ggimage)
})

source("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/src/theme_danilo.r")

# Load the schedule data
# teams_sched <- readRDS("C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/nhl_schedule.RDS")
# teams <- sort(unique(teams_sched$awayTeam.abbrev))

# Initialize an empty DataFrame to store all team rosters
# all_rosters_df <- data.frame()

# for (team in teams) {
#  # URL for the team roster
#  url <- paste0("https://api-web.nhle.com/v1/roster/", team, "/20242025")
#
#  # Make the API request
#  response <- GET(url)
#
#  # Parse the JSON response
#  data <- content(response, as = "text", encoding = "UTF-8")
#  json_data <- fromJSON(data, flatten = TRUE)
#
#  # Extract player IDs and create data frames with position and team information
#  forwards <- data.frame(
#    player_id = json_data$forwards$id,
#    position = "Forward",
#    team_abbrev = team
#  )
#
#  defensemen <- data.frame(
#    player_id = json_data$defensemen$id,
#    position = "Defenseman",
#    team_abbrev = team
#  )
#
#  goalies <- data.frame(
#    player_id = json_data$goalies$id,
#    position = "Goalie",
#    team_abbrev = team
#  )
#
#  # Combine all positions into a single data frame for the current team
#  roster_df <- bind_rows(forwards, defensemen, goalies)
#
#  # Append the current team's roster to the overall roster DataFrame
#  all_rosters_df <- bind_rows(all_rosters_df, roster_df)
# }
#
#
#
#
#
## Define a vector of player IDs
# player_ids <- all_rosters_df$player_id
#
## Initialize an empty list to store the data for each player
# player_list <- list()
#
# for (player_id in player_ids) {
#  # Construct the API URL for each player
#  url <- paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing")
#
#  # Make the API request
#  response <- GET(url)
#
#  # Parse the JSON response
#  data <- content(response, as = "text", encoding = "UTF-8")
#  json_data <- fromJSON(data, flatten = TRUE)
#
#  # Use if-else statements to check if each field exists
#  player_data <- data.frame(
#    player_id = ifelse(!is.null(json_data$playerId), json_data$playerId, NA),
#    team_id = ifelse(!is.null(json_data$currentTeamId),
#      json_data$currentTeamId, NA
#    ),
#    current_team = ifelse(!is.null(json_data$currentTeamAbbrev),
#      json_data$currentTeamAbbrev, NA
#    ),
#    first_name = ifelse(!is.null(json_data$firstName$default),
#      json_data$firstName$default, NA
#    ),
#    last_name = ifelse(!is.null(json_data$lastName$default),
#      json_data$lastName$default, NA
#    ),
#    sweater_number = ifelse(!is.null(json_data$sweaterNumber),
#      json_data$sweaterNumber, NA
#    ),
#    position = ifelse(!is.null(json_data$position),
#      json_data$position, NA
#    ),
#    player_img = ifelse(!is.null(json_data$headshot),
#      json_data$headshot, NA
#    ),
#    height_cm = ifelse(!is.null(json_data$heightInCentimeters),
#      json_data$heightInCentimeters, NA
#    ),
#    weight_kg = ifelse(!is.null(json_data$weightInKilograms),
#      json_data$weightInKilograms, NA
#    ),
#    birth_date = ifelse(!is.null(json_data$birthDate),
#      json_data$birthDate, NA
#    ),
#    country = ifelse(!is.null(json_data$birthCountry),
#      json_data$birthCountry, NA
#    ),
#    shoots = ifelse(!is.null(json_data$shootsCatches),
#      json_data$shootsCatches, NA
#    ),
#    draft_year = ifelse(!is.null(json_data$draftDetails$year),
#      json_data$draftDetails$year, NA
#    ),
#    draft_team = ifelse(!is.null(json_data$draft$teamAbbrev),
#      json_data$draft$teamAbbrev, NA
#    ),
#    draft_pick = ifelse(!is.null(json_data$draft$overallPick),
#      json_data$draft$overallPick, NA
#    ),
#    stringsAsFactors = FALSE
#  )
#
#  # Append the data frame to the list
#  player_list[[length(player_list) + 1]] <- player_data
# }

# Combine all player data frames into a single data frame
# all_players_df <- bind_rows(player_list)


# saveRDS(all_players_df, "C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/player_info.RDS")

all_players_df <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/player_info.RDS")
nhl <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/nhl_schedule.RDS")

aaa <- all_players_df %>%
  group_by(current_team) %>%
  summarise(
    mean_height = mean(height_cm, na.rm = TRUE),
    mean_weight = mean(weight_kg, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate league averages
aaa$league_height_average <- mean(all_players_df$height_cm, na.rm = TRUE)
aaa$league_weight_average <- mean(all_players_df$weight_kg, na.rm = TRUE)


aaa$zscore <- (aaa$mean_height - mean(all_players_df$height_cm)) / var(all_players_df$height_cm)


height_df <- aaa |>
  select(current_team, mean_height, league_height_average, zscore) |>
  rename(
    mean_measure = mean_height,
    league_average = league_height_average
  )

height_df$measure <- "Height"

# q()height_df$avg_zscore <- mean

weight_df <- aaa |>
  select(current_team, mean_weight, league_weight_average) |>
  rename(
    mean_measure = mean_weight,
    league_average = league_weight_average
  )

weight_df$zscore <- (aaa$mean_weight - mean(all_players_df$weight_kg)) / var(all_players_df$weight_kg)

weight_df$measure <- "Weight"

measures_teams_nhl <- bind_rows(height_df, weight_df)

# convert foul type to factor
measures_teams_nhl$measure <- as.factor(measures_teams_nhl$measure)

# order factor levels
measures_teams_nhl$measure <- factor(measures_teams_nhl$measure, levels = c("Height", "Weight"))


# order legend
legendOrder <- c("Height", "Weight")

measures_teams_nhl$current_team_Duplicates <- measures_teams_nhl$current_team

base_path <- "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/Logos/Logos Light/" # nolint
measures_teams_nhl <- measures_teams_nhl %>%
  mutate(logo_path = file.path(base_path, paste0(current_team, "_light.png")))

# Plot the data
p <- measures_teams_nhl %>%
  ggplot(aes(x = zscore, y = measure)) +
  # jitter background points
  geom_jitter(
    data = mutate(measures_teams_nhl, current_team = NULL),
    aes(group = current_team_Duplicates),
    height = 0.05,
    size = 1,
    color = "gray80",
    alpha = .25
  ) +
  # make mini multiples, sort by avg z-score
  facet_wrap(~ fct_reorder(current_team, current_team),
    nrow = 8,
    strip.position = "top"
  ) +
  # Add team logos
  geom_image(
    aes(image = logo_path),
    x = -0.05, y = 2.95,
    hjust = 0.5,
    size = 0.75,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  # add vertical line at 0
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = .5,
    color = "gray50"
  ) +
  # add point for each referee
  geom_jitter(
    aes(
      group = current_team,
      fill = measure,
      color = after_scale(clr_darken(fill, 0.3))
    ),
    height = 0.05,
    size = 1.5,
    shape = 21,
    alpha = 1
  ) +
  # add color palette
  scale_fill_manual(
    values = c(
      "#00B8AAFF",
      "#FD625EFF"
    ),
    breaks = rev(legendOrder)
  ) +
  scale_color_manual(values = c(
    "#00B8AAFF",
    "#FD625EFF"
  )) +
  # tweak x-axis
  xlim(-0.15, 0.15) + # scale_x_continuous(breaks = seq(-0.15, 0.05, 0.15)) +
  # turn off coord clipping
  coord_cartesian(clip = "off") +
  theme_danilo() +
  # make theme tweaks
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 8, hjust = 0.5),
    strip.text.x = element_text(size = 6),
    panel.spacing.x = unit(1, "lines"),
    plot.margin = margin(10, 10, 15, 10),
    axis.text.x = element_text(size = 5),
    axis.title.x = element_text(size = 7),
    axis.text.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 6.5),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, -10, -10, -10)
    # strip.text = element_text(margin = margin(t = 20, b = 5), hjust = 0.5),
    # strip.background = element_rect(fill = "white", color = "white")
  ) +
  # tweak legend
  guides(fill = guide_legend(keyheight = .75)) +
  labs(
    fill = "",
    color = "",
    x = "Z-Score of Average Height and Weight",
    y = "",
    title = "Average Player Height and Weight by Team",
    subtitle = "Comparing team averages to league-wide average height and weight" # nolint
  )


ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/roster_weight_height.png", # nolint
  p,
  width = 10, height = 8, dpi = 300
)
