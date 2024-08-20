library(httr)
library(jsonlite)
library(dplyr)
library(prismatic)

# Load the schedule data
teams_sched <- readRDS("C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/nhl_schedule.RDS")
teams <- sort(unique(teams_sched$awayTeam.abbrev))

# Initialize an empty DataFrame to store all team rosters
all_rosters_df <- data.frame()

for (team in teams) {
  # URL for the team roster
  url <- paste0("https://api-web.nhle.com/v1/roster/", team, "/20242025")
  
  # Make the API request
  response <- GET(url)
  
  # Parse the JSON response
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data, flatten = TRUE)
  
  
  # Extract player IDs and create data frames with position and team information
  forwards <- data.frame(player_id = json_data$forwards$id, 
                         position = "Forward", 
                         team_abbrev = team)
  
  defensemen <- data.frame(player_id = json_data$defensemen$id, 
                           position = "Defenseman", 
                           team_abbrev = team)
  
  goalies <- data.frame(player_id = json_data$goalies$id, 
                        position = "Goalie", 
                        team_abbrev = team)
  
  # Combine all positions into a single data frame for the current team
  roster_df <- bind_rows(forwards, defensemen, goalies)
  
  # Append the current team's roster to the overall roster DataFrame
  all_rosters_df <- bind_rows(all_rosters_df, roster_df)
}





# Define a vector of player IDs
player_ids <- all_rosters_df$player_id

# Initialize an empty list to store the data for each player
player_list <- list()

for (player_id in player_ids) {
  
  # Construct the API URL for each player
  url <- paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing")
  
  # Make the API request
  response <- GET(url)
  
  # Parse the JSON response
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data, flatten = TRUE)
  
  # Use if-else statements to check if each field exists
  player_data <- data.frame(
    player_id = ifelse(!is.null(json_data$playerId), json_data$playerId, NA),
    team_id = ifelse(!is.null(json_data$currentTeamId), json_data$currentTeamId, NA),
    current_team = ifelse(!is.null(json_data$currentTeamAbbrev), json_data$currentTeamAbbrev, NA),
    first_name = ifelse(!is.null(json_data$firstName$default), json_data$firstName$default, NA),
    last_name = ifelse(!is.null(json_data$lastName$default), json_data$lastName$default, NA),
    sweater_number = ifelse(!is.null(json_data$sweaterNumber), json_data$sweaterNumber, NA),
    position = ifelse(!is.null(json_data$position), json_data$position, NA),
    player_img = ifelse(!is.null(json_data$headshot), json_data$headshot, NA),
    height_cm = ifelse(!is.null(json_data$heightInCentimeters), json_data$heightInCentimeters, NA),
    weight_kg = ifelse(!is.null(json_data$weightInKilograms), json_data$weightInKilograms, NA),
    birth_date = ifelse(!is.null(json_data$birthDate), json_data$birthDate, NA),
    country = ifelse(!is.null(json_data$birthCountry), json_data$birthCountry, NA),
    shoots = ifelse(!is.null(json_data$shootsCatches), json_data$shootsCatches, NA),
    draft_year = ifelse(!is.null(json_data$draftDetails$year), json_data$draftDetails$year, NA),
    draft_team = ifelse(!is.null(json_data$draft$teamAbbrev), json_data$draft$teamAbbrev, NA),
    draft_pick = ifelse(!is.null(json_data$draft$overallPick), json_data$draft$overallPick, NA),
    stringsAsFactors = FALSE
  )
  
  # Append the data frame to the list
  player_list[[length(player_list) + 1]] <- player_data
  
}

# Combine all player data frames into a single data frame
all_players_df <- bind_rows(player_list)



#saveRDS(all_players_df, "C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/player_info.RDS")



# Custom theme
theme_owen <- function () { 
  theme_minimal(base_size=9, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}


library(tidyverse)

# Step 1: Convert birth_date to Date object
all_players_df$birth_date <- as.Date(all_players_df$birth_date, format="%Y-%m-%d")

# Step 2: Calculate age
all_players_df$age <- as.numeric(difftime(Sys.Date(), all_players_df$birth_date, units = "weeks")) / 52.25

# Step 3: Round the age to a whole number (optional)
all_players_df$age <- floor(all_players_df$age)

aaa <- all_players_df %>%
  group_by(current_team) %>%
  summarise(mean_height = mean(height_cm, na.rm = TRUE),
            mean_weight = mean(weight_kg, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE)) %>%
  ungroup()

# Calculate league averages
aaa$league_height_average <- mean(all_players_df$height_cm, na.rm = TRUE)
aaa$league_weight_average <- mean(all_players_df$weight_kg, na.rm = TRUE)
aaa$league_age_average <- mean(all_players_df$age, na.rm = TRUE)


aaa$zscore <- (aaa$mean_height - mean(all_players_df$height_cm))/var(all_players_df$height_cm)


height_df <- aaa |> 
  select(current_team, mean_height, league_height_average, zscore) |> 
  rename(mean_measure = mean_height,
         league_average = league_height_average)

height_df$measure <- "Altura"

#height_df$avg_zscore <- mean

weight_df <- aaa |> 
  select(current_team, mean_weight, league_weight_average) |> 
  rename(mean_measure = mean_weight,
         league_average = league_weight_average)

weight_df$zscore <- (aaa$mean_weight - mean(all_players_df$weight_kg))/var(all_players_df$weight_kg)

weight_df$measure <- "Peso"

age_df <- aaa |> 
  select(current_team, mean_age, league_age_average) |> 
  rename(mean_measure = mean_age,
         league_average = league_age_average)

age_df$zscore <- (aaa$mean_age - mean(all_players_df$age))/var(all_players_df$age)

age_df$measure <- "Idade"



measures_teams_nhl <- bind_rows(list(height_df, weight_df, age_df))



# convert foul type to factor
measures_teams_nhl$measure <- as.factor(measures_teams_nhl$measure)

# order factor levels
measures_teams_nhl$measure <- factor(measures_teams_nhl$measure, levels = c("Peso", "Altura", "Idade")) 


# order legend
legendOrder <- c("Peso", "Altura", "Idade")

measures_teams_nhl$current_team_Duplicates <- measures_teams_nhl$current_team



# Plot the data
measures_teams_nhl %>%
  ggplot(aes(x = zscore, y = measure)) +
  # jitter background points
  geom_jitter(data = mutate(measures_teams_nhl, current_team = NULL), 
              aes(group = current_team_Duplicates), 
              height = 0.05, 
              size = 1, 
              color = 'gray80', 
              alpha = .25) +
  # make mini multiples, sort by avg z-score
  facet_wrap(~fct_reorder(current_team, current_team), 
             nrow = 8, 
             strip.position = 'top') +
  # add vertical line at 0
  geom_vline(xintercept = 0, 
             linetype = 'dashed', 
             size = .5, 
             color = 'gray50') +
  # add point for each referee
  geom_jitter(aes(group = current_team, 
                  fill = measure, 
                  color = after_scale(clr_darken(fill, 0.3))), 
              height = 0.05, 
              size = 1.5, 
              shape = 21, 
              alpha = 1) +
  geom_text(aes(label = paste0("(",round(mean_measure,2),")")),
            size = 1.5, # Adjust text size
            vjust = -0.5, # Adjust vertical position
            hjust = 0.5,
            nudge_x = 0.03,# Adjust horizontal position
            color = 'gray10') +
  # add color palette
  scale_fill_manual(values = c("#00B8AAFF", 
                               "#FD625EFF",
                               "#625EEF"), 
                    breaks = rev(legendOrder)) +
  scale_color_manual(values = c("#00B8AAFF", 
                                "#FD625EFF",
                                "#625EEF"))  +
  # tweak x-axis
  #xlim(-0.15, 0.15) + 
  #scale_x_continuous(limits = c(-0.15, 0.15)) + 
  # turn off coord clipping
  coord_cartesian(clip = 'off') + 
  theme_owen() +
  # make theme tweaks
  theme(plot.title.position = 'plot',
        plot.title = element_text(face ='bold', size = 13),
        plot.subtitle = element_text(size = 8),
        strip.text.x = element_text(size = 6),
        panel.spacing.x = unit(1, "lines"), 
        plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 5), 
        axis.title.x = element_text(size = 7), 
        axis.text.y = element_blank(), 
        legend.position = 'top',
        legend.text = element_text(size = 6.5), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10)) +
  # tweak legend
  guides(fill = guide_legend(keyheight = .75)) +
  labs(fill = "",
       color = "",
       x = "Z-Score da Altura e Peso Médios",
       y = "",
       title = "Altura, Peso e Idade Média dos Jogadores por Time",
       subtitle = paste0("Comparando as médias dos times com a altura (",
                         round(height_df$league_average[1],2)," cm) , peso (",
                         round(weight_df$league_average[1],2)," kg) e idade (",
                         round(age_df$league_average[1],2),") média da liga"))
 
