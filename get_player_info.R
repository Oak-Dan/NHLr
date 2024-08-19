library(httr)
library(jsonlite)
library(dplyr)


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
#all_players_df <- bind_rows(player_list)


#saveRDS(all_players_df, "C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/player_info.RDS")
