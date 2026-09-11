source("R/get_current_rosters.r")
source("R/get_team_rosters.r")
source("R/get_player_details.r")
source("R/get_season_players.r")
source("R/get_season_schedule.r")
source("R/nhl_season_codes.r")
source("R/build_schedule_history.r")
source("R/build_player_history.r")

seasons <- nhl_season_codes(2026, 2026)   # 5 seasons: 2021-22 through 2025-26
print(seasons)
# schedule: fast, ~32 requests per season
#build_schedule_history(seasons)

# players: slow, ~700-900 requests PER season (one per player) - maybe run
# one season at a time the first time, e.g.:
#build_player_history("20242025")
# once that works, do the rest:
#build_player_history(nhl_season_codes(2021, 2024))

library(arrow)
schedule <- open_dataset("Data/schedule") |> dplyr::collect()
dplyr::glimpse(schedule |> dplyr::filter(game_type == "REG"))
#players_2025 <- open_dataset("Data/players") |> dplyr::filter(season == 20242025) |> dplyr::collect()
