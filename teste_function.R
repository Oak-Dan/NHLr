source("R/get_current_rosters.r")
source("R/get_team_rosters.r")
source("R/get_player_details.r")
source("R/get_season_players.r")
source("R/get_season_schedule.r")
source("R/nhl_season_codes.r")
source("R/build_schedule_history.r")
source("R/build_player_history.r")
source("R/load_team_info.r")   # nova, precisa vir junto
source("R/nhl_team_colors.r")
source("R/get_team_info.r")
source("R/save_team_info.r")

library(arrow)


#save_team_info()

# seasons <- nhl_season_codes(2026, 2026)   # 5 seasons: 2021-22 through 2025-26
# print(seasons)
# schedule: fast, ~32 requests per season
# build_schedule_history(seasons)

# players: slow, ~700-900 requests PER season (one per player) - maybe run
# one season at a time the first time, e.g.:
# build_player_history("20252026")
# once that works, do the rest:
# build_player_history(nhl_season_codes(2021, 2024))


# schedule <- open_dataset("Data/schedule") |> dplyr::collect()
# dplyr::glimpse(schedule)
# players_2025 <- open_dataset("Data/players") |> dplyr::filter(season == 20242025) |> dplyr::collect()


# players <- open_dataset("Data/players") |> dplyr::collect()
# dplyr::glimpse(players_2026)
# players |> dplyr::filter(is.na(team_abbr)) |> dplyr::select(player_id, first_name, last_name, team_id, team_abbr)


team_info <- arrow::read_parquet("Data/team_info.parquet")
dplyr::glimpse(team_info)

# e confere se o Utah veio certo agora:
team_info |> dplyr::filter(team_abbr == "UTA")
