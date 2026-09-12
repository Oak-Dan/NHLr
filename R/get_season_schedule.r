#' Get the full game schedule for a season
#'
#' @param season A season in YYYYYYYY format (e.g. 20242025).
#' @param teams Optional character vector of team codes to limit to. Since
#' every game appears in both teams' schedules, this mostly just controls
#' speed, not which games end up in the result.
#' @param cores Number of parallel workers. Defaults to 2.
#'
#' @description Pulls every game for a season from the NHL's official web
#' API (api-web.nhle.com/v1/club-schedule-season), one request per team, in
#' parallel, then de-duplicates (every game shows up in both teams'
#' schedules).
#'
#' Standardized, English-only column names, with no team colors/logos
#' baked in - join \code{\link{load_team_info}()} by \code{team_abbr} if you
#' need those.
#'
#' For a season that hasn't been played yet, \code{home_score}/\code{away_score}
#' will be entirely \code{NA} (no games have a result yet). For a season
#' too far in the future for the NHL to have published a schedule at all,
#' the underlying API returns an error for every team - this shows up as a
#' skipped season, not a crash.
#'
#' @return A tibble with one row per game
#' \describe{
#' \item{game_id}{NHL game ID}
#' \item{season}{Season, YYYYYYYY}
#' \item{game_type}{PRE, REG, POST, or ALLSTAR}
#' \item{game_date}{Date of the game}
#' \item{start_time_utc}{Scheduled start time, UTC}
#' \item{venue}{Arena name}
#' \item{neutral_site}{Logical - played at a neutral site}
#' \item{home_team_abbr}{Home team code}
#' \item{away_team_abbr}{Away team code}
#' \item{home_score}{Home team final score (NA if not yet played)}
#' \item{away_score}{Away team final score (NA if not yet played)}
#' \item{week}{Week of the REGULAR season, computed from game_date - week 1
#' is the week of the first REG game (the regular-season opener), not the
#' first game overall. Preseason (PRE) games therefore get a week number of
#' 0 or lower; playoff (POST) games continue counting up past the last
#' regular-season week.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' schedule_2025 <- get_season_schedule(20242025)
#' }
get_season_schedule <- function(season, teams = NULL, cores = 2){

  assertthat::assert_that(
    grepl("^[0-9]{8}$", as.character(season)),
    msg = "`season` must be an 8-digit season like 20242025"
  )
  assertthat::assert_that(
    is.numeric(cores), cores >= 1,
    msg = "`cores` must be a positive integer"
  )

  nhl_teams <- load_team_info()

  if(!is.null(teams)){
    teams <- toupper(teams)
    assertthat::assert_that(
      all(teams %in% nhl_teams$team_abbr),
      msg = paste(
        "Unknown team code(s):",
        paste(setdiff(teams, nhl_teams$team_abbr), collapse = ", ")
      )
    )
    nhl_teams <- nhl_teams |> dplyr::filter(team_abbr %in% teams)
  }

  old_plan <- future::plan()
  future::plan(future::multisession, workers = cores)
  on.exit(future::plan(old_plan), add = TRUE)

  games_list <- future.apply::future_lapply(
    X = nhl_teams$team_abbr,
    FUN = function(tm){
      url <- glue::glue("https://api-web.nhle.com/v1/club-schedule-season/{tm}/{season}")
      tryCatch({
        resp <- httr::GET(url, httr::timeout(15))
        httr::stop_for_status(resp, task = paste("fetch schedule for", tm))
        site <- jsonlite::fromJSON(
          httr::content(resp, as = "text", encoding = "UTF-8"),
          simplifyVector = FALSE
        )
        site$games
      },
        error = function(cond){
          message(paste0("Skipping ", tm, ": ", conditionMessage(cond)))
          return(NULL)
        }
      )
    },
    future.seed = TRUE
  )

  raw_games <- unlist(games_list, recursive = FALSE)

  if(length(raw_games) == 0){
    stop("Could not fetch schedule for any team")
  }

  games <- raw_games |>
    dplyr::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(venue, names_sep = "_") |>
    tidyr::unnest_wider(homeTeam, names_sep = "_") |>
    tidyr::unnest_wider(awayTeam, names_sep = "_")

  # a season with no games played yet (e.g. before it starts) has no
  # "score" field anywhere in the response, so the column never gets
  # created by unnest_wider - add it as all-NA rather than error out
  if(!"homeTeam_score" %in% names(games)) games$homeTeam_score <- NA_integer_
  if(!"awayTeam_score" %in% names(games)) games$awayTeam_score <- NA_integer_

  games <- games |>
    dplyr::mutate(
      game_type = dplyr::case_when(
        gameType == 1 ~ "PRE",
        gameType == 2 ~ "REG",
        gameType == 3 ~ "POST",
        gameType == 4 ~ "ALLSTAR",
        TRUE ~ NA_character_
      ),
      game_date = as.Date(gameDate)
    ) |>
    dplyr::select(
      game_id = id,
      season,
      game_type,
      game_date,
      start_time_utc = startTimeUTC,
      venue = venue_default,
      neutral_site = neutralSite,
      home_team_abbr = homeTeam_abbrev,
      away_team_abbr = awayTeam_abbrev,
      home_score = homeTeam_score,
      away_score = awayTeam_score
    ) |>
    dplyr::distinct(game_id, .keep_all = TRUE) |>
    dplyr::arrange(game_date)

  # anchor week 1 on the regular season's opening date, not the earliest
  # game overall (preseason games happen weeks before REG games start)
  reg_start <- suppressWarnings(min(
    games$game_date[games$game_type == "REG"],
    na.rm = TRUE
  ))

  if(is.infinite(reg_start)){
    message(
      "No REG games found in this pull - falling back to the earliest ",
      "game date for week numbering."
    )
    reg_start <- min(games$game_date)
  }

  games |>
    dplyr::mutate(
      week = as.integer(floor(as.numeric(
        difftime(game_date, reg_start, units = "weeks")
      )) + 1)
    )
}
