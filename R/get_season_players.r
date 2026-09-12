#' Get full player details for every roster spot in a season
#'
#' @param season A season in YYYYYYYY format (e.g. 20242025), or "current".
#' @param teams Optional character vector of team codes to limit to.
#' @param cores Number of parallel workers. Defaults to 4 - this fetches
#' one request per player (roughly 700-900 for a full season), so more
#' parallelism pays off more here than for \code{\link{get_current_rosters}}.
#'
#' @description Combines \code{\link{get_current_rosters}} (for the list of
#' players on each team) with \code{\link{get_player_details}} (for each
#' player's full bio and draft info), returning one row per player with
#' standardized, English-only column names.
#'
#' A player's individual "landing" profile can lag behind a recent trade or
#' signing - the NHL's roster endpoint already shows them on their new
#' team, but their own profile still comes back with no current team at
#' all (\code{team_id}/\code{team_abbr} both \code{NA}). When that happens,
#' this function falls back to the team the player was found under in
#' \code{\link{get_current_rosters}} instead of leaving it blank.
#'
#' This is slow the first time - one API call per player - but
#' \code{\link{get_player_details}} is memoised, so re-building the same
#' season again later in the session is instant.
#'
#' @return A tibble - see \code{\link{get_player_details}} for the player
#' columns, plus a \code{season} column.
#' @export
#'
#' @examples
#' \dontrun{
#' players_2025 <- get_season_players(20242025)
#' }
get_season_players <- function(season = "current", teams = NULL, cores = 4){

  assertthat::assert_that(
    is.numeric(cores), cores >= 1,
    msg = "`cores` must be a positive integer"
  )

  roster <- get_current_rosters(season = season, teams = teams)

  old_plan <- future::plan()
  future::plan(future::multisession, workers = cores)
  on.exit(future::plan(old_plan), add = TRUE)

  details_list <- future.apply::future_lapply(
    X = roster$player_id,
    FUN = function(pid){
      tryCatch(
        get_player_details(pid),
        error = function(cond){
          message(paste0("Skipping player ", pid, ": ", conditionMessage(cond)))
          return(NULL)
        }
      )
    },
    future.seed = TRUE
  )

  players <- dplyr::bind_rows(details_list)

  if(nrow(players) == 0){
    stop("Could not fetch details for any player")
  }

  missing_n <- nrow(roster) - nrow(players)
  if(missing_n > 0){
    warning(paste(missing_n, "player(s) could not be fetched - see messages above"))
  }

  # fall back to the roster's team info when a player's own landing page
  # hasn't caught up to a recent trade/signing yet (team_id/team_abbr NA)
  roster_teams <- roster |>
    dplyr::select(player_id, roster_team_abbr = team_abbr, roster_team_id = team_id) |>
    dplyr::distinct(player_id, .keep_all = TRUE)

  players <- players |>
    dplyr::left_join(roster_teams, by = "player_id") |>
    dplyr::mutate(
      used_roster_fallback = is.na(team_abbr) & !is.na(roster_team_abbr),
      team_abbr = dplyr::coalesce(team_abbr, roster_team_abbr),
      team_id = dplyr::coalesce(team_id, roster_team_id)
    ) |>
    dplyr::select(-roster_team_abbr, -roster_team_id)

  n_fallback <- sum(players$used_roster_fallback, na.rm = TRUE)
  if(n_fallback > 0){
    message(paste0(
      n_fallback, " player(s) had no current team on their own profile - ",
      "used the roster's team instead."
    ))
  }

  players <- players |> dplyr::select(-used_roster_fallback)

  players$season <- season

  players
}
