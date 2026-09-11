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

  players$season <- season

  players
}
