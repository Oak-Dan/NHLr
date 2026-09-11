#' Get current NHL rosters
#'
#' @param season A season in YYYYYYYY format (e.g. 20242025), or "current"
#' for active rosters as of right now. Defaults to "current".
#' @param teams Optional character vector of three-letter team codes to
#' limit the pull to (e.g. \code{c("TOR","BOS")}). Defaults to \code{NULL},
#' which pulls every team found in \code{Data/nhl_team_info.RDS}.
#' @param cores Number of parallel workers to fetch team rosters with.
#' Defaults to 2. Set to 1 to fetch sequentially.
#'
#' @description Get the roster for every active NHL team (or a subset),
#' combined into one tibble. Player data is pulled from the NHL's official
#' web API (api-web.nhle.com/v1/roster) in parallel via \pkg{future.apply},
#' reusing \code{\link{get_team_rosters}}'s memoised (cached) fetches on
#' repeat calls. Team metadata (full name, division, conference, logos,
#' colors) is joined in from the cached \code{Data/nhl_team_info.RDS} —
#' the same file \code{\link{get_game_ids}} uses — so team names/colors
#' stay consistent across the whole package.
#'
#' If a single team's fetch fails (e.g. a transient network error), it is
#' skipped with a warning rather than aborting the whole pull.
#'
#' @return A tibble containing the current rosters for every requested team,
#' with player columns from \code{\link{get_team_rosters}} plus every
#' column in \code{Data/nhl_team_info.RDS} (team_full_name, team_id,
#' division, conference, logos, colors, etc.)
#' @export
#'
#' @examples
#' \dontrun{
#' current_rosters <- get_current_rosters()
#' atlantic_only <- get_current_rosters(teams = c("TOR","MTL","BOS","BUF"))
#' }
get_current_rosters <- function(season = "current", teams = NULL, cores = 2){

  assertthat::assert_that(
    is.numeric(cores), cores >= 1,
    msg = "`cores` must be a positive integer"
  )

  team_info_path <- "Data/nhl_team_info.RDS"
  assertthat::assert_that(
    file.exists(team_info_path),
    msg = paste0(
      "Could not find ", team_info_path,
      " - run this from the repo root (where the Data/ folder lives)"
    )
  )
  nhl_teams <- readRDS(team_info_path)

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

  roster_list <- future.apply::future_lapply(
    X = nhl_teams$team_abbr,
    FUN = function(tm){
      tryCatch(
        get_team_rosters(tm, season = season),
        error = function(cond){
          message(paste0("Skipping ", tm, ": ", conditionMessage(cond)))
          return(NULL)
        }
      )
    },
    future.seed = TRUE
  )

  rosters <- dplyr::bind_rows(roster_list)

  if(nrow(rosters) == 0){
    stop("Could not fetch rosters for any team")
  }

  missing_teams <- setdiff(nhl_teams$team_abbr, unique(rosters$team_abbr))
  if(length(missing_teams) > 0){
    warning(paste("Could not fetch rosters for:", paste(missing_teams, collapse = ", ")))
  }

  rosters |>
    dplyr::left_join(nhl_teams, by = "team_abbr")
}
