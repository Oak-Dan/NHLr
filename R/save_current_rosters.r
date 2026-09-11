#' Save current NHL rosters to disk for later use
#'
#' @param season_label A short label for the season, used in the file name
#' (e.g. 2025 for the 2024-25 season). Defaults to the current year.
#' @param season Passed through to \code{\link{get_current_rosters}}:
#' "current" (default) or a specific YYYYYYYY season like 20242025.
#' @param ... Additional arguments passed to \code{\link{get_current_rosters}}
#' (e.g. \code{teams}, \code{cores}).
#'
#' @description Fetches the current NHL rosters via
#' \code{\link{get_current_rosters}} and saves them to
#' \code{Data/nhl_rosters_{season_label}.RDS} — one file per season, so
#' next year's pull won't overwrite this year's. Mirrors how
#' \code{Data/nhl_team_info.RDS} is already used for team metadata: load
#' the saved file later with \code{readRDS()} instead of hitting the API
#' again.
#'
#' @return Invisibly returns the rosters tibble that was saved.
#' @export
#'
#' @examples
#' \dontrun{
#' save_current_rosters(2025)
#' rosters_2025 <- readRDS("Data/nhl_rosters_2025.RDS")
#' }
save_current_rosters <- function(season_label = format(Sys.Date(), "%Y"), season = "current", ...){

  assertthat::assert_that(
    is.character(season_label) || is.numeric(season_label),
    nchar(season_label) > 0,
    msg = "`season_label` must be a year like 2025"
  )

  rosters <- get_current_rosters(season = season, ...)

  out_dir <- "Data"
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  out_path <- file.path(out_dir, glue::glue("nhl_rosters_{season_label}.RDS"))

  if(file.exists(out_path)){
    message(glue::glue("{out_path} already exists — overwriting with this fetch."))
  }

  saveRDS(rosters, out_path)

  message(glue::glue("Saved {nrow(rosters)} players to {out_path}"))

  invisible(rosters)
}
