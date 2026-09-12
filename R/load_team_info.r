#' Load the team reference table from disk
#'
#' @param path Path to the Parquet team table. Defaults to
#' "Data/team_info.parquet", produced by \code{\link{save_team_info}}.
#' @param fallback_path Legacy .RDS file to fall back on if the Parquet
#' file doesn't exist yet. Defaults to "Data/nhl_team_info.RDS". Pass NULL
#' to disable the fallback.
#'
#' @description Single entry point for reading the team table, so every
#' function that needs team metadata (\code{\link{get_current_rosters}},
#' \code{\link{get_season_schedule}}, \code{\link{get_game_ids}}) reads the
#' same file from the same place.
#'
#' Prefers \code{Data/team_info.parquet}, which
#' \code{\link{save_team_info}} rebuilds live from the NHL API and keeps in
#' sync with \code{\link{nhl_team_colors}}. Falls back to the older
#' \code{Data/nhl_team_info.RDS} with a warning if the Parquet file hasn't
#' been generated yet - note the legacy file can be out of date (it still
#' has Utah as "Utah Hockey Club" with the pre-2025 colors), so regenerate
#' with \code{save_team_info()} when you see that warning.
#'
#' @return A tibble of team metadata, keyed by team_abbr
#' @export
#'
#' @examples
#' \dontrun{
#' teams <- load_team_info()
#' }
load_team_info <- function(path = "Data/team_info.parquet",
                           fallback_path = "Data/nhl_team_info.RDS"){

  if(file.exists(path)){
    return(arrow::read_parquet(path))
  }

  if(!is.null(fallback_path) && file.exists(fallback_path)){
    warning(paste0(
      path, " not found - falling back to ", fallback_path,
      ", which may be out of date. Run save_team_info() to rebuild it."
    ))
    return(readRDS(fallback_path))
  }

  stop(paste0(
    "Could not find ", path,
    " - run save_team_info() first, from the repo root (where Data/ lives)"
  ))
}
