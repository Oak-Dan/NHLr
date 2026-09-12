#' Fetch and save the current NHL team reference table
#'
#' @param path Where to save the Parquet file. Defaults to
#' "Data/team_info.parquet".
#' @param colors Passed through to \code{\link{get_team_info}}.
#'
#' @description Runs \code{\link{get_team_info}} and saves the result to
#' Parquet. Unlike the schedule/player datasets, this isn't
#' season-partitioned - it's just a single, small, always-current table (32
#' rows), so re-running this overwrites the whole file.
#'
#' @return Invisibly returns the team info tibble that was saved.
#' @export
#'
#' @examples
#' \dontrun{
#' save_team_info()
#' }
save_team_info <- function(path = "Data/team_info.parquet", colors = nhl_team_colors()){

  team_info <- get_team_info(colors = colors)

  out_dir <- dirname(path)
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  arrow::write_parquet(team_info, path)

  message(glue::glue("Saved {nrow(team_info)} teams to {path}"))

  invisible(team_info)
}
