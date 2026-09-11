#' Build (or extend) a multi-season schedule dataset on disk
#'
#' @param seasons Character or numeric vector of season codes, e.g. from
#' \code{\link{nhl_season_codes}}.
#' @param path Directory to write the partitioned Parquet dataset to.
#' Defaults to "Data/schedule".
#' @param ... Passed through to \code{\link{get_season_schedule}}
#' (e.g. \code{teams}, \code{cores}).
#'
#' @description Fetches \code{\link{get_season_schedule}} for each season
#' and writes it to a season-partitioned Parquet dataset (one subfolder per
#' season, e.g. \code{Data/schedule/season=20242025/}). Requires the
#' \pkg{arrow} package.
#'
#' Each season is fetched and written one at a time, so if a later season
#' fails, the seasons already written are kept - safe to re-run later with
#' more/different seasons without re-downloading or losing what's already
#' there. Re-running the same season overwrites just that season's data.
#'
#' Read the saved dataset back with:
#' \code{arrow::open_dataset("Data/schedule") \%>\% dplyr::collect()}
#' (or filter first, e.g. \code{dplyr::filter(season == 20242025)}, before
#' \code{collect()}, to only read what you need).
#'
#' @return Invisibly returns a combined tibble of everything just fetched
#' @export
#'
#' @examples
#' \dontrun{
#' build_schedule_history(nhl_season_codes(2021, 2025))
#' }
build_schedule_history <- function(seasons, path = "Data/schedule", ...){

  assertthat::assert_that(
    is.character(seasons) || is.numeric(seasons), length(seasons) >= 1,
    msg = "`seasons` must be one or more season codes"
  )
  assertthat::assert_that(
    requireNamespace("arrow", quietly = TRUE),
    msg = "The `arrow` package is required - install.packages(\"arrow\")"
  )

  all_games <- list()

  for(s in seasons){
    message(glue::glue("Fetching schedule for season {s}..."))

    games <- tryCatch(
      get_season_schedule(season = s, ...),
      error = function(cond){
        message(glue::glue("Skipping season {s}: {conditionMessage(cond)}"))
        return(NULL)
      }
    )

    if(is.null(games)) next

    arrow::write_dataset(
      games,
      path = path,
      format = "parquet",
      partitioning = "season",
      existing_data_behavior = "delete_matching"
    )

    message(glue::glue("Saved {nrow(games)} games for season {s}"))
    all_games[[as.character(s)]] <- games
  }

  invisible(dplyr::bind_rows(all_games))
}
