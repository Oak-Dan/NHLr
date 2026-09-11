#' Build (or extend) a multi-season player dataset on disk
#'
#' @param seasons Character or numeric vector of season codes, e.g. from
#' \code{\link{nhl_season_codes}}.
#' @param path Directory to write the partitioned Parquet dataset to.
#' Defaults to "Data/players".
#' @param ... Passed through to \code{\link{get_season_players}}
#' (e.g. \code{teams}, \code{cores}).
#'
#' @description Same idea as \code{\link{build_schedule_history}}, but for
#' full player details (\code{\link{get_season_players}}). Requires the
#' \pkg{arrow} package.
#'
#' This is much slower than the schedule builder - one API request per
#' player per season, roughly 700-900 calls each - so budget time for it,
#' and consider running one season at a time (or with fewer \code{teams})
#' if you hit rate limits.
#'
#' Read the saved dataset back with:
#' \code{arrow::open_dataset("Data/players") \%>\% dplyr::collect()}
#'
#' @return Invisibly returns a combined tibble of everything just fetched
#' @export
#'
#' @examples
#' \dontrun{
#' build_player_history(nhl_season_codes(2021, 2025))
#' }
build_player_history <- function(seasons, path = "Data/players", ...){

  assertthat::assert_that(
    is.character(seasons) || is.numeric(seasons), length(seasons) >= 1,
    msg = "`seasons` must be one or more season codes"
  )
  assertthat::assert_that(
    requireNamespace("arrow", quietly = TRUE),
    msg = "The `arrow` package is required - install.packages(\"arrow\")"
  )

  all_players <- list()

  for(s in seasons){
    message(glue::glue("Fetching players for season {s}..."))

    players <- tryCatch(
      get_season_players(season = s, ...),
      error = function(cond){
        message(glue::glue("Skipping season {s}: {conditionMessage(cond)}"))
        return(NULL)
      }
    )

    if(is.null(players)) next

    arrow::write_dataset(
      players,
      path = path,
      format = "parquet",
      partitioning = "season",
      existing_data_behavior = "delete_matching"
    )

    message(glue::glue("Saved {nrow(players)} players for season {s}"))
    all_players[[as.character(s)]] <- players
  }

  invisible(dplyr::bind_rows(all_players))
}
