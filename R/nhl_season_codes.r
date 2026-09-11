#' Generate a sequence of NHL season codes
#'
#' @param start_year First season's start year (e.g. 2021 for the 2021-22 season)
#' @param end_year Last season's start year (e.g. 2025 for the 2025-26 season)
#'
#' @description Small helper for building a multi-season codes vector to
#' pass to \code{\link{build_schedule_history}} or
#' \code{\link{build_player_history}}.
#'
#' @return A character vector of YYYYYYYY season codes
#' @export
#'
#' @examples
#' \dontrun{
#' nhl_season_codes(2021, 2025)
#' #> "20212022" "20222023" "20232024" "20242025" "20252026"
#' }
nhl_season_codes <- function(start_year, end_year){
  assertthat::assert_that(
    end_year >= start_year,
    msg = "`end_year` must be >= `start_year`"
  )
  paste0(start_year:end_year, (start_year:end_year) + 1)
}
