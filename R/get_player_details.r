#' Get one player's full bio & draft details
#'
#' @param player_id NHL player ID, as returned by \code{\link{get_team_rosters}}
#' or \code{\link{get_current_rosters}} (their \code{player_id} column).
#' @param timeout_sec Seconds to wait on the API before giving up. Defaults to 10.
#'
#' @description Fetches a single player's full profile from the NHL's
#' official web API (api-web.nhle.com/v1/player/{id}/landing), including
#' height/weight, birth info, and draft details (year/team/overall pick) -
#' data the roster endpoint alone doesn't have.
#'
#' Memoised per player_id for the rest of the R session, so re-fetching the
#' same player later is instant. Call \code{memoise::forget(get_player_details)}
#' to force a refresh.
#'
#' @return A one-row tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_player_details(8478402)
#' }
get_player_details <- memoise::memoise(function(player_id, timeout_sec = 10){

  assertthat::assert_that(
    is.numeric(player_id) || is.character(player_id), length(player_id) == 1,
    msg = "`player_id` must be a single NHL player ID"
  )

  url <- glue::glue("https://api-web.nhle.com/v1/player/{player_id}/landing")

  site <- tryCatch({
    resp <- httr::GET(url, httr::timeout(timeout_sec))
    httr::stop_for_status(resp, task = paste("fetch player", player_id))
    jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
  },
    error = function(cond){
      message(paste0(
        "There was a problem fetching player ", player_id, "\n\n",
        conditionMessage(cond)
      ))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop(paste("Could not get details for player", player_id))
  }

  draft <- site$draftDetails

  dplyr::tibble(
    player_id = rlang::`%||%`(site$playerId, NA_integer_),
    team_id = rlang::`%||%`(site$currentTeamId, NA_integer_),
    team_abbr = rlang::`%||%`(site$currentTeamAbbrev, NA_character_),
    first_name = rlang::`%||%`(site$firstName$default, NA_character_),
    last_name = rlang::`%||%`(site$lastName$default, NA_character_),
    jersey_number = rlang::`%||%`(site$sweaterNumber, NA_integer_),
    position = rlang::`%||%`(site$position, NA_character_),
    headshot = rlang::`%||%`(site$headshot, NA_character_),
    height_cm = rlang::`%||%`(site$heightInCentimeters, NA_integer_),
    weight_kg = rlang::`%||%`(site$weightInKilograms, NA_integer_),
    birth_date = rlang::`%||%`(site$birthDate, NA_character_),
    birth_country = rlang::`%||%`(site$birthCountry, NA_character_),
    shoots_catches = rlang::`%||%`(site$shootsCatches, NA_character_),
    draft_year = rlang::`%||%`(draft$year, NA_integer_),
    draft_team = rlang::`%||%`(draft$teamAbbrev, NA_character_),
    draft_pick = rlang::`%||%`(draft$overallPick, NA_integer_)
  )
})
