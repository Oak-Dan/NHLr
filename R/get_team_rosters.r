#' Get a team's roster
#'
#' @param team A three-letter NHL team code (e.g. "TOR", "BOS"). See
#' \code{\link{get_current_rosters}} for the full list of valid codes.
#' @param season A season in YYYYYYYY format (e.g. 20242025), or "current"
#' for the active roster as of right now. Defaults to "current".
#' @param timeout_sec Number of seconds to wait on the API before giving up.
#' Defaults to 10.
#'
#' @description Get a team's roster from the NHL's official web API
#' (api-web.nhle.com/v1/roster). Returns forwards, defensemen, and goalies
#' combined into a single tibble.
#'
#' Results are memoised (cached in memory) per unique
#' team/season/timeout_sec combination for the rest of the R session, so
#' calling this again for a team you've already fetched is instant and
#' makes no network request. Call \code{memoise::forget(get_team_rosters)}
#' to clear the cache and force fresh data.
#'
#' @return A tibble containing the roster for the specified team
#' @export
#'
#' @examples
#' \dontrun{
#' get_team_rosters("TOR")
#' get_team_rosters("BOS", season = 20232024)
#' }
get_team_rosters <- memoise::memoise(function(team, season = "current", timeout_sec = 10){

  assertthat::assert_that(
    is.character(team), length(team) == 1, nchar(team) > 0,
    msg = "`team` must be a single team code, e.g. \"TOR\""
  )

  team <- toupper(team)
  url <- glue::glue("https://api-web.nhle.com/v1/roster/{team}/{season}")

  site <- tryCatch({
    resp <- httr::GET(url, httr::timeout(timeout_sec))
    httr::stop_for_status(resp, task = paste("fetch roster for", team))
    jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
  },
    error = function(cond){
      message(paste0(
        "There was a problem fetching the roster for ", team, "\n\n",
        conditionMessage(cond)
      ))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop(paste("Could not get roster for team", team))
  }

  parse_group <- function(group){
    if(length(group) == 0) return(dplyr::tibble())
    group |>
      dplyr::tibble() |>
      tidyr::unnest_wider(1) |>
      tidyr::unnest_wider(firstName, names_sep = "_") |>
      tidyr::unnest_wider(lastName, names_sep = "_") |>
      tidyr::unnest_wider(birthCity, names_sep = "_") |>
      dplyr::rename(
        first_name = firstName_default,
        last_name = lastName_default,
        birth_city = birthCity_default
      )
  }

  rosters <- dplyr::bind_rows(
    parse_group(site$forwards),
    parse_group(site$defensemen),
    parse_group(site$goalies)
  )

  if(nrow(rosters) == 0){
    stop(paste("Could not get roster for team", team))
  }

  if(!"sweaterNumber" %in% names(rosters)) rosters$sweaterNumber <- NA_integer_

  rosters |>
    dplyr::mutate(
      player_name = paste(first_name, last_name),
      position = positionCode,
      position = ifelse(position %in% c("L","R"), paste0(position,"W"), position),
      position_type = dplyr::case_when(
        position %in% c("LW","RW","C") ~ "F",
        position == "D" ~ "D",
        position == "G" ~ "G",
        TRUE ~ NA_character_
      ),
      team_abbr = team
    ) |>
    dplyr::select(
      player_id = id,
      player_name,
      first_name,
      last_name,
      jersey_number = sweaterNumber,
      position,
      position_type,
      shoots_catches = shootsCatches,
      height_in = heightInInches,
      weight_lbs = weightInPounds,
      birth_date = birthDate,
      birth_city,
      birth_country = birthCountry,
      headshot,
      team_abbr
    )
})
