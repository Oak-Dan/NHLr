#' Get the current list of active NHL teams with metadata
#'
#' @param colors A tibble of team colors keyed by \code{team_abbr}. Defaults
#' to \code{\link{nhl_team_colors}}, the curated table kept in code. Pass
#' NULL to skip colors entirely. A team returned by the API but missing
#' from this table (e.g. a brand-new expansion team) comes back with NA
#' colors and a message telling you to add it to
#' \code{R/nhl_team_colors.r}.
#'
#' @description Rebuilds the team reference table live instead of relying
#' on a static cached file, so relocations, rebrands, and new expansion
#' teams show up automatically. Combines three sources:
#' \itemize{
#' \item \code{api-web.nhle.com/v1/standings/now} for the current list of
#' active teams, full names, division, conference, and logos
#' \item \code{api.nhle.com/stats/rest/en/team} for each team's numeric ID
#' (matched by team code)
#' \item \code{\link{nhl_team_colors}} for colors, which have no API source
#' and are maintained by hand
#' }
#'
#' @return A tibble with columns: team_full_name, team_id, team_abbr,
#' division, conference, team_logo_light, team_logo_dark,
#' team_primary_color, team_secondary_color, team_text_color_home,
#' team_text_color_away
#' @export
#'
#' @examples
#' \dontrun{
#' team_info <- get_team_info()
#' }
get_team_info <- function(colors = nhl_team_colors()){

  standings <- tryCatch({
    resp <- httr::GET("https://api-web.nhle.com/v1/standings/now", httr::timeout(15))
    httr::stop_for_status(resp, task = "fetch standings")
    jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
  },
    error = function(cond){
      stop(paste("Could not fetch current team list:", conditionMessage(cond)))
    }
  )

  teams <- standings$standings |>
    dplyr::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(teamName, names_sep = "_") |>
    tidyr::unnest_wider(teamAbbrev, names_sep = "_") |>
    dplyr::transmute(
      team_full_name = teamName_default,
      team_abbr = teamAbbrev_default,
      division = divisionName,
      conference = conferenceName,
      team_logo_light = teamLogo,
      team_logo_dark = teamLogoDark
    ) |>
    dplyr::distinct(team_abbr, .keep_all = TRUE)

  # attach numeric team_id, matched by team code
  team_ids <- tryCatch({
    resp <- httr::GET("https://api.nhle.com/stats/rest/en/team", httr::timeout(15))
    httr::stop_for_status(resp, task = "fetch team IDs")
    site <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    site$data |>
      dplyr::tibble() |>
      tidyr::unnest_wider(1) |>
      dplyr::transmute(team_id = id, team_abbr = triCode) |>
      dplyr::distinct(team_abbr, .keep_all = TRUE)
  },
    error = function(cond){
      message(paste("Could not fetch team IDs, leaving team_id blank:", conditionMessage(cond)))
      dplyr::tibble(team_id = integer(0), team_abbr = character(0))
    }
  )

  teams <- teams |> dplyr::left_join(team_ids, by = "team_abbr")

  color_cols <- c(
    "team_primary_color", "team_secondary_color",
    "team_text_color_home", "team_text_color_away"
  )

  if(!is.null(colors) && all(c("team_abbr", color_cols) %in% names(colors))){
    teams <- teams |>
      dplyr::left_join(
        colors |> dplyr::select(team_abbr, dplyr::all_of(color_cols)),
        by = "team_abbr"
      )

    uncolored <- setdiff(teams$team_abbr, colors$team_abbr)
    if(length(uncolored) > 0){
      message(paste0(
        "No colors on file for: ", paste(uncolored, collapse = ", "),
        " - add them to R/nhl_team_colors.r"
      ))
    }

    retired <- setdiff(colors$team_abbr, teams$team_abbr)
    if(length(retired) > 0){
      message(paste0(
        "In nhl_team_colors() but not in the current NHL: ",
        paste(retired, collapse = ", "),
        " - they may have relocated or rebranded"
      ))
    }
  } else {
    if(!is.null(colors)){
      message("`colors` is missing expected columns - colors left blank.")
    }
    for(col in color_cols) teams[[col]] <- NA_character_
  }

  teams |>
    dplyr::select(
      team_full_name, team_id, team_abbr, division, conference,
      team_logo_light, team_logo_dark,
      dplyr::all_of(color_cols)
    ) |>
    dplyr::arrange(team_abbr)
}
