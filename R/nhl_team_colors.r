#' NHL team colors
#'
#' @description Curated table of team colors, kept in code (and therefore in
#' git history) rather than in a binary data file, because team colors are
#' the one piece of team metadata with no NHL API source - they have to be
#' maintained by hand when a team rebrands.
#'
#' Used by \code{\link{get_team_info}} to attach colors to the live team
#' list. If the NHL adds or rebrands a team, add/update its row here.
#'
#' Text colors are chosen to stay readable on their matching background:
#' every pair here clears a 4.5:1 contrast ratio (WCAG AA).
#'
#' Last reviewed: 2026-09. Utah reflects the 2025 Mammoth rebrand
#' (Mountain Blue #6CACE3).
#'
#' @return A tibble: team_abbr, team_primary_color, team_secondary_color,
#' team_text_color_home, team_text_color_away
#' @export
#'
#' @examples
#' \dontrun{
#' nhl_team_colors()
#' }
nhl_team_colors <- function(){
  dplyr::tribble(
    ~team_abbr, ~team_primary_color, ~team_secondary_color, ~team_text_color_home, ~team_text_color_away,
    "ANA", "#F47A38", "#B9975B", "#000000", "#000000",
    "BOS", "#FFB81C", "#000000", "#000000", "#ffffff",
    "BUF", "#002654", "#FDBB30", "#ffffff", "#000000",
    "CAR", "#CC0000", "#000000", "#ffffff", "#ffffff",
    "CBJ", "#002654", "#CE1126", "#ffffff", "#ffffff",
    "CGY", "#C8102E", "#F1BE48", "#ffffff", "#000000",
    "CHI", "#CF0A2C", "#000000", "#ffffff", "#ffffff",
    "COL", "#6F263D", "#236192", "#ffffff", "#ffffff",
    "DAL", "#006847", "#8F8F8C", "#ffffff", "#000000",
    "DET", "#CE1126", "#FFFFFF", "#ffffff", "#000000",
    "EDM", "#041E42", "#FF4C00", "#ffffff", "#000000",
    "FLA", "#C8102E", "#041E42", "#ffffff", "#ffffff",
    "LAK", "#111111", "#A2AAAD", "#ffffff", "#000000",
    "MIN", "#154734", "#A6192E", "#ffffff", "#ffffff",
    "MTL", "#AF1E2D", "#192168", "#ffffff", "#ffffff",
    "NJD", "#CE1126", "#000000", "#ffffff", "#ffffff",
    "NSH", "#FFB81C", "#041E42", "#000000", "#ffffff",
    "NYI", "#00539B", "#F47D30", "#ffffff", "#000000",
    "NYR", "#0038A8", "#CE1126", "#ffffff", "#ffffff",
    "OTT", "#C52032", "#C2912C", "#ffffff", "#000000",
    "PHI", "#F74902", "#000000", "#000000", "#ffffff",
    "PIT", "#FCB514", "#000000", "#000000", "#ffffff",
    "SEA", "#001628", "#99D9D9", "#ffffff", "#000000",
    "SJS", "#006D75", "#000000", "#ffffff", "#ffffff",
    "STL", "#002F87", "#FCB514", "#ffffff", "#000000",
    "TBL", "#002868", "#FFFFFF", "#ffffff", "#000000",
    "TOR", "#00205B", "#FFFFFF", "#ffffff", "#000000",
    "UTA", "#6CACE3", "#000000", "#000000", "#ffffff",
    "VAN", "#00205B", "#00843D", "#ffffff", "#ffffff",
    "VGK", "#B4975A", "#333F42", "#000000", "#ffffff",
    "WPG", "#041E42", "#FFFFFF", "#ffffff", "#000000",
    "WSH", "#C8102E", "#041E42", "#ffffff", "#ffffff"
  )
}
