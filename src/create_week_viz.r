#' Create NHL Weekly Schedule Visualization
#'
#' @param week_number The week number for which to generate the schedule visualization
#' @param schedule_path Path to the RDS file containing the NHL schedule data
#' @param team_info_path Path to the RDS file containing the NHL team information data
#' @param output_path Path where the output PNG file should be saved
#'
#' @description This function generates a visual representation of the NHL schedule for a specified week.
#' It loads data, processes games, creates a ggplot visualization, and saves it as a PNG file.
#'
#' @details The function performs the following steps:
#' 1. Loads pre-saved NHL schedule and team color data
#' 2. Processes the games for the specified week
#' 3. Creates a ggplot visualization of the weekly schedule
#' 4. Saves the visualization as a PNG file
#'
#' @return NULL. The function saves the generated plot as a side effect.
#'
#' @note This function requires pre-saved data files:
#'   - 'nhl_schedule.RDS': containing the NHL schedule
#'   - 'nhl_team_info.RDS': containing team color information
#'
#' @examples
#' \dontrun{
#' create_nhl_week_visualization(
#'   week_number = 2,
#'   schedule_path = "/path/to/your/nhl_schedule.RDS",
#'   team_info_path = "/path/to/your/nhl_team_info.RDS",
#'   output_path = "/path/to/your/output/directory/"
#' )
#' }
#'
#' @import tidyverse ggplot2 lubridate dplyr ggimage cowplot extrafont tidyr ggchicklet grid
#' @importFrom rsvg rsvg_png
#' @importFrom httr GET
#' @importFrom png readPNG
#' @importFrom magick image_read
#'
#' @export
create_week_viz <- function(
    week_number,
    schedule_path = "/path/to/your/nhl_schedule.RDS",
    team_info_path = "/path/to/your/nhl_team_info.RDS",
    output_path = "/path/to/your/nhl_team_info.RDS") {
  # Input validation
  if (!is.numeric(week_number) || week_number < 1 || week_number > 52) {
    stop("Invalid week_number. Please provide a number between 1 and 52.")
  }

  # Check if input files exist
  if (!file.exists(schedule_path)) {
    stop("Schedule file not found: ", schedule_path)
  }
  if (!file.exists(team_info_path)) {
    stop("Team info file not found: ", team_info_path)
  }

  # Load required libraries
  suppressPackageStartupMessages({
    library(tidyverse)
    library(ggplot2)
    library(lubridate)
    library(ggimage)
    library(cowplot)
    library(extrafont)
    library(ggchicklet)
    library(grid)
  })

  # Load data
  nhl_schedule <- readRDS(schedule_path)
  team_colors <- readRDS(team_info_path)

  # Define constants
  BACKGROUND_COLOR <- "floralwhite"
  TEXT_COLOR_DEFAULT <- "black"

  # Helper function: theme_danilo
  theme_danilo <- function() {
    theme_minimal(base_size = 10, base_family = "Oswald SemiBold") %+replace%
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(
          fill = BACKGROUND_COLOR,
          color = BACKGROUND_COLOR
        )
      )
  }

  # Process games for the week
  process_games <- function(games, week_num, colors) {
    dias_semana_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")

    games %>%
      filter(week == week_num) %>%
      mutate(
        dia_semana = factor(dias_semana_pt[wday(gameDate, week_start = 1)],
          levels = dias_semana_pt
        ),
        hora = sub("^.*T([0-9]{2}:[0-9]{2}).*$", "\\1", startTimeBR_v2),
        jogo = paste(awayTeam.abbrev, "at", homeTeam.abbrev),
        logo_away = awayTeam_logo_light,
        logo_home = case_when(
          homeTeam.abbrev %in% c("TOR", "TBL") ~ gsub(
            "_light", "_dark",
            homeTeam_logo_light
          ),
          TRUE ~ homeTeam_logo_light
        ),
        cor_away = colors$team_secondary_color[match(
          awayTeam.abbrev,
          colors$team_abbr
        )],
        cor_home = colors$team_primary_color[match(
          homeTeam.abbrev,
          colors$team_abbr
        )],
      ) %>%
      select(
        id, gameDate, week, dia_semana, hora, jogo, logo_away, logo_home,
        cor_away, cor_home
      )
  }

  # Create weekly schedule plot
  create_schedule_plot <- function(week_games) {
    week_games$hora_ordenada <- as.POSIXct(
      paste(week_games$gameDate, week_games$hora),
      format = "%Y-%m-%d %H:%M"
    )

    week_games <- week_games %>%
      arrange(dia_semana, hora_ordenada) %>%
      group_by(dia_semana) %>%
      mutate(jogo_index = row_number()) %>%
      ungroup()

    data_inicio <- min(as.Date(week_games$gameDate), na.rm = TRUE)
    data_fim <- max(as.Date(week_games$gameDate), na.rm = TRUE)

    if (is.finite(data_inicio) && is.finite(data_fim)) {
      meses_pt <- c(
        "janeiro", "fevereiro", "marco", "abril", "maio", "junho",
        "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
      )

      data_inicio_pt <- format(data_inicio, "%d")
      data_fim_pt <- format(data_fim, "%d de %B de %Y")

      for (i in 1:12) {
        data_fim_pt <- sub(month.name[i], meses_pt[i], data_fim_pt,
          ignore.case = TRUE
        )
      }

      titulo <- paste(
        "Jogos da NHL: Semana", unique(week_games$week),
        "-", data_inicio_pt, "ate", data_fim_pt
      )
    } else {
      titulo <- paste("Jogos da NHL: Semana", unique(week_games$week))
    }

    dias_semana_ordem <- c("Dom", "Sab", "Sex", "Qui", "Qua", "Ter", "Seg")
    week_games$dia_semana <- factor(week_games$dia_semana,
      levels = dias_semana_ordem
    )

    ggplot(week_games, aes(x = dia_semana, y = jogo_index)) +
      ggchicklet:::geom_rrect(
        aes(
          xmin = as.numeric(dia_semana) - 0.40,
          xmax = as.numeric(dia_semana) - 0.045,
          ymin = jogo_index - 0.30,
          ymax = jogo_index + 0.30,
          fill = cor_away
        ),
        r = unit(0.15, "cm"),
        color = NA
      ) +
      ggchicklet:::geom_rrect(
        aes(
          xmin = as.numeric(dia_semana) + 0.045,
          xmax = as.numeric(dia_semana) + 0.40,
          ymin = jogo_index - 0.30,
          ymax = jogo_index + 0.30,
          fill = cor_home
        ),
        r = unit(0.15, "cm"),
        color = NA
      ) +
      geom_text(aes(label = hora),
        size = 2.5, vjust = 0.5,
        family = "Oswald SemiBold"
      ) +
      geom_image(aes(image = logo_away), size = 0.055, nudge_x = -0.225) +
      geom_image(aes(image = logo_home), size = 0.055, nudge_x = 0.225) +
      scale_fill_identity() +
      scale_x_discrete(expand = c(0.01, 0.01)) +
      scale_y_continuous(
        breaks = NULL,
        expand = c(0.01, 0.01)
      ) +
      labs(
        title = titulo,
        x = NULL,
        y = NULL
      ) +
      theme_danilo() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.25),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
      ) +
      coord_flip()
  }

  # Main execution
  tryCatch(
    {
      print(paste("Generating schedule for week:", week_number))

      jogos_semana <- process_games(nhl_schedule, week_number, team_colors)
      programacao_semanal <- create_schedule_plot(jogos_semana)

      output_file <- file.path(output_path, paste0(
        "NHL_week_", week_number,
        ".png"
      ))
      ggsave(output_file, programacao_semanal, width = 10, height = 8, dpi = 300)

      print(paste("Visualization saved to:", output_file))
    },
    error = function(e) {
      stop("An error occurred: ", e$message)
    }
  )
}

schedule_path <- "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/data/nhl_schedule.RDS"
team_info_path <- "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/data/nhl_team_info.RDS"
output_path <- "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/"

create_week_viz(
  week_number = 2,
  schedule_path,
  team_info_path,
  output_path
)
