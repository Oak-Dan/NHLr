#' Create NHL Calendar Visualization
#'
#' This function generates a visual calendar of NHL games for a specified team, year, and month.
#' It creates a formatted calendar image showing home and away games, with team logos and game times.
#'
#' @param team_abbrev Character string. The abbreviation of the NHL team (e.g., "TOR" for Toronto Maple Leafs).
#' @param year Numeric. The year for which to create the calendar.
#' @param month Numeric. The month for which to create the calendar (1-12).
#' @param schedule_path Character string. Path to the NHL schedule RDS file.
#' @param team_info_path Character string. Path to the NHL team info RDS file.
#' @param output_path Character string. Path to the output directory for saving the calendar image.
#'
#' @return This function does not return a value. Instead, it saves the generated calendar as a PNG file
#' in the specified directory.
#'
#' @import tidyverse ggplot2 lubridate dplyr ggimage cowplot extrafont tidyr ggchicklet grid
#' @importFrom rsvg rsvg_png
#' @importFrom httr GET
#' @importFrom png readPNG
#' @importFrom magick image_read image_write image_composite
#' @importFrom utils write.csv
#'
#' @export
create_calendar_viz <- function(
    team_abbrev, year, month,
    schedule_path = "/path/to/nhl_schedule.RDS",
    team_info_path = "/path/to/nhl_team_info.RDS",
    output_path = "/path/to/output/") {
  # Load required libraries
  suppressPackageStartupMessages({
    library(tidyverse)
    library(ggplot2)
    library(lubridate)
    library(dplyr)
    library(rsvg)
    library(ggimage)
    library(httr)
    library(cowplot)
    library(extrafont)
    library(tidyr)
    library(ggchicklet)
    library(grid)
    library(png)
    library(magick)
    library(purrr)
  })

  # Input validation
  if (!is.character(team_abbrev) || nchar(team_abbrev) != 3) {
    stop("team_abbrev must be a 3-letter character string")
  }
  if (!is.numeric(year) || year < 1900 || year > 2100) {
    stop("year must be a valid year")
  }
  if (!is.numeric(month) || month < 1 || month > 12) {
    stop("month must be a number between 1 and 12")
  }

  # Check if input files exist
  if (!file.exists(schedule_path)) {
    stop("Schedule file not found: ", schedule_path)
  }
  if (!file.exists(team_info_path)) {
    stop("Team info file not found: ", team_info_path)
  }

  # Load data
  nhl_schedule <- readRDS(schedule_path)
  team_colors <- readRDS(team_info_path)

  # Source the external script
  source("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/src/theme_danilo.r")

  # Get team colors
  team_color <- team_colors %>%
    filter(team_abbr == team_abbrev) %>%
    select(
      team_primary_color, team_secondary_color,
      team_text_color_home, team_text_color_away
    )

  # Global configurations
  BACKGROUND_COLOR <- "floralwhite"
  TEXT_COLOR_DEFAULT <- "black"
  HOME_COLOR <- team_color$team_primary_color
  AWAY_COLOR <- team_color$team_secondary_color
  TEXT_COLOR_HOME <- team_color$team_text_color_home
  TEXT_COLOR_AWAY <- team_color$team_text_color_away

  # Convert month to numeric
  month <- as.numeric(month)

  # Helper functions
  download_and_convert_svg <- function(url, png_path,
                                       width = 1000, height = 1000) {
    temp_svg <- tempfile(fileext = ".svg")
    GET(url, write_disk(temp_svg, overwrite = TRUE))
    rsvg_png(temp_svg, png_path, width = width, height = height)
    unlink(temp_svg)
    return(png_path)
  }

  # Function to process game data (monthly view)
  process_games <- function(games, team_abbrev) {
    games %>%
      mutate(
        date = as.Date(gameDate),
        opponent = if_else(homeTeam.abbrev == team_abbrev,
          awayTeam.placeName.default,
          homeTeam.placeName.default
        ),
        team_abbreviated = if_else(homeTeam.abbrev == team_abbrev,
          awayTeam.abbrev,
          homeTeam.abbrev
        ),
        logo_url = if_else(homeTeam.abbrev == team_abbrev,
          awayTeam_logo_light,
          homeTeam_logo_light
        ),
        time = sub(
          "^.*T([0-9]{2}:[0-9]{2}).*$", "\\1",
          startTimeUTC
        ),
        location = case_when(
          neutralSite == TRUE ~ "Special",
          homeTeam.abbrev == team_abbrev ~ "Home",
          TRUE ~ "Away"
        )
      ) %>%
      select(
        id, date, opponent, team_abbreviated,
        logo_url,
        time,
        location
      )
  }

  # Function to prepare calendar data (monthly view)
  prepare_calendar_data <- function(year, month, games,
                                    home_color, away_color, text_color_home,
                                    text_color_away) {
    month_start <- as.Date(paste(year, month, "01", sep = "-"))
    month_end <- ceiling_date(month_start, "month") - days(1)
    all_days <- seq(month_start, month_end, by = "day")

    weekdays_en <- c(
      "Mon", "Tue",
      "Wed", "Thu",
      "Fri", "Sat",
      "Sun"
    )

    calendar <- data.frame(
      date = all_days,
      day = day(all_days),
      weekday = factor(
        weekdays_en[wday(all_days,
          week_start = 1
        )],
        levels = weekdays_en
      )
    ) %>%
      left_join(games, by = "date") %>%
      mutate(
        week = as.numeric(format(date, "%W")) -
          as.numeric(format(
            as.Date(paste(year, month, "01", sep = "-")),
            "%W"
          )) + 1,
        background_color = case_when(
          location == "Home" ~ home_color,
          location == "Away" ~ away_color,
          location == "Special" ~ "white",
          TRUE ~ BACKGROUND_COLOR
        ),
        text_color = case_when(
          location == "Home" ~ text_color_home,
          location == "Away" ~ text_color_away,
          TRUE ~ TEXT_COLOR_DEFAULT
        )
      )

    return(calendar)
  }

  # Main function to create monthly calendar
  create_game_calendar <- function(year, month, games, logo_png_team,
                                   team_abbrev, home_color, away_color,
                                   text_color_home,
                                   text_color_away) {
    calendar <- prepare_calendar_data(
      year, month, games,
      home_color, away_color, text_color_home,
      text_color_away
    )

    months_en <- c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
    month_name <- months_en[as.numeric(month)]

    img <- logo_png_team

    # Create a data frame for the legend
    legend_data <- data.frame(
      x = c(2, 2.8),
      y = 1,
      label = c("Home", "Away"),
      color = c(home_color, away_color),
      text_color = c(text_color_home, text_color_away)
    )

    p_calendar <- ggplot(calendar, aes(x = weekday, y = week)) +
      geom_tile(aes(fill = background_color),
        color = "black", linewidth = 0.5
      ) +
      scale_fill_identity() +
      geom_text(
        aes(label = day, color = text_color),
        size = 3, vjust = 1,
        nudge_x = -0.40, nudge_y = 0.45, family = "Oswald SemiBold"
      ) +
      geom_image(
        data = subset(calendar, !is.na(logo_png)),
        aes(image = logo_png), size = 0.12, nudge_y = 0.2
      ) +
      geom_text(
        data = subset(calendar, !is.na(team_abbreviated)),
        aes(label = team_abbreviated, color = text_color), size = 3.5,
        vjust = 1.2, nudge_y = -0.09,
        family = "Oswald SemiBold"
      ) +
      geom_text(
        data = subset(calendar, !is.na(time)),
        aes(label = time, color = text_color), size = 2.5,
        vjust = 1.2, nudge_y = -0.3,
        family = "Oswald SemiBold"
      ) +
      scale_color_identity() +
      scale_y_reverse() +
      coord_fixed() +
      labs(
        title = toupper(month_name),
        caption = c(
          "All dates and times are in Brasilia Time (BRT, UTC-3) and are subject to change.",
          "Author: Danilo Carvalho"
        ),
        x = NULL,
        y = NULL
      ) +
      theme_danilo() +
      theme(
        legend.position = "none",
        plot.title = element_text(
          hjust = 0.5, face = "bold",
          vjust = 8,
          family = "Oswald SemiBold",
          size = 16
        ),
        plot.caption = element_text(
          size = 8, hjust = c(0.02, 1),
          vjust = -5, color = "black"
        ),
        axis.text.x = element_text(
          face = "bold",
          family = "Oswald SemiBold",
          color = "black"
        ),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
      )

    # Create the legend plot
    p_legend <- ggplot(legend_data, aes(x = x, y = y)) +
      ggchicklet:::geom_rrect(
        aes(
          xmin = 1.75, xmax = 2.05, ymin = 0.9,
          ymax = 1.1,
          fill = home_color
        ),
        r = unit(0.2, "npc"),
        color = "black"
      ) +
      ggchicklet:::geom_rrect(
        aes(
          xmin = 2.55, xmax = 2.85, ymin = 0.9,
          ymax = 1.1,
          fill = away_color
        ),
        r = unit(0.2, "npc"),
        color = "black"
      ) +
      geom_text(aes(label = label, color = "black"),
        hjust = -0.5, nudge_x = 0.01, size = 2.5,
        family = "Oswald SemiBold"
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      xlim(0.5, 4.5) +
      ylim(0.5, 1.5) +
      theme_void() +
      theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

    # Save our inset plot
    ggsave(file.path(output_path, "p_legend.png"),
      p_legend,
      w = 3.85, h = 1.25, dpi = 300
    )

    set_null_device("png")

    ggdraw(p_calendar) +
      draw_image(img, x = 0.40, y = 0.42, scale = .25) + # inserting logo
      theme(plot.background = element_rect(fill = BACKGROUND_COLOR, color = NA))
  }

  # Main execution
  tryCatch(
    {
      print(paste("Generating monthly schedule for:", team_abbrev))
      if (!is.null(year) && !is.null(month)) {
        nhl_schedule_month <- nhl_schedule %>%
          filter(
            (homeTeam.abbrev == team_abbrev | awayTeam.abbrev == team_abbrev),
            year(gameDate) == year,
            month(gameDate) == month
          ) %>%
          process_games(team_abbrev)

        # Download and convert logos for monthly view
        nhl_schedule_month$logo_png <- purrr::map_chr(
          seq_len(nrow(nhl_schedule_month)),
          function(i) {
            png_path <- file.path(tempdir(), paste0("logo_", i, ".png"))
            download_and_convert_svg(nhl_schedule_month$logo_url[i], png_path)
          }
        )

        team_logo_abbrev_url <- file.path(
          "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/imgs/Logos/",
          paste0(team_abbrev, "_light.png")
        )

        # Create and save the monthly calendar
        calendar <- create_game_calendar(
          year, month,
          nhl_schedule_month, team_logo_abbrev_url, team_abbrev,
          HOME_COLOR, AWAY_COLOR, TEXT_COLOR_HOME, TEXT_COLOR_AWAY
        )

        plot_schedule <- ggdraw(calendar) +
          theme(plot.background = element_rect(
            fill = BACKGROUND_COLOR,
            color = NA
          ))

        ggsave(
          file.path(output_path, paste0(
            "monthly_calendar_",
            team_abbrev,
            "_", year, "_", month, ".png"
          )),
          plot_schedule,
          width = 6.5, height = 6.5, dpi = 300
        )
      }

      # Read in Inset plot
      inset <- image_read(file.path(output_path, "p_legend.png"))

      # Read in plot
      graph <- image_read(file.path(output_path, paste0(
        "monthly_calendar_",
        team_abbrev,
        "_", year, "_", month, ".png"
      )))

      # Combine images
      image_composite(graph, inset, offset = "+440+50") %>%
        image_write(file.path(
          output_path,
          paste0(
            "monthly_calendar_",
            team_abbrev,
            "_", year, "_", month, ".png"
          )
        ))
      print(paste0(
        "Visualization saved to:", output_path,
        "monthly_calendar_",
        team_abbrev,
        "_", year, "_", month, ".png"
      ))
    },
    error = function(e) {
        print(paste("An error occurred:", e$message))
        print("Stack trace:")
        print(sys.calls())
        stop(e)
      }

  )
}

schedule_path <- "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/data/nhl_schedule.RDS"
team_info_path <- "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/data/nhl_team_info.RDS"
output_path <- "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/imgs/Figures/"

create_calendar_viz(
  team_abbrev = "UTA", year = 2024, month = 10,
  schedule_path, team_info_path, output_path
)
