# Load required packages
pacman::p_load(tidyverse, ggplot2, ggimage, lubridate, ggnewscal, ggforce)

#' Create NHL Schedule Plot
#'
#' @param schedule_data A dataframe containing NHL schedule data
#' @param team_info A dataframe containing team information
#' @param week_num The week number to create the schedule for
#' @param day Optional. A specific day to filter the schedule
#' @param base_path Path to the logo files
#' @param save_plot Logical. Whether to save the plot to a file
#'
#' @return A ggplot object of the NHL schedule
#'
#' @import tidyverse ggplot2 ggimage lubridate ggnewscale
#' @export
create_nhl_schedule <- function(schedule_data, team_info, week_num, 
                                day = NULL, 
                                #base_path = "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/imgs/Logos/Logos Light/", 
                                save_plot = TRUE) {
  
  # Input validation
  required_schedule_cols <- c("gameDate", "awayTeam.abbrev", "homeTeam.abbrev", "startTimeBR_v2", "week")
  required_team_cols <- c("team_abbr", "team_primary_color", "team_secondary_color", "team_full_name")
  
  if (!all(required_schedule_cols %in% names(schedule_data))) {
    stop("schedule_data is missing required columns")
  }
  if (!all(required_team_cols %in% names(team_info))) {
    stop("team_info is missing required columns")
  }
  
  # Process games for the week
  process_games <- function(games, week_num, colors, start_day_of_week) {
    days_of_week_en <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    games %>%
      filter(week == week_num) %>%
      mutate(
        day_of_week = factor(
          days_of_week_en[wday(gameDate, week_start = start_day_of_week)],
          levels = days_of_week_en
        ),
        time = sub("^.*T([0-9]{2}:[0-9]{2}).*$", "\\1", startTimeBR_v2),
        game = paste(awayTeam.abbrev, "at", homeTeam.abbrev),
        team = colors$team_full_name[match(homeTeam.abbrev, colors$team_abbr)],
        opponent = colors$team_full_name[match(awayTeam.abbrev, colors$team_abbr)],
        logo_away = awayTeam_logo_light,
        logo_home = case_when(
          homeTeam.abbrev %in% c("TOR", "TBL") ~ gsub("_light", "_dark", homeTeam_logo_light),
          TRUE ~ homeTeam_logo_light
        ),
        color_away = colors$team_secondary_color[match(awayTeam.abbrev, colors$team_abbr)],
        color_home = colors$team_primary_color[match(homeTeam.abbrev, colors$team_abbr)]
      ) %>%
      rename(
        home_team = homeTeam.abbrev,
        away_team = awayTeam.abbrev,
        date = gameDate
      ) %>%
      select(date, week, day_of_week, time, home_team, team,
             opponent, away_team, color_away, color_home)
  }
  
  # Process the schedule data
  nhl_schedule <- process_games(schedule_data, week_num, team_info, 1)
  
  # Add logo file paths
  #nhl_schedule <- nhl_schedule %>%
  #  mutate(
  #    VISITING_LOGO = file.path(base_path, paste0(away_team, "_light.png")),
  #    HOME_LOGO = file.path(base_path, paste0(home_team, "_light.png"))
  #  )
  
  # Filter for specific day if provided
  if (!is.null(day)) {
    nhl_schedule <- nhl_schedule %>%
      filter(lubridate::wday(date, label = TRUE) == day)
  }
  
  # Check if there are any games in the schedule
  if (nrow(nhl_schedule) == 0) {
    message("No games scheduled for the specified criteria.")
    return(NULL)
  }
  
  # Create the plot
  create_schedule_plot <- function(data) {
    data <- data %>%
      arrange(date) %>%
      group_by(date) %>%
      mutate(
        row_in_day = row_number(),
        day_start = min(seq_len(n())) + 0.5,
        day_label = if_else(row_in_day == 1, format(as.Date(date),
                                                    "%a, %b %d"), NA_character_)
      ) %>%
      ungroup() %>%
      mutate(overall_row = seq_len(n()))
    
    schedule_plot <- ggplot2::ggplot(data, ggplot2::aes(y = overall_row * 1.5,
                                                        fill = as.factor(date))) +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = -Inf, xmax = Inf,
        ymin = overall_row * 1.5 - 0.75,
        ymax = overall_row * 1.5 + 0.75
      ), alpha = 0.7) +
      ggplot2::scale_fill_manual(values = rep(c("#f0f0f0", "white"),
                                              length(unique(data$date)))) +
      ggplot2::scale_x_continuous(limits = c(0, 0.85),
                                  expand = c(0, 0)) +
      ggplot2::scale_y_reverse(limits = c((nrow(data) + 0.5) * 1.5, -1),
                               expand = c(0, 0)) +
      ggplot2::labs(
        title = if (is.null(day)) {
          paste("NHL Week", week_num, "Schedule")
        } else {
          paste("NHL Schedule -", format(min(as.Date(data$date)),
                                         "%A, %B %d, %Y"))
        },
        caption = c(
          "All times are in Brasilia Time (BRT, UTC-3) and are subject to change",
          "Author: Danilo Carvalho"
        )
      ) +
      theme_danilo() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(30, 10, 30, 10),
        plot.title = ggplot2::element_text(hjust = 0.5, vjust = 2, size = 12),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = "none",
        plot.caption = ggplot2::element_text(size = 8, hjust = c(0.02, 1),
                                             vjust = -5, color = "black")
      )
    
    schedule_plot <- schedule_plot + ggnewscale::new_scale_fill()
    
    # Add team color rectangles
    if (is.null(day)) {
      schedule_plot <- schedule_plot +
        ggforce::geom_regon(ggplot2::aes(
          x0 = 0.20, y0 = overall_row * 1.5,
          sides = 4, angle = 0, r = 0.02,
          fill = color_away
        ), n = 0.5, expand = unit(0.24, 'cm'), radius = unit(0.12, 'cm'),
        show.legend = FALSE) +
        ggforce::geom_regon(ggplot2::aes(
          x0 = 0.48, y0 = overall_row * 1.5,
          sides = 4, angle = 0, r = 0.02,
          fill = color_home
        ), n = 0.5, expand = unit(0.24, 'cm'), radius = unit(0.12, 'cm'),
        show.legend = FALSE)
    } else {
      schedule_plot <- schedule_plot +
        ggforce::geom_regon(ggplot2::aes(
          x0 = 0.10, y0 = overall_row * 1.5,
          sides = 4, angle = 0, r = 0.04,
          fill = color_away
        ), n = 4, show.legend = FALSE) +
        ggforce::geom_regon(ggplot2::aes(
          x0 = 0.38, y0 = overall_row * 1.5,
          sides = 4, angle = 0, r = 0.04,
          fill = color_home
        ), n = 4, show.legend = FALSE)
    }
    
    schedule_plot <- schedule_plot + ggplot2::scale_fill_identity()
    
    # Add text and images
    if (is.null(day)) {
      schedule_plot <- schedule_plot +
        ggplot2::geom_text(ggplot2::aes(x = 0.02, label = day_label),
                           hjust = 0, size = 3, family = "Oswald SemiBold",
                           na.rm = TRUE) +
        #ggimage::geom_image(ggplot2::aes(x = 0.20, image = VISITING_LOGO),
        #                    size = 0.025, asp = 1.5) +
        ggplot2::geom_text(ggplot2::aes(x = 0.26, label = opponent),
                           hjust = 0, size = 3.2, family = "Oswald SemiBold") +
        #ggimage::geom_image(ggplot2::aes(x = 0.48, image = HOME_LOGO),
        #                    size = 0.025, asp = 1.5) +
        ggplot2::geom_text(ggplot2::aes(x = 0.54, label = team),
                           hjust = 0, size = 3.2, family = "Oswald SemiBold") +
        ggplot2::geom_text(ggplot2::aes(x = 0.84, label = time),
                           hjust = 1, size = 2.8, family = "Oswald SemiBold")
      
      x_positions <- c(0.02, 0.26, 0.54, 0.84)
      headers <- c("Date", "Visiting Team", "Home Team", "Time")
    } else {
      schedule_plot <- schedule_plot +
        ggimage::geom_image(ggplot2::aes(x = 0.10, image = VISITING_LOGO),
                            size = 0.04, asp = 1.5) +
        ggplot2::geom_text(ggplot2::aes(x = 0.16, label = opponent),
                           hjust = 0, size = 3.2, family = "Oswald SemiBold") +
        ggimage::geom_image(ggplot2::aes(x = 0.38, image = HOME_LOGO),
                            size = 0.04, asp = 1.5) +
        ggplot2::geom_text(ggplot2::aes(x = 0.44, label = team),
                           hjust = 0, size = 3.2, family = "Oswald SemiBold") +
        ggplot2::geom_text(ggplot2::aes(x = 0.74, label = time),
                           hjust = 1, size = 2.8, family = "Oswald SemiBold")
      
      x_positions <- c(0.16, 0.44, 0.74)
      headers <- c("Visiting Team", "Home Team", "Time")
    }
    
    # Add headers
    for (i in seq_along(headers)) {
      schedule_plot <- schedule_plot + ggplot2::annotate("text",
                                                         x = x_positions[i],
                                                         y = -0.5,
                                                         label = headers[i],
                                                         hjust = ifelse(i == length(headers), 1, 0),
                                                         size = 3, family = "Oswald SemiBold"
      )
    }
    
    # Add separator line
    schedule_plot <- schedule_plot + ggplot2::annotate("segment",
                                                       x = 0, xend = 0.85,
                                                       y = 0.5, yend = 0.5,
                                                       color = "gray40",
                                                       linewidth = 0.5,
                                                       linetype = "solid"
    )
    
    # Add background
    schedule_plot <- schedule_plot + ggplot2::theme(
      plot.background = ggplot2::element_rect(color = "gray60",
                                              fill = "white", linewidth = 1)
    )
    
    return(schedule_plot)
  }
  
  # Create the plot
  schedule_plot <- create_schedule_plot(nhl_schedule)
  
  # Save the plot if requested
  if (save_plot) {
    plot_height <- if (is.null(day)) 10 else max(3, 3 + 0.5 * nrow(nhl_schedule))
    filename <- paste0(
      "nhl_schedule_week_", week_num,
      if (!is.null(day)) paste0("_", tolower(day)), ".png"
    )
    ggplot2::ggsave(filename, plot = schedule_plot, width = 6,
                    height = plot_height, dpi = 600)
  }
  
  return(schedule_plot)
}
setwd("c://Users/danilo.carvalho/Downloads/")
# Load data and source theme
schedule_data <- readRDS("nhl_schedule.RDS")
team_info <- readRDS("nhl_team_info.RDS")
source("theme_danilo.r")

# Example usage
# For the entire week
full_week_plot <- create_nhl_schedule(schedule_data, team_info, week_num = 2)

# For a specific day (e.g., Tuesday)
#tuesday_plot <- create_nhl_schedule(schedule_data, team_info, week_num = 2, day = "Tue")