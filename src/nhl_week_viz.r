library(tidyverse)
library(ggplot2)
library(ggimage)
library(lubridate)
library(ggnewscale)

create_nhl_schedule <- function(
    schedule_data, team_info,
    week_num, day = NULL) {
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
        time = sub(
          "^.*T([0-9]{2}:[0-9]{2}).*$", "\\1",
          startTimeBR_v2
        ),
        game = paste(awayTeam.abbrev, "at", homeTeam.abbrev),
        team = colors$team_full_name[match(
          homeTeam.abbrev,
          colors$team_abbr
        )],
        opponent = colors$team_full_name[match(
          awayTeam.abbrev,
          colors$team_abbr
        )],
        logo_away = awayTeam_logo_light,
        logo_home = case_when(
          homeTeam.abbrev %in% c("TOR", "TBL") ~ gsub(
            "_light",
            "_dark", homeTeam_logo_light
          ),
          TRUE ~ homeTeam_logo_light
        ),
        color_away = colors$team_secondary_color[match(
          awayTeam.abbrev,
          colors$team_abbr
        )],
        color_home = colors$team_primary_color[match(
          homeTeam.abbrev,
          colors$team_abbr
        )]
      ) %>%
      rename(
        home_team = homeTeam.abbrev,
        away_team = awayTeam.abbrev,
        date = gameDate
      ) %>%
      select(
        date, week, day_of_week, time, home_team,
        team, opponent, away_team, color_away, color_home
      )
  }

  # Process the schedule data
  nhl_schedule <- process_games(schedule_data, week_num, team_info, 1)

  # Add logo file paths
  base_path <- "/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/imgs/Logos/Logos Light/"
  nhl_schedule <- nhl_schedule %>%
    mutate(
      VISITING_LOGO = paste0(base_path, away_team, "_light.png"),
      HOME_LOGO = paste0(base_path, home_team, "_light.png")
    )

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
        day_label = if_else(row_in_day == 1, format(
          as.Date(date),
          "%a, %b %d"
        ), NA_character_)
      ) %>%
      ungroup() %>%
      mutate(overall_row = seq_len(n()))

    p <- ggplot(data, aes(y = overall_row * 1.5, fill = as.factor(date))) +
      geom_rect(aes(
        xmin = -Inf, xmax = Inf,
        ymin = overall_row * 1.5 - 0.75,
        ymax = overall_row * 1.5 + 0.75
      ), alpha = 0.7) +
      scale_fill_manual(values = rep(
        c("#f0f0f0", "white"),
        length(unique(data$date))
      )) +
      scale_x_continuous(limits = c(0, 0.85), expand = c(0, 0)) +
      scale_y_reverse(
        limits = c((nrow(data) + 0.5) * 1.5, -1),
        expand = c(0, 0)
      ) +
      labs(
        title = if (is.null(day)) {
          paste("NHL Week", week_num, "Schedule")
        } else {
          paste("NHL Schedule - ", format(
            min(as.Date(data$date)),
            "%A, %B %d, %Y"
          ))
        },
        caption = c(
          "All times are in Brasilia Time (BRT, UTC-3) and are subject to change",
          "Author: Danilo Carvalho"
        )
      ) +
      theme_danilo() +
      theme(
        plot.margin = margin(30, 10, 30, 10),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.caption = element_text(
          size = 8, hjust = c(0.02, 1),
          vjust = -5, color = "black"
        )
      )

    p <- p + new_scale_fill()

    if (is.null(day)) {
      p <- p +
        geom_rect(aes(
          xmin = 0.18, xmax = 0.22,
          ymin = overall_row * 1.5 - 0.7,
          ymax = overall_row * 1.5 + 0.7,
          fill = color_away
        ), show.legend = FALSE) +
        geom_rect(aes(
          xmin = 0.46, xmax = 0.50,
          ymin = overall_row * 1.5 - 0.7,
          ymax = overall_row * 1.5 + 0.7,
          fill = color_home
        ), show.legend = FALSE)
    } else {
      p <- p +
        geom_rect(aes(
          xmin = 0.06, xmax = 0.14,
          ymin = overall_row * 1.5 - 0.7,
          ymax = overall_row * 1.5 + 0.7,
          fill = color_away
        ), show.legend = FALSE) +
        geom_rect(aes(
          xmin = 0.34, xmax = 0.42,
          ymin = overall_row * 1.5 - 0.7,
          ymax = overall_row * 1.5 + 0.7,
          fill = color_home
        ), show.legend = FALSE)
    }

    p <- p + scale_fill_identity()

    if (is.null(day)) {
      p <- p +
        geom_text(aes(x = 0.02, label = day_label),
          hjust = 0, size = 3, family = "Oswald SemiBold", na.rm = TRUE
        )
      x_positions <- c(0.02, 0.26, 0.54, 0.84)
      headers <- c("Date", "Visiting Team", "Home Team", "Time")
    } else {
      x_positions <- c(0.16, 0.44, 0.74)
      headers <- c("Visiting Team", "Home Team", "Time")
    }

    if (is.null(day)) {
      p <- p +
        geom_image(aes(x = 0.20, image = VISITING_LOGO),
          size = 0.025, asp = 1.5
        ) +
        geom_text(aes(x = 0.26, label = opponent),
          hjust = 0, size = 3.2, family = "Oswald SemiBold"
        ) +
        geom_image(aes(x = 0.48, image = HOME_LOGO), size = 0.025, asp = 1.5) +
        geom_text(aes(x = 0.54, label = team),
          hjust = 0, size = 3.2, family = "Oswald SemiBold"
        ) +
        geom_text(aes(x = 0.84, label = time),
          hjust = 1, size = 2.8, family = "Oswald SemiBold"
        )
    } else {
      p <- p +
        geom_image(aes(x = 0.10, image = VISITING_LOGO),
          size = 0.04, asp = 1.5
        ) +
        geom_text(aes(x = 0.16, label = opponent),
          hjust = 0, size = 3.2, family = "Oswald SemiBold"
        ) +
        geom_image(aes(x = 0.38, image = HOME_LOGO), size = 0.04, asp = 1.5) +
        geom_text(aes(x = 0.44, label = team),
          hjust = 0, size = 3.2, family = "Oswald SemiBold"
        ) +
        geom_text(aes(x = 0.74, label = time),
          hjust = 1, size = 2.8, family = "Oswald SemiBold"
        )
    }


    for (i in seq_along(headers)) {
      p <- p + annotate("text",
        x = x_positions[i], y = -0.5, label = headers[i],
        hjust = ifelse(i == length(headers), 1, 0),
        size = 3, family = "Oswald SemiBold"
      )
    }

    p <- p + annotate("segment",
      x = 0, xend = 0.85, y = 0.5, yend = 0.5,
      color = "gray40", linewidth = 0.5, linetype = "solid"
    )

    p <- p + theme(
      plot.background = element_rect(
        color = "gray60",
        fill = "white",
        linewidth = 1
      )
    )

    return(p)
  }

  # Create and save the plot
  schedule_plot <- create_schedule_plot(nhl_schedule)

  plot_height <- if (is.null(day)) 10 else max(3, 3 + 0.5 * nrow(nhl_schedule))

  ggsave(
    paste0(
      "nhl_schedule_week_", week_num,
      if (!is.null(day)) paste0("_", tolower(day)), ".png"
    ),
    plot = schedule_plot, width = 6,
    height = plot_height, dpi = 600
  )

  return(schedule_plot)
}

schedule_data <- readRDS("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/data/nhl_schedule.RDS")
team_info <- readRDS("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/data/nhl_team_info.RDS")
source("/Users/danilooak/Documents/Code/R_Code/Sports_Analytics/NHL/src/theme_danilo.r")

# For the entire week
# full_week_plot <- create_nhl_schedule(schedule_data, team_info, week_num = 2)

# For a specific day (e.g., Monday)
monday_plot <- create_nhl_schedule(schedule_data,
  team_info,
  week_num = 2, day = "Tue"
)
