# Load required libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggimage)
library(cowplot)
library(extrafont)
library(ggchicklet)
library(grid)
library(nflreadr)
library(webshot2)
library(gt)
library(gtExtras)



# Load data
nhl_schedule <- readRDS("C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/nhl_schedule.RDS")
team_colors <- readRDS("C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/nhl_team_info.RDS")

# Define constants
BACKGROUND_COLOR <- "floralwhite"
TEXT_COLOR_DEFAULT <- "black"

# Source the external script
#source("")

# Process games for the week
process_games <- function(games, week_num, colors) {
  dias_semana_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")
  
  games %>%
    filter(week == week_num) %>%
    mutate(
      dia_semana = factor(dias_semana_pt[wday(gameDate, week_start = 1)],
                          levels = dias_semana_pt
      ),
      hora = sub("^.*T([0-9]{2}:[0-9]{2}).*$", "//1", startTimeBR_v2),
      time_mandante =  homeTeam.abbrev,
      time_visitante = awayTeam.abbrev,
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
      id, gameDate, week, dia_semana, hora, jogo, time_mandante,
      time_visitante, logo_away, logo_home, cor_away, cor_home
    )
}

df <- process_games(nhl_schedule, 1, team_colors)
home_teams <- sort(unique(df$time_mandante))
dias_semanas <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")

final_df <- data.frame(Time_Casa = character(length(home_teams)),
                       Seg = character(length(home_teams)),
                       Ter = character(length(home_teams)),
                       Qua = character(length(home_teams)),
                       Qui = character(length(home_teams)),
                       Sex = character(length(home_teams)),
                       Sab = character(length(home_teams)),
                       Dom = character(length(home_teams)))

final_df$Time_Casa <- home_teams


# Preenchendo as colunas de dias da semana
for (dia in dias_semanas) {
  # Filtra o DataFrame de jogos para o dia específico
  jogos_dia <- df |> filter(dia_semana == dia)
  
  # Para cada time da casa, encontre os adversários e preencha a coluna
  for (i in seq_along(home_teams)) {
    # Encontrar os jogos onde o time da casa é o time_mandante
    adversarios <- jogos_dia |> 
      filter(time_mandante == home_teams[i]) |> 
      pull(time_visitante)
    
    # Verificar se há adversários e preencher a coluna correspondente
    if (length(adversarios) > 0) {
      final_df[i, dia] <- adversarios
    } else {
      final_df[i, dia] <- NA  # Ou uma string vazia se preferir
    }
  }
}

dias_semana_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")
nhl_schedule2 <- nhl_schedule |>
  filter(week == 13) |> 
  mutate(start_date = min(as.Date(gameDate), na.rm = TRUE),
         end_date = max(as.Date(gameDate), na.rm = TRUE),
         dia_semana = dias_semana_pt[wday(gameDate, week_start = 1)]) |>  
  select(homeTeam.placeName.default, awayTeam.placeName.default, gameDate, week, start_date, end_date, dia_semana) |> 
  rename(home_team = homeTeam.placeName.default, away_team = awayTeam.placeName.default, date = gameDate)
  

#schedule2 <- nhl_schedule2 %>% 
#  select(home_team, away_team, week) %>% 
#  clean_homeaway()


plot_data <- nhl_schedule2 %>% 
  select(home_team, away_team, dia_semana, week) %>%
  rename(team = home_team, opponent = away_team) |> 
  mutate(location = "home")
  #nflreadr::clean_homeaway()

logos <- nhl_schedule |>
  arrange(awayTeam.placeName.default) |> 
  select(awayTeam.placeName.default, awayTeam_logo_light) |> 
  distinct_all() |> 
  mutate(team = awayTeam.placeName.default, logo = awayTeam_logo_light) |> 
  select(team, logo)

logos <- logos |> pull(logo) |> set_names(logos$team)

plot_data <- plot_data %>% 
  mutate(opponent = glue::glue("<img src='{logos[opponent]}' alt={location} style='height:25px; vertical-align:middle;'>"))


plot_data <- plot_data %>% 
  pivot_wider(id_cols = team, names_from = dia_semana, values_from = opponent) %>% 
  arrange(team) %>% 
  mutate(team = logos[team])

generate_css <- function(indices, css_id, color) {
  map2_chr(
    .x = indices[, 1],
    .y = indices[, 2],
    .f = ~glue::glue("#{css_id} tbody tr:nth-child({.x}) td:nth-child({.y}) {{ background-color: {color}; }}")
  )
}

home_css <- arrayInd(which(str_detect(as.matrix(plot_data), 'alt=home')), .dim = dim(plot_data)) %>% 
  generate_css('table', '#cce7f5')

bye_css <- arrayInd(which(is.na(as.matrix(plot_data))), .dim = dim(plot_data)) %>% 
  generate_css('table', '#d9d9d9')

additional_css <- "
  
  #table .gt_sourcenote {
    line-height: 1.3;
  }

"

html_content <- '
<div style="text-align: center;">
  <h1 style="margin: 0; font-size: 20px;">NHL Week Schedule | 2024/2025</h1>
  <div style="display: flex; justify-content: center; align-items: center; margin-top: 5px;">
    <div style="border: 1.5px solid black; padding: 2px 10px; text-align: center; background-color: #cce7f5; font-size: 10px; margin-right: 5px;">Home</div>
    <div style="border: 1.5px solid black; padding: 2px 10px; text-align: center; font-size: 10px; margin-right: 5px;">Away</div>
    <div style="border: 1.5px solid black; padding: 2px 10px; text-align: center; background-color: #d9d9d9; font-size: 10px;">Bye</div>
  </div>
</div>
'
setwd("C:/Users/danilo.carvalho/Documents/Python Scripts/Outros/")
plot_data %>% 
  gt(id = 'table') %>% 
  gt_theme_538() %>% 
  fmt_image(team, height = 25) %>%
  fmt_markdown(-team) %>% 
  # use sub_missing to replace na with empty text string
  sub_missing(-team, missing_text = '') %>% 
  cols_align(columns = everything(), 'center') %>% 
  cols_label(team = '') %>% 
  # bold col. headers
  tab_style(locations = cells_column_labels(), style = cell_text(weight = 'bold')) %>% 
  # add dividers
  gt_add_divider(columns = -team, sides = 'all', include_labels = FALSE, color = 'black', weight = px(1.5)) %>% 
  tab_header(html(html_content)) %>% 
  tab_source_note(md("Data by cfbfastR<br>Viz. by @andreweatherman (h/t to @cobrastats)")) %>% 
  tab_options(data_row.padding = 1) %>% 
  # apply above css
  opt_css(c(home_css, bye_css, additional_css)) %>% 
  gtsave_extra("schedule2.png", zoom = 10)
