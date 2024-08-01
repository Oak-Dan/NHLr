# Bibliotecas necessarias
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

# Configuracoess globais
TEAM_ABBREV <- "STL"
BACKGROUND_COLOR <- "#eee8d5"
HOME_COLOR <- "#00539B"
AWAY_COLOR <- "#FFFFFF"
TEXT_COLOR_HOME <- "white"
TEXT_COLOR_AWAY <- "black"
TEXT_COLOR_DEFAULT <- "black"

# Funcoes auxiliares
baixar_e_converter_svg <- function(url, png_path, width = 1000, height = 1000) {
  temp_svg <- tempfile(fileext = ".svg")
  GET(url, write_disk(temp_svg, overwrite = TRUE))
  rsvg_png(temp_svg, png_path, width = width, height = height)
  unlink(temp_svg)
  return(png_path)
}

theme_danilo <- function() {
  theme_minimal(base_size = 10, base_family = "Roboto Slab SemiBold") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(
        fill = BACKGROUND_COLOR,
        color = BACKGROUND_COLOR
      )
    )
}

# Funcao para processar os dados dos jogos (visao mensal)
processar_jogos <- function(jogos, team_abbrev) {
  jogos %>%
    mutate(
      data = as.Date(gameDate),
      adversario = if_else(homeTeam.abbrev == team_abbrev,
        awayTeam.placeName.default,
        homeTeam.placeName.default
      ),
      time_abreviado = if_else(homeTeam.abbrev == team_abbrev,
        awayTeam.abbrev,
        homeTeam.abbrev
      ),
      logo_url = if_else(homeTeam.abbrev == team_abbrev,
        awayTeam.logo,
        homeTeam.logo
      ),
      horario = sub("^.*T([0-9]{2}:[0-9]{2}).*$", "\\1", startTimeBR_v2),
      local = if_else(homeTeam.abbrev == team_abbrev, "Casa", "Fora")
    ) %>%
    select(id, data, adversario, time_abreviado, logo_url, horario, local)
}

# Funcao para preparar os dados do calendario (visao mensal)
preparar_dados_calendario <- function(ano, mes, jogos) {
  inicio_mes <- as.Date(paste(ano, mes, "01", sep = "-"))
  fim_mes <- ceiling_date(inicio_mes, "month") - days(1)
  todos_dias <- seq(inicio_mes, fim_mes, by = "day")

  dias_semana_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")

  calendario <- data.frame(
    data = todos_dias,
    dia = day(todos_dias),
    dia_semana = factor(dias_semana_pt[wday(todos_dias, week_start = 1)],
      levels = dias_semana_pt
    )
  ) %>%
    left_join(jogos, by = "data") %>%
    mutate(
      semana = as.numeric(format(data, "%W")) -
        as.numeric(format(as.Date(paste(ano, mes, "01", sep = "-")), "%W")) + 1,
      cor_fundo = case_when(
        local == "Casa" ~ HOME_COLOR,
        local == "Fora" ~ AWAY_COLOR,
        TRUE ~ BACKGROUND_COLOR
      ),
      cor_texto = case_when(
        local == "Casa" ~ TEXT_COLOR_HOME,
        local == "Fora" ~ TEXT_COLOR_AWAY,
        TRUE ~ TEXT_COLOR_DEFAULT
      )
    )

  return(calendario)
}

# Funcao principal para criar o calendario mensal
criar_calendario_jogos <- function(ano, mes, jogos) {
  calendario <- preparar_dados_calendario(ano, mes, jogos)

  meses_pt <- c(
    "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
    "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
  )
  nome_mes <- meses_pt[mes]

  ggplot(calendario, aes(x = dia_semana, y = semana)) +
    geom_tile(aes(fill = cor_fundo), color = "black", linewidth = 0.5) +
    scale_fill_identity() +
    geom_text(
      aes(label = dia, color = cor_texto),
      size = 3, vjust = 1,
      nudge_x = -0.40, nudge_y = 0.45, family = "Roboto Slab SemiBold"
    ) +
    geom_image(
      data = subset(calendario, !is.na(logo_png)),
      aes(image = logo_png), size = 0.11, nudge_y = 0.2
    ) +
    geom_text(
      data = subset(calendario, !is.na(time_abreviado)),
      aes(label = time_abreviado, color = cor_texto), size = 3.5,
      vjust = 1.2, nudge_y = -0.15,
      family = "Roboto Slab SemiBold"
    ) +
    geom_text(
      data = subset(calendario, !is.na(horario)),
      aes(label = horario, color = cor_texto), size = 2.5,
      vjust = 1.2, nudge_y = -0.3,
      family = "Roboto Slab SemiBold"
    ) +
    scale_color_identity() +
    scale_y_reverse() +
    coord_fixed() +
    labs(
      title = paste0(
        "Calendario de jogos\n",
        TEAM_ABBREV, " em ", nome_mes, " ", ano
      ),
      x = NULL, y = NULL
    ) +
    theme_danilo() +
    theme(
      legend.position = "none",
      plot.title = element_text(
        hjust = 0.5, face = "bold",
        family = "Roboto Slab SemiBold",
        size = 12
      ),
      axis.text.x = element_text(
        face = "bold",
        family = "Roboto Slab SemiBold"
      ),
      axis.text.y = element_blank()
    )
}

# Funcao para processar os jogos da semana (visao semanal)
processar_jogos_semana <- function(jogos, data_inicio, team_colors) {
  data_inicio <- data_inicio - days(wday(data_inicio) - 2)
  data_fim <- data_inicio + days(6)


  dias_semana_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")

  jogos %>%
    filter(gameDate >= data_inicio, gameDate <= data_fim) %>%
    mutate(
      dia_semana = factor(dias_semana_pt[wday(gameDate, week_start = 1)],
        levels = dias_semana_pt
      ),
      hora = sub("^.*T([0-9]{2}:[0-9]{2}).*$", "\\1", startTimeBR_v2),
      jogo = paste(awayTeam.abbrev, "at", homeTeam.abbrev),
      logo_away = awayTeam.logo,
      logo_home = homeTeam.logo,
      cor_away = team_colors$cor_secundaria[match(awayTeam.abbrev, team_colors$nome_abreviado)],
      cor_home = team_colors$cor_primaria[match(homeTeam.abbrev, team_colors$nome_abreviado)],
    ) %>%
    select(
      id, gameDate, dia_semana, hora, jogo, logo_away, logo_home,
      cor_away, cor_home
    )
}

# Funcao para criar o grafico da programacao semanal
criar_programacao_semanal <- function(jogos_semana) {
  # Certifique-se de que gameDate esta no formato de data
  jogos_semana$gameDate <- as.Date(jogos_semana$gameDate)

  # Converta a hora para formato POSIXct para ordenado
  jogos_semana$hora_ordenada <- as.POSIXct(
    paste(
      jogos_semana$gameDate,
      jogos_semana$hora
    ),
    format = "%Y-%m-%d %H:%M"
  )

  # Ordene os jogos por dia da semana e hora
  jogos_semana <- jogos_semana %>%
    arrange(dia_semana, hora_ordenada) %>%
    group_by(dia_semana) %>%
    mutate(jogo_index = row_number()) %>%
    ungroup()

  # Encontre a data de inicio e fim da semana
  data_inicio <- min(jogos_semana$gameDate)
  data_fim <- max(jogos_semana$gameDate)

  # Crie um fator para os dias da semana na ordem correta
  dias_semana_ordem <- c("Dom", "Sab", "Sex", "Qui", "Qua", "Ter", "Seg")
  jogos_semana$dia_semana <- factor(jogos_semana$dia_semana,
    levels = dias_semana_ordem
  )

  ggplot(jogos_semana, aes(x = dia_semana, y = jogo_index)) +
    geom_tile(aes(width = 0.85, height = 0.65),
      fill = "#eee8d5",
      color = "lightgray", linewidth = 0.20
    ) +
    geom_rect(
      aes(
        xmin = as.numeric(dia_semana) - 0.42,
        xmax = as.numeric(dia_semana) - 0.035,
        ymin = jogo_index - 0.32,
        ymax = jogo_index + 0.32,
        fill = cor_away
      ),
      color = NA
    ) +
    geom_rect(
      aes(
        xmin = as.numeric(dia_semana) + 0.035,
        xmax = as.numeric(dia_semana) + 0.42,
        ymin = jogo_index - 0.32,
        ymax = jogo_index + 0.32,
        fill = cor_home
      ),
      color = NA
    ) +
    geom_text(aes(label = hora),
      size = 2.5, vjust = 0.5,
      family = "Roboto Slab SemiBold"
    ) +
    geom_image(aes(image = logo_away), size = 0.045, nudge_x = -0.22) +
    geom_image(aes(image = logo_home), size = 0.045, nudge_x = 0.22) +
    scale_fill_identity() +
    scale_x_discrete(expand = c(0.01, 0.01)) +
    scale_y_continuous(
      breaks = NULL,
      expand = c(0.01, 0.01)
    ) +
    labs(
      title = paste(
        "NHL Games:", format(data_inicio, "%B %d"),
        "ate", format(data_fim, "%B %d, %Y")
      ),
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
    ) +
    coord_flip()
}

# Funcao principal
main <- function() {
  # Carregar dados
  nhl_schedule <- readRDS("C:/Users/danilo.carvalho/Documents/Python Scripts/nhl_schedule.RDS")
  team_colors <- readRDS("C:/Users/danilo.carvalho/Documents/Python Scripts/nhl_teamcolors.RDS")


  # Visualizacao mensal para um time especifico
  #nhl_schedule_mes_stl <- nhl_schedule %>%
  #  filter(
  #    (homeTeam.abbrev == TEAM_ABBREV | awayTeam.abbrev == TEAM_ABBREV),
  #    gameDate >= as.Date("2024-10-01"),
  #    gameDate < as.Date("2024-11-01")
  #  ) %>%
  #  processar_jogos(TEAM_ABBREV)

  # Baixar e converter logos para visao mensal
  #nhl_schedule_mes_stl$logo_png <- sapply(
  #  seq_len(nrow(nhl_schedule_mes_stl)),
  #  function(i) {
  #    png_path <- file.path(tempdir(), paste0("logo_", i, ".png"))
  #    baixar_e_converter_svg(nhl_schedule_mes_stl$logo_url[i], png_path)
  #  }
  #)

  # Criar e salvar o calendario mensal
  #calendario <- criar_calendario_jogos(2024, 10, nhl_schedule_mes_stl)
  #plot_schedule <- ggdraw(calendario) +
  #  theme(plot.background = element_rect(fill = BACKGROUND_COLOR, color = NA))

  #ggsave("C:/Users/danilo.carvalho/Documents/Python Scripts/calendario_mensal.png",
  #  plot_schedule,
  #  width = 6, height = 6, dpi = 300
  #)

  # Visao semanal para todos os times
  data_inicio_semana <- as.Date("2024-10-24")
  jogos_semana <- processar_jogos_semana(nhl_schedule, data_inicio_semana, team_colors)

  # Criar e salvar o grafico semanal
  programacao_semanal <- criar_programacao_semanal(jogos_semana)

  ggsave("C:/Users/danilo.carvalho/Documents/Python Scripts/programacao_semanal.png",
    programacao_semanal,
    width = 10, height = 8, dpi = 300
  )
}

# Executar o script
main()
