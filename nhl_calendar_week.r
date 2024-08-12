# Bibliotecas necessarias. Suprimir mensagens
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
})

# Configuracoess globais
BACKGROUND_COLOR <- "#eee8d5" # nolint
TEXT_COLOR_DEFAULT <- "black" # nolint


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
        awayTeam_logo_light,
        homeTeam_logo_light
      ),
      horario = sub(
        "^.*T([0-9]{2}:[0-9]{2}).*$", "\\1",
        startTimeBR_v2
      ),
      local = if_else(homeTeam.abbrev == team_abbrev,
        "Casa",
        "Fora"
      )
    ) %>%
    select(
      id, data, adversario, time_abreviado,
      logo_url,
      horario,
      local
    )
}

# Funcao para preparar os dados do calendario (visao mensal)
preparar_dados_calendario <- function(
    ano, mes, jogos,
    home_color, away_color, text_color_home,
    text_color_away) {
  inicio_mes <- as.Date(paste(ano,
    mes,
    "01",
    sep = "-"
  ))
  fim_mes <- ceiling_date(
    inicio_mes,
    "month"
  ) - days(1)
  todos_dias <- seq(inicio_mes,
    fim_mes,
    by = "day"
  )

  dias_semana_pt <- c(
    "Seg", "Ter",
    "Qua", "Qui",
    "Sex", "Sab",
    "Dom"
  )

  calendario <- data.frame(
    data = todos_dias,
    dia = day(todos_dias),
    dia_semana = factor(
      dias_semana_pt[wday(todos_dias,
        week_start = 1
      )],
      levels = dias_semana_pt
    )
  ) %>%
    left_join(jogos, by = "data") %>%
    mutate(
      semana = as.numeric(format(data, "%W")) -
        as.numeric(format(
          as.Date(paste(ano, mes,
            "01",
            sep = "-"
          )),
          "%W"
        )) + 1,
      cor_fundo = case_when(
        local == "Casa" ~ home_color,
        local == "Fora" ~ away_color,
        TRUE ~ BACKGROUND_COLOR
      ),
      cor_texto = case_when(
        local == "Casa" ~ text_color_home,
        local == "Fora" ~ text_color_away,
        TRUE ~ TEXT_COLOR_DEFAULT
      )
    )

  return(calendario)
}

# Funcao principal para criar o calendario mensal
criar_calendario_jogos <- function(
    ano, mes, jogos, logo_png_team,
    team_abbrev, home_color, away_color, text_color_home,
    text_color_away) {
  calendario <- preparar_dados_calendario(
    ano, mes, jogos,
    home_color, away_color, text_color_home,
    text_color_away
  )

  meses_pt <- c(
    "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
    "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
  )
  nome_mes <- meses_pt[mes]

  img <- logo_png_team

  # Create a data frame for the legend
  legend_data <- data.frame(
    x = c(2, 2.8),
    y = 1,
    label = c("Casa", "Fora"),
    color = c(home_color, away_color),
    text_color = c(text_color_home, text_color_away)
  )


  p_calendar <- ggplot(calendario, aes(x = dia_semana, y = semana)) +
    geom_tile(aes(fill = cor_fundo), color = "black", linewidth = 0.5) +
    scale_fill_identity() +
    geom_text(
      aes(label = dia, color = cor_texto),
      size = 3, vjust = 1,
      nudge_x = -0.40, nudge_y = 0.45, family = "Roboto Slab SemiBold"
    ) +
    geom_image(
      data = subset(calendario, !is.na(logo_png)),
      aes(image = logo_png), size = 0.125, nudge_y = 0.2
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
      title = paste(
        "OUTUBRO"
      ),
      x = NULL,
      y = NULL
    ) +
    theme_danilo() +
    theme(
      legend.position = "none",
      plot.title = element_text(
        hjust = 0.5, face = "bold",
        vjust = 10,
        family = "Roboto Slab SemiBold",
        size = 12
      ),
      axis.text.x = element_text(
        face = "bold",
        family = "Roboto Slab SemiBold"
      ),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(t = 0, r = 10, b = 0, l = 10, unit = "pt")
    )

  # Create the legend plot
  p_legend <- ggplot(legend_data, aes(x = x, y = y)) +
    ggchicklet:::geom_rrect(
      aes(
        xmin = 1.75, xmax = 2.05, ymin = 0.9,
        ymax = 1.1,
        fill = home_color
      ),
      r = unit(0.15, "npc"),
      color = "black"
    ) +
    ggchicklet:::geom_rrect(
      aes(
        xmin = 2.55, xmax = 2.85, ymin = 0.9,
        ymax = 1.1,
        fill = away_color
      ),
      r = unit(0.15, "npc"),
      color = "black"
    ) +
    geom_text(aes(label = label, color = "black"),
      hjust = -0.5, nudge_x = 0.01, size = 2.5, family = "Roboto Slab SemiBold"
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    xlim(0.5, 4.5) +
    ylim(0.5, 1.5) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))


  # Save our inset plot
  ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/p_legend.png", # nolint
    p_legend,
    w = 3.85, h = 1.25, dpi = 300
  )

  set_null_device("png")


  ggdraw(p_calendar) +
    draw_image(img, x = 0.40, y = 0.42, scale = .25) + # inserindo logo
    theme(plot.background = element_rect(fill = BACKGROUND_COLOR, color = NA))
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
      logo_away = awayTeam_logo_light,
      logo_home = case_when(
        homeTeam.abbrev %in% c("TOR", "TBL") ~ gsub(
          "_light",
          "_dark", homeTeam_logo_light
        ),
        TRUE ~ homeTeam_logo_light
      ),
      cor_away = team_colors$cor_secundaria[match(
        awayTeam.abbrev,
        team_colors$nome_abreviado
      )],
      cor_home = team_colors$cor_primaria[match(
        homeTeam.abbrev,
        team_colors$nome_abreviado
      )],
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
    # geom_tile(aes(width = 0.85, height = 0.65),
    #  fill = "#eee8d5",
    #  color = "lightgray", linewidth = 0.20
    # ) +
    ggchicklet:::geom_rrect(
      aes(
        xmin = as.numeric(dia_semana) - 0.40,
        xmax = as.numeric(dia_semana) - 0.045,
        ymin = jogo_index - 0.30,
        ymax = jogo_index + 0.30,
        fill = cor_away
      ),
      r = unit(0.15, "npc"),
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
      r = unit(0.15, "npc"),
      color = NA
    ) +
    geom_text(aes(label = hora),
      size = 2.5, vjust = 0.5,
      family = "Roboto Slab SemiBold"
    ) +
    geom_image(aes(image = logo_away), size = 0.045, nudge_x = -0.225) +
    geom_image(aes(image = logo_home), size = 0.045, nudge_x = 0.225) +
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
main <- function(team_abbrev) {
  # Carregar dados
  nhl_schedule <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/nhl_schedule.RDS") # nolint
  team_colors <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/nhl_teamcolors.RDS") # nolint

  team_color <- team_colors %>%
    filter(nome_abreviado == team_abbrev) %>%
    select(
      cor_primaria, cor_secundaria,
      text_color_home, text_color_away
    )

  HOME_COLOR <- team_color$cor_primaria
  AWAY_COLOR <- team_color$cor_secundaria
  TEXT_COLOR_HOME <- team_color$text_color_home
  TEXT_COLOR_AWAY <- team_color$text_color_away

  # Visualizacao mensal para um time especifico
  nhl_schedule_mes_team <- nhl_schedule %>%
    filter(
      (homeTeam.abbrev == team_abbrev | awayTeam.abbrev == team_abbrev),
      gameDate >= as.Date("2024-10-01"),
      gameDate < as.Date("2024-11-01")
    ) %>%
    processar_jogos(team_abbrev)

  # Baixar e converter logos para visao mensal
  nhl_schedule_mes_team$logo_png <- sapply(
    seq_len(nrow(nhl_schedule_mes_team)),
    function(i) {
      png_path <- file.path(tempdir(), paste0("logo_", i, ".png"))
      baixar_e_converter_svg(nhl_schedule_mes_team$logo_url[i], png_path)
    }
  )

  team_logo_abbrev_url <- paste0(
    "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/Logos/Logos Light/", # nolint
    team_abbrev,
    "_light.png"
  )
  # Criar e salvar o calendario mensal
  calendario <- criar_calendario_jogos(
    2024, 10,
    nhl_schedule_mes_team, team_logo_abbrev_url,
    team_abbrev, HOME_COLOR, AWAY_COLOR, TEXT_COLOR_HOME,
    TEXT_COLOR_AWAY
  )
  plot_schedule <- ggdraw(calendario) +
    theme(plot.background = element_rect(fill = BACKGROUND_COLOR, color = NA))

  ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/calendario_mensal.png", # nolint
    plot_schedule,
    width = 6, height = 6, dpi = 300
  )

  # Read in Inset plot
  inset <- image_read("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/p_legend.png") # nolint

  # Read in Comet plot
  graf <- image_read("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/calendario_mensal.png") # nolint

  # Juntar imagens
  image_composite(graf, inset, offset = "+355+50") %>%
    image_write("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/calendario_mensal.png") # nolint



  # Visao semanal para todos os times
  # data_inicio_semana <- as.Date("2024-11-22")
  # jogos_semana <- processar_jogos_semana(
  #  nhl_schedule,
  #  data_inicio_semana, team_colors
  # )

  # Criar e salvar o grafico semanal
  # programacao_semanal <- criar_programacao_semanal(jogos_semana)

  # ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/programacao_semanal.png", # nolint
  #       programacao_semanal,
  #       width = 10, height = 8, dpi = 300
  # )
}

# Executar o script
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Por favor, forne<U+00E7>a a abrevia<U+00E7><U+00E3>o do time como argumento.")
}
main(args[1])
