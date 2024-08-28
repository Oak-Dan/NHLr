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

# font_import()
# loadfonts()

# Configuracoess globais
BACKGROUND_COLOR <- "floralwhite" # nolint
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
      local = case_when(
        neutralSite == TRUE ~ "Special",
        homeTeam.abbrev == team_abbrev ~ "Casa",
        TRUE ~ "Fora"
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
        local == "Special" ~ "white",
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
  nome_mes <- meses_pt[as.numeric(mes)]

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
      nudge_x = -0.40, nudge_y = 0.45, family = "Oswald SemiBold"
    ) +
    geom_image(
      data = subset(calendario, !is.na(logo_png)),
      aes(image = logo_png), size = 0.12, nudge_y = 0.2
    ) +
    geom_text(
      data = subset(calendario, !is.na(time_abreviado)),
      aes(label = time_abreviado, color = cor_texto), size = 3.5,
      vjust = 1.2, nudge_y = -0.09,
      family = "Oswald SemiBold"
    ) +
    geom_text(
      data = subset(calendario, !is.na(horario)),
      aes(label = horario, color = cor_texto), size = 2.5,
      vjust = 1.2, nudge_y = -0.3,
      family = "Oswald SemiBold"
    ) +
    scale_color_identity() +
    scale_y_reverse() +
    coord_fixed() +
    labs(
      title = toupper(nome_mes),
      caption = "Todas as datas e horarios estao no Horario de Brasilia (BRT, UTC-3) e estao sujeitos a alteracoes.", # nolint
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
        size = 8, hjust = 0.02,
        vjust = -5, color = "black"
      ),
      axis.text.x = element_text(
        face = "bold",
        family = "Oswald SemiBold",
        color = "black"
      ),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
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
      hjust = -0.5, nudge_x = 0.01, size = 2.5, family = "Oswald SemiBold"
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    xlim(0.5, 4.5) +
    ylim(0.5, 1.5) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))


  # Save our inset plot
  ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/p_legend.png", # nolint
    p_legend,
    w = 3.85, h = 1.25, dpi = 300
  )

  set_null_device("png")

  ggdraw(p_calendar) +
    draw_image(img, x = 0.40, y = 0.42, scale = .25) + # inserindo logo
    theme(plot.background = element_rect(fill = BACKGROUND_COLOR, color = NA))
}

# Main function
main <- function(team_abbrev, ano, mes) {
  # Load data
  nhl_schedule <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/data/nhl_schedule.RDS") # nolint
  team_colors <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/data/nhl_team_info.RDS") # nolint

  # Get team colors
  team_color <- team_colors %>%
    filter(team_abbr == team_abbrev) %>%
    select(
      team_primary_color, team_secondary_color,
      team_text_color_home, team_text_color_away
    )

  # Set colors
  HOME_COLOR <- team_color$team_primary_color
  AWAY_COLOR <- team_color$team_secondary_color
  TEXT_COLOR_HOME <- team_color$team_text_color_home
  TEXT_COLOR_AWAY <- team_color$team_text_color_away

  # converter mes para numerico
  mes <- as.numeric(mes)

  # Adicione esta logica para o calendario mensal
  if (!is.null(ano) && !is.null(mes)) {
    nhl_schedule_mes <- nhl_schedule %>%
      filter(
        (homeTeam.abbrev == team_abbrev | awayTeam.abbrev == team_abbrev),
        year(gameDate) == ano,
        month(gameDate) == mes
      ) %>%
      processar_jogos(team_abbrev)

    # Baixar e converter logos para visao mensal
    nhl_schedule_mes$logo_png <- sapply(
      seq_len(nrow(nhl_schedule_mes)),
      function(i) {
        png_path <- file.path(tempdir(), paste0("logo_", i, ".png"))
        baixar_e_converter_svg(nhl_schedule_mes$logo_url[i], png_path)
      }
    )

    team_logo_abbrev_url <- paste0(
      "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Logos/Logos Light/", # nolint
      team_abbrev, "_light.png"
    )

    # Criar e salvar o calendario mensal
    calendario <- criar_calendario_jogos(
      ano, mes,
      nhl_schedule_mes, team_logo_abbrev_url, team_abbrev,
      HOME_COLOR, AWAY_COLOR, TEXT_COLOR_HOME, TEXT_COLOR_AWAY
    )
    plot_schedule <- ggdraw(calendario) +
      theme(plot.background = element_rect(fill = BACKGROUND_COLOR, color = NA))

    ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/calendario_mensal.png", # nolint
      plot_schedule,
      width = 6.5, height = 6.5, dpi = 300
    )
  }


  # Read in Inset plot
  inset <- image_read("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/p_legend.png") # nolint

  # Read in Comet plot
  graf <- image_read("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/calendario_mensal.png") # nolint

  # Juntar imagens
  image_composite(graf, inset, offset = "+440+50") %>%
    image_write("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/calendario_mensal.png") # nolint
}

# Executar o script
# Pegar argumentos da linha de comando
args <- commandArgs(trailingOnly = TRUE)

# Verificar se pelo menos o team_abbrev foi fornecido
if (length(args) < 1) {
  stop("Usage: Rscript nhl_calendar.R [team_abbrev] [ano] [mes]")
}

# Pegar team_abbrev
team_abbrev <- args[1]

# Pegar ano e mes se fornecidos
ano <- args[2]
mes <- args[3]

# Chamar a funcao main
main(team_abbrev, ano, mes)
