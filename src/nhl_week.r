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

# Funcao para processar os jogos da semana (visao semanal)
processar_jogos_semana <- function(jogos, week_number, team_colors) {
  dias_semana_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")

  jogos %>%
    filter(week == week_number) %>% # data_inicio, gameDate <= data_fim) %>%
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
      cor_away = team_colors$team_secondary_color[match(
        awayTeam.abbrev,
        team_colors$team_abbr
      )],
      cor_home = team_colors$team_primary_color[match(
        homeTeam.abbrev,
        team_colors$team_abbr
      )],
    ) %>%
    select(
      id, gameDate, week, dia_semana, hora, jogo, logo_away, logo_home,
      cor_away, cor_home
    )
}

# Funcao para criar o grafico da programacao semanal
criar_programacao_semanal <- function(jogos_semana) {
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

  # Encontre a data de in<U+00ED>cio e fim da semana
  data_inicio <- min(as.Date(jogos_semana$gameDate), na.rm = TRUE)
  data_fim <- max(as.Date(jogos_semana$gameDate), na.rm = TRUE)

  if (is.finite(data_inicio) && is.finite(data_fim)) {
    meses_pt <- c(
      "janeiro", "fevereiro", "marco",
      "abril", "maio", "junho", "julho", "agosto",
      "setembro", "outubro", "novembro", "dezembro"
    )

    # Formatar a data de in<U+00ED>cio apenas com o dia
    data_inicio_pt <- format(data_inicio, "%d")

    # Formatar a data de fim com dia, m<U+00EA>s e ano
    data_fim_pt <- format(data_fim, "%d de %B de %Y")

    # Substituir o nome do mes em ingles pelo equivalente em portugues
    for (i in 1:12) {
      data_fim_pt <- sub(month.name[i], meses_pt[i],
        data_fim_pt,
        ignore.case = TRUE
      )
    }

    titulo <- paste(
      "Jogos da NHL: Semana", unique(jogos_semana$week),
      "-", data_inicio_pt, "ate", data_fim_pt
    )
  } else {
    titulo <- paste("Jogos da NHL: Semana", unique(jogos_semana$week))
  }

  # Crie um fator para os dias da semana na ordem correta
  dias_semana_ordem <- c("Dom", "Sab", "Sex", "Qui", "Qua", "Ter", "Seg")
  jogos_semana$dia_semana <- factor(jogos_semana$dia_semana,
    levels = dias_semana_ordem
  )

  ggplot(jogos_semana, aes(x = dia_semana, y = jogo_index)) +
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

# Main function
create_nhl_week <- function(week_number) {
  # Load data
  nhl_schedule <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/data/nhl_schedule.RDS") # nolint
  team_colors <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/data/nhl_team_info.RDS") # nolint

  # Visao semanal para todos os times
  if (!is.null(week_number)) {
    print(paste("Generating schedule for week:", week_number))
    jogos_semana <- processar_jogos_semana(
      nhl_schedule,
      week_number, team_colors
    )

    # Criar e salvar o grafico semanal
    programacao_semanal <- criar_programacao_semanal(jogos_semana)

    ggsave(paste0("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/NHL/imgs/Figures/NHL_week_", week_number, ".png"), # nolint
      programacao_semanal,
      width = 10, height = 8, dpi = 300
    )
  }
}

# Executar o script
# Pegar argumentos da linha de comando
args <- commandArgs(trailingOnly = TRUE)

# Verificar se pelo menos o team_abbrev foi fornecido
if (length(args) < 1) {
  stop("Usage: Rscript nhl_calendar.R [week_number]")
}

# Pegar team_abbrev
week_number <- args[1]

# Chamar a funcao main
create_nhl_week(week_number)
