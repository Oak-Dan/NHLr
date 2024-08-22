# Configuration
CONFIG <- list(
  time_escolhido = "STL",
  data_inicio = "2024-10-01",
  data_fim = "2024-11-01",
  output_path = "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/travel_distance.png",
  schedule_path = "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/nhl_schedule.RDS"
)

# Package loading
pacman::p_load(tidyverse, ggplot2, maps, geosphere, cowplot, rnaturalearth, sf, rsvg, ggimage, httr, extrafont)

# Load fonts
loadfonts(quiet = TRUE)

# Function to download and convert SVG to PNG
baixar_e_converter_svg <- function(url, png_path, width = 1000, height = 1000) {
  tryCatch(
    {
      temp_svg <- tempfile(fileext = ".svg")
      GET(url, write_disk(temp_svg, overwrite = TRUE))
      rsvg_png(temp_svg, png_path, width = width, height = height)
      unlink(temp_svg)
      return(png_path)
    },
    error = function(e) {
      warning(paste("Error processing", url, ":", e$message))
      return(NA)
    }
  )
}

# Custom theme function
theme_danilo <- function() {
  theme_minimal(base_size = 9, base_family = "Oswald SemiBold") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(
        fill = "floralwhite",
        color = "floralwhite"
      )
    )
}

# Function to calculate distance between two points
calcular_distancia <- function(lat1, lon1, lat2, lon2) {
  raio <- 6371 # Earth's radius in km

  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180

  dlat <- lat2 - lat1
  dlon <- lon2 - lon1

  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  raio * c
}

# Function to create arc for travel paths
criar_arco <- function(start, end, n = 100, height = 0.35) {
  start_rad <- start * pi / 180
  end_rad <- end * pi / 180

  d <- acos(sin(start_rad[2]) * sin(end_rad[2]) +
    cos(start_rad[2]) * cos(end_rad[2]) *
      cos(abs(end_rad[1] - start_rad[1])))

  theta <- seq(0, pi, length.out = n)

  arc_points <- sapply(theta, function(t) {
    A <- sin((1 - t / pi) * d) / sin(d)
    B <- sin(t / pi * d) / sin(d)
    x <- A * cos(start_rad[2]) * cos(start_rad[1]) +
      B * cos(end_rad[2]) * cos(end_rad[1])
    y <- A * cos(start_rad[2]) * sin(start_rad[1]) +
      B * cos(end_rad[2]) * sin(end_rad[1])
    z <- A * sin(start_rad[2]) + B * sin(end_rad[2])
    c(atan2(y, x) * 180 / pi, atan2(z, sqrt(x^2 + y^2)) * 180 / pi)
  })

  arc_points[2, ] <- arc_points[2, ] + height * sin(seq(0, pi, length.out = n))

  distancia <- calcular_distancia(start[2], start[1], end[2], end[1])

  list(
    arco = data.frame(lon = arc_points[1, ], lat = arc_points[2, ]),
    distancia = distancia
  )
}

# Main data processing function
process_nhl_data <- function(schedule, config) {
  jogos_do_time <- schedule %>%
    filter(
      gameDate >= as.Date(config$data_inicio),
      gameDate < as.Date(config$data_fim),
      (homeTeam.abbrev == config$time_escolhido |
        awayTeam.abbrev == config$time_escolhido)
    ) %>%
    rename(
      time_casa = homeTeam.abbrev,
      time_visitante = awayTeam.abbrev,
      data = gameDate
    ) %>%
    arrange(data) %>%
    mutate(
      origem = if_else(time_visitante == config$time_escolhido,
        config$time_escolhido, time_visitante
      ),
      destino = if_else(time_visitante == config$time_escolhido,
        time_casa, time_casa
      )
    ) %>%
    select(data, origem, destino, homeTeam_logo_light)

  cidades_nhl <- data.frame(
    nome = c(
      "Anaheim", "Utah", "Boston", "Buffalo", "Calgary",
      "Carolina", "Chicago", "Colorado", "Columbus", "Dallas",
      "Detroit", "Edmonton", "Florida", "Los Angeles", "Minnesota",
      "Montreal", "Nashville", "New Jersey", "New York Islanders",
      "New York Rangers", "Ottawa", "Philadelphia", "Pittsburgh",
      "San Jose", "Seattle", "St. Louis", "Tampa Bay", "Toronto",
      "Vancouver", "Vegas", "Washington", "Winnipeg"
    ),
    abreviados = c(
      "ANA", "UTA", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL",
      "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR",
      "OTT", "PHI", "PIT", "SJS", "SEA", "STL", "TBL", "TOR", "VAN", "VGK",
      "WSH", "WPG"
    ),
    lat = c(
      33.8008, 40.7608, 42.3663, 42.8758, 51.0375, 35.8032, 41.8807,
      39.7484, 39.9612, 32.7905, 42.3314, 53.5461, 26.1593, 34.0430,
      44.9448, 45.4957, 36.1627, 40.7334, 40.7228, 40.7505, 45.4215,
      39.9012, 40.4394, 37.3541, 47.6204, 38.6270, 27.9427, 43.6435,
      49.2778, 36.1699, 38.8981, 49.8951
    ),
    lon = c(
      -117.8778, -111.8910, -71.0622, -78.8784, -114.0519, -78.7218,
      -87.6742, -104.9885, -82.9988, -96.8104, -83.0458, -113.4938,
      -80.3256, -118.2673, -93.1009, -73.5683, -86.7816, -74.1710,
      -73.5900, -73.9934, -75.6972, -75.1720, -79.9962, -121.9018,
      -122.3491, -90.1994, -82.4454, -79.3791, -123.1216, -115.1398,
      -77.0365, -97.1384
    )
  )

  cidades_nhl <- cidades_nhl %>%
    left_join(
      schedule %>%
        select(homeTeam.abbrev, homeTeam_logo_light),
      by = c("abreviados" = "homeTeam.abbrev")
    ) %>%
    rename(logo_url = homeTeam_logo_light) %>%
    distinct()

  cidades_nhl$logo_png <- purrr::map_chr(
    seq_len(nrow(cidades_nhl)),
    ~ baixar_e_converter_svg(
      cidades_nhl$logo_url[.x],
      file.path(tempdir(), paste0("logo_", .x, ".png"))
    )
  )

  jogos_do_time <- jogos_do_time %>%
    mutate(
      jogo_em_casa = destino == config$time_escolhido,
      time_viajante = if_else(jogo_em_casa, origem, config$time_escolhido),
      proxima_origem = lead(origem)
    )

  cidades_envolvidas <- unique(c(jogos_do_time$origem, jogos_do_time$destino))
  cidades_filtradas <- cidades_nhl %>%
    filter(abreviados %in% cidades_envolvidas)

  list(
    jogos_do_time = jogos_do_time,
    cidades_nhl = cidades_nhl,
    cidades_filtradas = cidades_filtradas,
    cidades_envolvidas = cidades_envolvidas
  )
}

# Main visualization function
create_nhl_travel_map <- function(processed_data, config) {
  jogos_do_time <- processed_data$jogos_do_time
  cidades_nhl <- processed_data$cidades_nhl
  cidades_filtradas <- processed_data$cidades_filtradas
  cidades_envolvidas <- processed_data$cidades_envolvidas

  mapa_na <- map_data(map = "world", region = c("USA", "Canada"))
  estados_eua <- map_data("state")
  provincias_canada <- ne_states(country = "canada", returnclass = "sf")

  x_range <- range(cidades_nhl$lon)
  y_range <- range(cidades_nhl$lat)
  x_margin <- diff(x_range) * 0.10
  y_margin <- diff(y_range) * 0.10

  p <- ggplot() +
    geom_polygon(
      data = mapa_na, aes(x = long, y = lat, group = group),
      fill = "lightgray", color = "white"
    ) +
    geom_path(
      data = estados_eua, aes(x = long, y = lat, group = group),
      color = "white", linewidth = 0.2
    ) +
    geom_sf(
      data = provincias_canada, fill = NA, color = "white",
      linewidth = 0.2
    ) +
    coord_sf(
      xlim = c(x_range[1] - x_margin, x_range[2] + x_margin),
      ylim = c(y_range[1] - y_margin, y_range[2] + y_margin),
      expand = TRUE
    ) +
    theme_danilo() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  ultima_localizacao <- config$time_escolhido
  distancia_total <- 0

  for (i in seq_len(nrow(jogos_do_time))) {
    if (!jogos_do_time$jogo_em_casa[i]) {
      origem <- if (i == 1 || jogos_do_time$jogo_em_casa[i - 1]) {
        cidades_filtradas[
          cidades_filtradas$abreviados == ultima_localizacao,
          c("lon", "lat")
        ]
      } else {
        cidades_filtradas[
          cidades_filtradas$abreviados == jogos_do_time$destino[i - 1],
          c("lon", "lat")
        ]
      }

      destino <- cidades_filtradas[
        cidades_filtradas$abreviados == jogos_do_time$destino[i],
        c("lon", "lat")
      ]

      resultado_arco <- criar_arco(as.numeric(origem), as.numeric(destino),
        height = 0.8
      )

      p <- p + geom_path(
        data = resultado_arco$arco, aes(x = lon, y = lat),
        color = "blue", linewidth = 0.5, alpha = 0.3
      )

      distancia_total <- distancia_total + resultado_arco$distancia
      ultima_localizacao <- jogos_do_time$destino[i]
    } else {
      origem_visitante <- cidades_filtradas[
        cidades_filtradas$abreviados == jogos_do_time$origem[i],
        c("lon", "lat")
      ]
      destino_time_escolhido <- cidades_filtradas[
        cidades_filtradas$abreviados == config$time_escolhido,
        c("lon", "lat")
      ]

      resultado_arco_visitante <- criar_arco(as.numeric(origem_visitante),
        as.numeric(destino_time_escolhido),
        height = 0.8
      )

      p <- p + geom_path(
        data = resultado_arco_visitante$arco, aes(x = lon, y = lat),
        color = "green", linewidth = 0.5, alpha = 0.3
      )
    }

    if (!jogos_do_time$jogo_em_casa[i] && (i == nrow(jogos_do_time) || jogos_do_time$jogo_em_casa[i + 1])) { # nolint
      origem_volta <- cidades_filtradas[cidades_filtradas$abreviados == ultima_localizacao, c("lon", "lat")] # nolint
      destino_volta <- cidades_filtradas[cidades_filtradas$abreviados == config$time_escolhido, c("lon", "lat")] # nolint

      resultado_arco_volta <- criar_arco(as.numeric(origem_volta),
        as.numeric(destino_volta),
        height = 0.3
      )

      p <- p + geom_path(
        data = resultado_arco_volta$arco, aes(x = lon, y = lat),
        color = "red", linewidth = 0.5, alpha = 0.3
      )

      ultima_localizacao <- config$time_escolhido
    }
  }

  p <- p + geom_image(
    data = cidades_nhl %>% filter(abreviados %in% cidades_envolvidas),
    aes(x = lon, y = lat, image = logo_png),
    size = 0.065, asp = 1.5
  )

  data_inicio <- "2024-10-01"

  titulo <- paste(
    "Sequencia de viagem do", config$time_escolhido, "em ",
    format(as.Date(data_inicio), "%B"), " 2024\n",
    "Distancia total", format(round(distancia_total),
      big.mark = ".", decimal.mark = ","
    ), "km"
  )

  p <- p + labs(title = titulo) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

  ggdraw(p) + theme(plot.background = element_rect(
    fill = "#eee8d5",
    color = NA
  ))
}

# Main execution function
main <- function() {
  nhl_schedule <- readRDS(CONFIG$schedule_path)
  processed_data <- process_nhl_data(nhl_schedule, CONFIG)
  plot <- create_nhl_travel_map(processed_data, CONFIG)
  ggsave(CONFIG$output_path, plot, width = 6.5, height = 6.5, dpi = 300)
}

# Run the main function
main()
