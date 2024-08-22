# Instalar e carregar os pacotes necessarios
library(tidyverse)
library(ggplot2)
library(maps)
library(geosphere)
library(cowplot)
library(rnaturalearth)
library(sf)
library(rsvg)
library(ggimage)
library(httr)
library(extrafont)

# font_import()
loadfonts()

# Funcao para baixar SVG e converter para PNG
baixar_e_converter_svg <- function(url, png_path, width = 500, height = 500) {
  temp_svg <- tempfile(fileext = ".svg")
  GET(url, write_disk(temp_svg, overwrite = TRUE))
  rsvg_png(temp_svg, png_path, width = width, height = height)
  unlink(temp_svg) # Remove o arquivo temporario
  return(png_path)
}

theme_danilo <- function() {
  theme_minimal(base_size = 9, base_family = "Roboto Slab SemiBold") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#eee8d5", color = "#eee8d5")
    )
}

# carregar programacao
nhl_schedule <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/nhl_schedule.RDS")
# nhl_schedule <- readRDS("C:/Users/danilo.carvalho/Documents/nhl_schedule.RDS")

# Filtrar jogos para um time especifico (por exemplo, "Toronto Maple Leafs")
time_escolhido <- "STL"
jogos_do_time <- nhl_schedule %>%
  filter(
    gameDate >= as.Date("2025-03-01"),
    gameDate < as.Date("2025-04-01"),
    (homeTeam.abbrev == time_escolhido |
      awayTeam.abbrev == time_escolhido)
  ) %>%
  rename(
    time_casa = homeTeam.abbrev,
    time_visitante = awayTeam.abbrev,
    data = gameDate
  ) %>%
  arrange(data) %>%
  mutate(
    origem = ifelse(time_visitante == time_escolhido,
      time_escolhido, time_visitante
    ),
    destino = ifelse(time_visitante == time_escolhido,
      time_casa, time_casa
    )
  ) %>%
  select(data, origem, destino, homeTeam.logo, awayTeam.logo)

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

cidades_nhl <- cidades_nhl |>
  left_join(
    nhl_schedule |>
      select(homeTeam.abbrev, homeTeam.logo),
    by = c("abreviados" = "homeTeam.abbrev")
  ) |>
  rename(logo_url = homeTeam.logo) |>
  distinct_all()

cidades_nhl$logo_png <- sapply(
  seq_len(nrow(cidades_nhl)),
  function(i) {
    png_path <- file.path(tempdir(), paste0("logo_", i, ".png"))
    baixar_e_converter_svg(cidades_nhl$logo_url[i], png_path)
  }
)

# Adicionar uma coluna para a proxima cidade e identificar jogos em casa/fora
jogos_do_time <- jogos_do_time %>%
  mutate(
    jogo_em_casa = destino == time_escolhido,
    time_viajante = ifelse(jogo_em_casa, origem, time_escolhido),
    proxima_origem = lead(origem)
  )

# Filtrar cidades envolvidas
cidades_envolvidas <- unique(c(jogos_do_time$origem, jogos_do_time$destino))
cidades_filtradas <- cidades_nhl %>%
  filter(abreviados %in% cidades_envolvidas)

# Funcao para calcular a distancia entre dois pontos (em km)
calcular_distancia <- function(lat1, lon1, lat2, lon2) {
  raio <- 6371 # Raio da Terra em km

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

# Obter o mapa da America do Norte (EUA e Canada)
mapa_na <- map_data(map = "world", region = c("USA", "Canada"))

# Obter os dados dos estados dos EUA
estados_eua <- map_data("state")

# Obter os dados das provincias do Canada usando rnaturalearth
provincias_canada <- ne_states(country = "canada", returnclass = "sf")

# Calcular os limites do mapa com base nas cidades
x_range <- range(cidades_nhl$lon)
y_range <- range(cidades_nhl$lat)

# Adicionar uma margem de 5% aos limites para melhor visualizacao
x_margin <- diff(x_range) * 0.10
y_margin <- diff(y_range) * 0.10

# Criar o grafico base com limites ajustados
p <- ggplot() +
  geom_polygon(
    data = mapa_na, aes(x = long, y = lat, group = group),
    fill = "lightgray", color = "white"
  ) +
  # Adicionar as linhas dos estados dos EUA
  geom_path(
    data = estados_eua, aes(x = long, y = lat, group = group),
    color = "white", linewidth = 0.2
  ) +
  # Adicionar as linhas das provincias do Canad<U+00E1>
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
# Adicionar pontos para as cidades

criar_arco <- function(start, end, n = 100, height = 0.35) {
  # Converter para radianos
  start_rad <- start * pi / 180
  end_rad <- end * pi / 180

  # Calcular a distancia great circle
  d <- acos(sin(start_rad[2]) * sin(end_rad[2]) +
    cos(start_rad[2]) * cos(end_rad[2]) *
      cos(abs(end_rad[1] - start_rad[1])))

  # Criar sequencia de angulos
  theta <- seq(0, pi, length.out = n)

  # Calcular pontos do arco
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

  # Ajustar a altura do arco
  arc_points[2, ] <- arc_points[2, ] + height * sin(seq(0, pi, length.out = n))

  distancia <- calcular_distancia(start[2], start[1], end[2], end[1])

  list(
    arco = data.frame(lon = arc_points[1, ], lat = arc_points[2, ]),
    distancia = distancia
  )
}

# Inicializar variaveis para controle de viagem
ultima_localizacao <- time_escolhido

distancia_total <- 0

# Adicionar arcos para as viagens e calcular a distancia total
for (i in seq_len(nrow(jogos_do_time))) {
  # Viagem do time escolhido (STL)
  if (!jogos_do_time$jogo_em_casa[i]) {
    # Determinar a origem correta para STL
    if (i == 1 || jogos_do_time$jogo_em_casa[i - 1]) {
      origem <- cidades_filtradas[
        cidades_filtradas$abreviados ==
          ultima_localizacao,
        c("lon", "lat")
      ]
    } else {
      origem <- cidades_filtradas[
        cidades_filtradas$abreviados ==
          jogos_do_time$destino[i - 1],
        c("lon", "lat")
      ]
    }

    destino <- cidades_filtradas[
      cidades_filtradas$abreviados ==
        jogos_do_time$destino[i],
      c("lon", "lat")
    ]

    resultado_arco <- criar_arco(as.numeric(origem),
      as.numeric(destino),
      height = 0.8
    )

    p <- p + geom_path(
      data = resultado_arco$arco, aes(x = lon, y = lat),
      color = "blue", linewidth = 0.5, alpha = 0.3
    )

    distancia_total <- distancia_total + resultado_arco$distancia
    ultima_localizacao <- jogos_do_time$destino[i]
  } else {
    # Viagem do time visitante para time escolhido
    origem_visitante <- cidades_filtradas[
      cidades_filtradas$abreviados ==
        jogos_do_time$origem[i],
      c("lon", "lat")
    ]
    destino_time_escolhido <- cidades_filtradas[
      cidades_filtradas$abreviados ==
        time_escolhido,
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

    # distancia_total <- distancia_total + resultado_arco_visitante$distancia
  }

  # Se for o Ultimo jogo ou o proximo jogo em casa,
  # adicionar viagem de volta para time escolhido
  if (!jogos_do_time$jogo_em_casa[i] && (i == nrow(jogos_do_time) ||
    jogos_do_time$jogo_em_casa[i + 1])) {
    origem_volta <- cidades_filtradas[
      cidades_filtradas$abreviados ==
        ultima_localizacao,
      c("lon", "lat")
    ]
    destino_volta <- cidades_filtradas[
      cidades_filtradas$abreviados ==
        time_escolhido,
      c("lon", "lat")
    ]

    resultado_arco_volta <- criar_arco(as.numeric(origem_volta),
      as.numeric(destino_volta),
      height = 0.3
    )

    p <- p + geom_path(
      data = resultado_arco_volta$arco, aes(x = lon, y = lat),
      color = "red", linewidth = 0.5, alpha = 0.3
    )

    # distancia_total <- distancia_total + resultado_arco_volta$distancia
    ultima_localizacao <- time_escolhido
  }
}

# adicionar pontos e rotulos para todas as cidades da nhl_schedule
p <- p +
  geom_image(
    data = cidades_nhl %>%
      filter(abreviados %in% cidades_envolvidas),
    aes(x = lon, y = lat, image = logo_png),
    size = 0.065, asp = 1.5
  )

# verifcas se a distancia e NA
if (is.na(distancia_total)) {
  titulo <- paste(
    "Distancia Total de Viagem para",
    time_escolhido, ":Erro no calculo"
  )
} else {
  titulo <- paste(
    "Distancia Total de Viagem para", time_escolhido, ".",
    format(round(distancia_total), big.mark = ".", decimal.mark = ","),
    "km"
  )
}

# Atualizar o titulo com a nova distancia total
titulo <- paste(
  "Sequencia de viagem do", time_escolhido, " em Novembro 2024\n",
  "Distancia total", format(round(distancia_total),
    big.mark = ".",
    decimal.mark = ","
  ), "km"
)

p <- p + labs(title = titulo)

p <- p + theme(
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
)


plot_map_travel_distance <- ggdraw(p) +
  theme(plot.background = element_rect(fill = "#eee8d5", color = NA))

ggsave("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/travel_distance.png",
  plot_map_travel_distance,
  width = 8, height = 6, dpi = 300
)
