# Instale os pacotes necess<U+00E1>rios
# install.packages("httr")
# install.packages("rsvg")
# install.packages("dplyr")
# install.packages("purrr")

# Carregue os pacotes necessarios
library(tidyverse)
library(httr)
library(rsvg)
# library(dplyr)
# library(purrr)

# Defina a fun<U+00E7><U+00E3>o baixar_e_converter_svg
baixar_e_converter_svg <- function(url, png_path, width = 2000, height = 1000) {
  temp_svg <- tempfile(fileext = ".svg")
  GET(url, write_disk(temp_svg, overwrite = TRUE))
  rsvg_png(temp_svg, png_path, width = width, height = height)
  unlink(temp_svg)
  return(png_path)
}

# Crie um dataframe de exemplo
df_raw <- readRDS("/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/data/nhl_schedule.RDS")

# Filtrar pelas imagens unicas no df_raw
df <- df_raw %>%
  select(homeTeam_logo_dark) %>%
  distinct_all()

# Defina o diret<U+00F3>rio de destino para os arquivos PNG
output_dir <- "/Users/danilooak/Documents/Code/R Projects/Sports Analytics/Hockey/imgs/Logos/Logos Dark"

# Crie uma fun<U+00E7><U+00E3>o auxiliar para gerar os caminhos PNG e baixar/convert<U+00EA>-los
converter_e_salvar <- function(svg_url, output_dir) {
  file_name <- tools::file_path_sans_ext(basename(svg_url))
  png_path <- file.path(output_dir, paste0(file_name, ".png"))
  baixar_e_converter_svg(svg_url, png_path)
}

# Aplique a fun<U+00E7><U+00E3>o a cada caminho SVG na coluna do dataframe
walk(df$homeTeam_logo_dark, converter_e_salvar, output_dir = output_dir)
