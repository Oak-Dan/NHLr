setwd("c:/Users/danil/Documents/NHL Data Analysis/NHLBrasil/src/")
# Libraries ---------------------------------------------------------------

library(tidyverse) # all the things
library(ggExtra)   # marginal plots
library(ggtext)    # color your text
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales)
library(imputeTS)
library(metR)
library(cowplot)
library(ggnewscale)

# Aux Functions -----------------------------------------------------------

source(file = "nhl_rink.R") # NHL rink
source(file = "theme_owen.R") # ggplot theme


# Load dataset ------------------------------------------------------------

df <- read_csv("~/NHL Data Analysis/NHLBrasil/datasets/allshotsgoals.csv")


# Data preparation --------------------------------------------------------

df$xC <- na_replace(df$xC, 0)
df$yC <- na_replace(df$yC, 0)


df <- df %>% 
  select(-...59) %>% 
  filter((Event == "GOAL"|Event == "SHOT"),
         Period < 4,
         Strength == "5x5") %>% 
  mutate(xC = case_when(
    xC < 0 ~ xC*-1,
    TRUE ~ xC),
    yC = case_when(
      yC < 0 ~ yC*-1,
      yC > 0 ~ yC*-1,
      TRUE ~ yC
    ))

df <- df %>% 
  filter(xC < 90)

# Skater ------------------------------------------------------------------

qb_density_compareJ <- function(df, player, n = 300){
  
  # filter to ply1
  ply1 <- df %>% 
    select(xC, yC, p1_name) %>% 
    filter(str_detect(p1_name, player))
  
  #filter to liga tira para nao repetir
  liga <- df %>% 
    select(xC, yC, p1_name) %>% 
    filter(p1_name != player)
  
  # get x/y coords as vectors
  ply1_x <- pull(ply1, xC)
  ply1_y <- pull(ply1, yC)
  
  # get x/y coords as vectorsS
  liga_x <- pull(liga, xC)
  liga_y <- pull(liga, yC)
  
  # get x and y range to compute comparisons across
  x_rng = range(c(0, 89))
  y_rng = range(c(-42,5, 42,5))
  
  # Explicitly calculate bandwidth for future use
  bandwidth_x <- MASS::bandwidth.nrd(c(ply1_x, liga_x))
  bandwidth_y <- MASS::bandwidth.nrd(c(ply1_y, liga_y))
  
  bandwidth_calc <- c(bandwidth_x, bandwidth_y)
  
  # Calculate the 2d density estimate over the common range
  d2_ply1 = MASS::kde2d(ply1_x, ply1_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  d2_liga = MASS::kde2d(liga_x, liga_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  
  # create diff df
  qb_diff <- d2_ply1
  
  # matrix subtraction density from liga from ply1
  qb_diff$z <- d2_ply1$z - d2_liga$z
  
  # add matrix col names
  colnames(qb_diff$z) = qb_diff$y
  
  #### return tidy tibble ####
  qb_diff$z %>% 
    # each col_name is actually the y_coord from the matrix
    as_tibble() %>% 
    # add back the x_coord
    mutate(x_coord= qb_diff$x) %>% 
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
    mutate(y_coord = as.double(y_coord),
           bandwidth = list(bandwidth_calc),
           Player = player)
  
}



df_jogador = data.frame()
lista_seasonJ <- unique(df$Season)
lista_seasonJ <- lista_seasonJ[c(-1,-2,-3,-4,-5,-6,-7,-8)]

for (i in lista_seasonJ){
  # # vector output
  valor <- qb_density_compareJ(df %>%
                                filter(Season == i),
                               player = "CONNOR MCDAVID", n = 300)
  
  valor$season <- i
  #
  #   # add vector to a dataframe
  #   df <- data.frame(model)
  df_jogador <- rbind(df_jogador,valor)
  
}
 

df_jogador_negative <- df_jogador %>% filter(z < 0)

# make positive 
df_jogador_negative$z <- abs(df_jogador_negative$z)

df_jogador_positive <- df_jogador

# if less than 0, make 0
df_jogador_positive$z <- ifelse(df_jogador_positive$z < 0, 0, df_jogador_positive$z)

# vector of max differences (use this for plot limits)
diffs <- c(sqrt(max(df_jogador_positive$z)), sqrt(max(df_jogador_negative$z)))

p <- ggplot() +
  geom_contour_fill(data = df_jogador_positive %>% filter(z >= mean(z)),
                    aes(x = x_coord, y = y_coord, z = sqrt(z)))  +
  geom_contour_fill(data = df_jogador_positive %>% filter(z == 0),
                    aes(x = x_coord, y = y_coord, z = sqrt(z)),
                    fill = 'floralwhite')  +
  geom_contour_tanaka(data = df_jogador_positive %>% filter(z >= mean(z)),
                     aes(x = x_coord, y = y_coord, z = sqrt(z)), bins = 4,
                     smooth = 1) +
  scale_fill_gradient2(low = '#7D4F73FF', midpoint = 0, mid = 'floralwhite',
                       high = "#E31A1CFF", limits = c(0, max(diffs)+.001))  +
  new_scale_fill() +
  geom_contour_fill(data = df_jogador_negative %>% filter(z >= mean(z)),
                    aes(x = x_coord, y = y_coord, z = sqrt(z)))  +
  geom_contour_fill(data = df_jogador_negative %>% filter(z == 0), 
                    aes(x = x_coord, y = y_coord, z = sqrt(z)), 
                    fill = 'floralwhite')  +
  geom_contour_tanaka(data = df_jogador_negative %>% filter(z >= mean(z)),
                     aes(x = x_coord, y = y_coord, z = sqrt(z)), bins = 4,
                     smooth = 1) +
  scale_fill_gradient2(low="#008A80FF", mid = "floralwhite", 
                       high="navyblue", midpoint = 0, 
                       limits = c(0, max(diffs)+.001)) +
  guides(color="none", fill = "none") +
  facet_wrap(~season, nrow = 2, strip.position = 'top') +
  gg_rink() +
  coord_equal() +
  theme_owen() +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.spacing  = unit(-.0005, "lines"), 
        plot.title = element_markdown(hjust= .5, size = 14, color = 'black'),
        plot.subtitle = element_markdown(hjust= .5, size = 13,
                                     color = 'black'),
        plot.caption  = element_text(size = 10, hjust = 0, color = 'black'),
        strip.text = element_text(size = 8, vjust = -1, face = 'bold')) + 
  scale_y_continuous(limits = c(-42.5, 42.5)) +
  scale_x_continuous(limits = c(25, 100))  +
  #coord_fixed(clip = 'off') +
  labs(title =  "De onde <span style='color:red'>**Connor McDavid**</span> gosta de chutar",
       subtitle = "em relação à média da <span style='color:blue'>**NHL**</span>",
       caption = "Autor: Danilo Carvalho")

p <- ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))



ggsave("~/NHL Data Analysis/NHLBrasil/imagens/Density97.png", p, width = 10, height = 10, dpi = 300,
       type = 'cairo')

## TIME ########################################################################


qb_density_compareT <- function(df, time, n = 300){
  
  # filter to ply1
  ply1 <- df %>% 
    select(xC, yC, Ev_Team) %>% 
    filter(str_detect(Ev_Team, time))
  
  #filter to liga tira para nao repetir
  liga <- df %>% 
    select(xC, yC, Ev_Team) %>% 
    filter(Ev_Team != time)
  
  # get x/y coords as vectors
  ply1_x <- pull(ply1, xC)
  ply1_y <- pull(ply1, yC)
  
  # get x/y coords as vectors
  liga_x <- pull(liga, xC)
  liga_y <- pull(liga, yC)
  
  # get x and y range to compute comparisons across
  x_rng = range(c(0, 89))
  y_rng = range(c(-42,5, 42,5))
  
  # Explicitly calculate bandwidth for future use
  bandwidth_x <- MASS::bandwidth.nrd(c(ply1_x, liga_x))
  bandwidth_y <- MASS::bandwidth.nrd(c(ply1_y, liga_y))
  
  bandwidth_calc <- c(bandwidth_x, bandwidth_y)
  
  # Calculate the 2d density estimate over the common range
  d2_ply1 = MASS::kde2d(ply1_x, ply1_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  d2_liga = MASS::kde2d(liga_x, liga_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  
  # create diff df
  qb_diff <- d2_ply1
  
  # matrix subtraction density from liga from ply1
  qb_diff$z <- d2_ply1$z - d2_liga$z
  
  # add matrix col names
  colnames(qb_diff$z) = qb_diff$y
  
  #### return tidy tibble ####
  qb_diff$z %>% 
    # each col_name is actually the y_coord from the matrix
    as_tibble() %>% 
    # add back the x_coord
    mutate(x_coord= qb_diff$x) %>% 
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
    mutate(y_coord = as.double(y_coord),
           bandwidth = list(bandwidth_calc),
           Time = time)
  
}

# tira o jogador selecionada em cima
# df2 <- df %>% 
#   filter(p1_name != "CONNOR MCDAVID",
#          homePlayer1 != "CONNOR MCDAVID",
#          homePlayer2 != "CONNOR MCDAVID",
#          homePlayer3 != "CONNOR MCDAVID",
#          homePlayer4 != "CONNOR MCDAVID",
#          homePlayer5 != "CONNOR MCDAVID",
#          homePlayer6 != "CONNOR MCDAVID",
#          awayPlayer1 != "CONNOR MCDAVID",
#          awayPlayer2 != "CONNOR MCDAVID",
#          awayPlayer3 != "CONNOR MCDAVID",
#          awayPlayer4 != "CONNOR MCDAVID",
#          awayPlayer5 != "CONNOR MCDAVID",
#          awayPlayer6 != "CONNOR MCDAVID")

jogador_off = data.frame()
#lista_season <- unique(df$Season)
#lista_season <- lista_season[c(-1,-2,-3)]
lista_times <- unique(df$ 

for (i in lista_season){
  # # filtra pelo time sem o jogador
  valor_off <- qb_density_compareT(df2 %>%
                                filter(Season == i),
                              time = "EDM", n = 300)
  
  valor_off$season <- i
  #
  #   # add vector to a dataframe
  #   df <- data.frame(model)
  jogador_off <- rbind(jogador_off,valor_off)
  
}


jogador_off_negative <- jogador_off %>% 
  filter(z < 0)

# make positive 
jogador_off_negative$z <- abs(jogador_off_negative$z)

jogador_off_positive <- jogador_off

# if less than 0, make 0
jogador_off_positive$z <- ifelse(jogador_off_positive$z < 0, 0, jogador_off_positive$z)

# vector of max differences (use this for plot limits)
diffs <- c(sqrt(max(jogador_off_positive$z)), sqrt(max(jogador_off_negative$z)))

p2 <- ggplot() +
  geom_contour_fill(data = jogador_off_positive %>% filter(z >= mean(z)),
                    aes(x = x_coord, y = y_coord, z = sqrt(z)))  +
  geom_contour_fill(data = jogador_off_positive %>% filter(z == 0),
                    aes(x = x_coord, y = y_coord, z = sqrt(z)),
                    fill = 'floralwhite')  +
  geom_contour_tanaka(data = jogador_off_positive %>% filter(z >= mean(z)),
                      aes(x = x_coord, y = y_coord, z = sqrt(z)), bins = 4,
                      smooth = 1) +
  scale_fill_gradient2(low = '#7D4F73FF', midpoint = 0, mid = 'floralwhite',
                       high = "#E31A1CFF", limits = c(0, max(diffs)+.001))  +
  new_scale_fill() +
  geom_contour_fill(data = jogador_off_negative %>% filter(z >= mean(z)),
                    aes(x = x_coord, y = y_coord, z = sqrt(z)))  +
  geom_contour_fill(data = jogador_off_negative %>% filter(z == 0), 
                    aes(x = x_coord, y = y_coord, z = sqrt(z)), 
                    fill = 'floralwhite')  +
  geom_contour_tanaka(data = jogador_off_negative %>% filter(z >= mean(z)),
                      aes(x = x_coord, y = y_coord, z = sqrt(z)), bins = 4,
                      smooth = 1) +
  scale_fill_gradient2(low="#008A80FF", mid = "floralwhite", 
                       high="navyblue", midpoint = 0, 
                       limits = c(0, max(diffs)+.001)) +
  guides(color="none", fill = "none") +
  facet_wrap(~season, nrow = 2, strip.position = 'top') +
  gg_rink() +
  coord_equal() +
  theme_owen() +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.spacing  = unit(-.0005, "lines"), 
        plot.title = element_markdown(hjust= .5, size = 14, color = 'black'),
        plot.subtitle = element_markdown(hjust= .5, size = 13,
                                         color = 'black'),
        plot.caption  = element_text(size = 10, hjust = 0, color = 'black'),
        strip.text = element_text(size = 8, vjust = -1, face = 'bold')) + 
  scale_y_continuous(limits = c(-42.5, 42.5)) +
  scale_x_continuous(limits = c(25, 100))  +
  #coord_fixed(clip = 'off') +
  labs(title =  "De onde as <span style='color:red'>**Edmonton Oilers**</span> gosta de chutar quando Connor McDavid não está no gelo",
       subtitle = "em relação à média da <span style='color:blue'>**NHL**</span>",
       caption = "Autor: Danilo Carvalho")

p2 <- ggdraw(p2) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))


#p


ggsave("~/NHL Data Analysis/NHLBrasil/imagens/DensityEDM.png", p2, width = 10, height = 10, dpi = 300,
       type = 'cairo')

