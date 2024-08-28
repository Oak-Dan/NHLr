library(tidyverse) # all the things
library(ggExtra)   # marginal plots
library(ggtext)    # color your text
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales)
library(imputeTS)
library(hexbin)
library(prismatic)
library(teamcolors)
library(cowplot)


source("C:/Users/danil/Documents/Projetos/NHLr/src/rinkplot.R")

theme_owen <- function () {
  theme_minimal(base_size=11) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#f3deaf',
                                     color = "#f3deaf"),
      panel.grid = element_blank()
    )
}

#tm_colors <- read_csv(file = "NHLr/datasets/teamsv1.csv")

nhl_shot_map <- read.csv("C:/Users/danil/Documents/Projetos/NHLr/datasets/nhl_pbp_20202021.csv")

nhl_shot_map$xC <- na_replace(nhl_shot_map$xC, 0)
nhl_shot_map$yC <- na_replace(nhl_shot_map$yC, 0)

# pegando so os chutes
nhl_shot_map <- nhl_shot_map %>%
  select(-X) %>%
  filter((Event == "GOAL"|Event == "SHOT"),
         (Ev_Zone == "Off"| Ev_Zone == "Neu"),
         Period < 4) %>%
  mutate(xC = case_when(
      xC < 0 ~ xC*-1,
      TRUE ~ xC),
      yC = case_when(
        yC < 0 ~ yC*-1,
        yC > 0 ~ yC*-1,
        TRUE ~ yC
    ))

nhl_shot_map <- nhl_shot_map %>%
  filter(xC < 90)


# Create a function that helps create our custom hexs
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

# Set the size of the hex
binwidths <- 3.5

# Calculate the area of the court that we're going to divide into hexagons
xbnds <- hex_bounds(nhl_shot_map$xC, binwidths)
xbins <- diff(xbnds) / binwidths
ybnds <- hex_bounds(nhl_shot_map$yC, binwidths)
ybins <- diff(ybnds) / binwidths

# Create a hexbin based on the dimensions of our court
hb <- hexbin(
  x = nhl_shot_map$xC,
  y = nhl_shot_map$yC,
  xbins = xbins,
  xbnds = xbnds,
  ybnds = ybnds,
  shape = ybins / xbins,
  IDs = TRUE
)

# map our hexbins onto our dataframe of shot attempts
df <- mutate(nhl_shot_map, hexbin_id = hb@cID)


# find the leauge avg % of shots coming from each hex
la <- df %>%
  group_by(hexbin_id) %>%
  summarize(hex_attempts = n()) %>%
  ungroup() %>%
  mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>%
  ungroup() %>%
  rename("league_average" = "hex_pct") %>%
  select(-hex_attempts)

# Calculate the % of shots coming from each hex for each team
hexbin_stats <- df %>%
  group_by(hexbin_id, Ev_Team) %>%
  summarize(hex_attempts = n()) %>%
  ungroup() %>%
  group_by(Ev_Team) %>%
  mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>%
  ungroup()

hexbin_stats <- hexbin_stats %>%
  left_join(., la) %>%
  group_by(hexbin_id) %>%
  mutate(sd_hex_pct = sd(hex_pct, na.rm = TRUE),
         z_score = (hex_pct - league_average) / sd_hex_pct)


# Full disclosure, no idea what this next part does
# from hexbin package, see: https://github.com/edzer/hexbin
sx <- hb@xbins / diff(hb@xbnds)
sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
dx <- 1 / (2 * sx)
dy <- 1 / (2 * sqrt(3) * sy)
origin_coords <- hexcoords(dx, dy)

hex_centers <- hcell2xy(hb)

hexbin_coords <- bind_rows(lapply(1:hb@ncells, function(i) {
  data.frame(
    x = origin_coords$x + hex_centers$x[i],
    y = origin_coords$y + hex_centers$y[i],
    center_x = hex_centers$x[i],
    center_y = hex_centers$y[i],
    hexbin_id = hb@cell[i]
  )
}))

# Merge out hexbin coordinates with our hexbin stats
hex_data <- inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")

# Adjusts the size of the hexagons
hex_data <- hex_data %>%
  mutate( radius_factor = .99,
          adj_x = center_x + radius_factor * (x - center_x),
          adj_y = center_y + radius_factor * (y - center_y))

hex_data <- hex_data %>% filter(hex_attempts >= 5 & hex_data$z_score > 0.3)
# merge with the team colors
hex_data <- left_join(hex_data, tm_colors, by = c("Ev_Team" = "home_team"))

p <- ggplot() +
  gg_rink() +
  geom_polygon(data = hex_data,
               aes(x = adj_x, y = adj_y, alpha = sqrt(z_score), fill = team_colors.x, color = after_scale(clr_darken(fill, 0.3)), group = hexbin_id),
               size = .25) +
  theme_owen() +
  facet_wrap(~Ev_Team, nrow = 5, strip.position = 'top') +
  scale_alpha_continuous(range = c(.05, 1)) +
  scale_fill_identity() +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.spacing  = unit(-0.05, "lines"),
        plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black'),
        plot.caption  = element_text(size = 6, hjust= .5, color = 'black'),
        strip.text = element_text(size = 8, vjust = -1, face = 'bold')) +
  scale_y_continuous(limits = c(-42, 42)) +
  scale_x_continuous(limits = c(20, 100))  +
  #coord_fixed(clip = 'off') +
  coord_equal(clip = "off") +
  labs(title =  "Where Teams Like To Shoot From\nRelative To League Average",
       caption = "Darker and denser areas indicate a team takes more shots from that spot relative to the league as a whole")

p
p <- ggdraw(p) +
  theme(plot.background = element_rect(fill="#f3deaf", color = NA))

ggsave("HexChart.png", p, width = 6, height = 7, dpi = 300, type = 'cairo')
#install.packages("metR")
