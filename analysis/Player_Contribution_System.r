library(tidyverse)
library(tibble)
library(janitor)
library(hockeyR)


# Criar o DataFrame a partir dos dados
dados_nhl <- tribble(
  ~Team, ~GP, ~W, ~L, ~T, ~GF, ~GA, ~`EV GF`, ~`PP GF`, ~`SH GF`, ~`EV GA`, ~`PP GA`, ~`SH GA`, ~ENG, ~SOG, ~SV,
  "Anaheim Ducks", 82, 40, 27, 9, 203, 193, 139, 56, 8, 146, 42, 5, 5, 2378, 0.919,
  "Atlanta Thrashers", 82, 31, 39, 7, 226, 284, 154, 64, 8, 205, 65, 14, 5, 2591.2, 0.890,
  "Boston Bruins", 82, 36, 31, 11, 245, 237, 176, 59, 10, 161, 65, 11, 7, 2320.6, 0.898,
  "Buffalo Sabres", 82, 27, 37, 10, 190, 219, 131, 51, 8, 155, 53, 11, 4, 2296, 0.905,
  "Calgary Flames", 82, 29, 36, 13, 186, 228, 134, 47, 5, 153, 65, 10, 5, 2222.2, 0.897,
  "Carolina Hurricanes", 82, 22, 43, 11, 171, 240, 107, 58, 6, 160, 71, 9, 4, 2328.8, 0.897,
  "Chicago Blackhawks", 82, 30, 33, 13, 207, 226, 160, 39, 8, 162, 56, 8, 2, 2345.2, 0.904,
  "Colorado Avalanche", 82, 42, 19, 13, 251, 194, 178, 68, 5, 125, 63, 6, 4, 2320.6, 0.916,
  "Columbus Blue Jackets", 82, 29, 42, 8, 213, 263, 130, 71, 12, 194, 60, 9, 9, 2640.4, 0.900,
  "Dallas Stars", 82, 46, 17, 15, 245, 169, 174, 62, 9, 113, 50, 6, 13, 2074.6, 0.919,
  "Detroit Red Wings", 82, 48, 20, 10, 269, 203, 182, 76, 11, 144, 55, 4, 6, 2361.6, 0.914,
  "Edmonton Oilers", 82, 36, 26, 11, 231, 230, 162, 56, 13, 161, 61, 8, 7, 2246.8, 0.898,
  "Florida Panthers", 82, 24, 36, 13, 176, 237, 120, 49, 7, 162, 66, 9, 7, 2722.4, 0.913,
  "Los Angeles Kings", 82, 33, 37, 6, 203, 221, 143, 52, 8, 155, 62, 4, 6, 2140.2, 0.897,
  "Minnesota Wild", 82, 42, 29, 10, 198, 178, 137, 52, 9, 130, 43, 5, 10, 2337, 0.924,
  "Montreal Canadiens", 82, 30, 35, 8, 206, 234, 160, 44, 2, 170, 58, 6, 7, 2681.4, 0.913,
  "Nashville Predators", 82, 27, 35, 13, 183, 206, 123, 58, 2, 137, 61, 8, 1, 2255, 0.909,
  "New Jersey Devils", 82, 46, 20, 10, 216, 166, 173, 36, 7, 130, 32, 4, 6, 1935.2, 0.914,
  "New York Islanders", 82, 35, 34, 11, 224, 231, 154, 58, 12, 159, 67, 5, 9, 2320.6, 0.900,
  "New York Rangers", 82, 32, 36, 10, 210, 231, 147, 55, 8, 150, 73, 8, 1, 2427.2, 0.905,
  "Ottawa Senators", 82, 52, 21, 8, 263, 182, 176, 83, 4, 124, 50, 8, 12, 2033.6, 0.911,
  "Philadelphia Flyers", 82, 45, 20, 13, 211, 166, 156, 47, 8, 108, 50, 8, 3, 2017.2, 0.918,
  "Phoenix Coyotes", 82, 31, 35, 11, 204, 230, 145, 55, 4, 142, 77, 11, 5, 2460, 0.907,
  "Pittsburgh Penguins", 82, 27, 44, 6, 189, 255, 120, 66, 3, 187, 58, 10, 5, 2533.8, 0.899,
  "San Jose Sharks", 82, 28, 37, 9, 214, 239, 141, 68, 5, 161, 68, 10, 3, 2394.4, 0.900,
  "St. Louis Blues", 82, 41, 24, 11, 253, 222, 162, 80, 11, 144, 71, 7, 12, 2050, 0.892,
  "Tampa Bay Lightning", 82, 36, 25, 16, 219, 210, 143, 70, 6, 150, 55, 5, 6, 2296, 0.909,
  "Toronto Maple Leafs", 82, 44, 28, 7, 236, 208, 162, 63, 11, 145, 56, 7, 7, 2419, 0.914,
  "Vancouver Canucks", 82, 45, 23, 13, 264, 208, 165, 87, 12, 136, 62, 10, 8, 2181.2, 0.905,
  "Washington Capitals", 82, 39, 29, 8, 224, 220, 159, 57, 8, 144, 72, 4, 6, 2451.8, 0.910
)

teams <- c("New Jersey Devils", "Philadelphia Flyers", "Minnesota Wild",
           "Calgary Flames", "Toronto Maple Leafs", "Tampa Bay Lightning",
           "Buffalo Sabres", "Ottawa Senators", "Anaheim Ducks", "Dallas Stars",
           "Washington Capitals", "Detroit Red Wings", "Phoenix Coyotes",
           "Montreal Canadiens", "Chicago Blackhawks", "Pittsburgh Penguins",
           "Nashville Predators", "Vancouver Canucks", "Colorado Avalanche",
           "Edmonton Oilers", "San Jose Sharks", "Carolina Hurricanes",
           "Boston Bruins", "New York Islanders", "Columbus Blue Jackets",
           "Atlanta Thrashers", "Los Angeles Kings", "New York Rangers",
           "Florida Panthers", "St. Louis Blues")
sqa <- c(0.915, 0.935, 0.947, 0.953, 0.956, 0.965, 0.969, 0.970, 0.972, 0.972,
         0.973, 0.976, 0.980, 0.982, 0.996, 0.996, 1.000, 1.004, 1.018, 1.018,
         1.022, 1.024, 1.027, 1.037, 1.041, 1.045, 1.048, 1.057, 1.078, 1.087)

nhl_goal <- as_tibble(data.frame(Team = teams , SQA = sqa ))

dados_nhl <- left_join(dados_nhl, nhl_goal, by="Team")

dados_nhl <- dados_nhl |> 
  clean_names()


dados_nhl_marginal <- dados_nhl |> 
  mutate(wins = w + (t/2)) |> 
  select(team, gf, ga, wins)


# Predicted Winning Percentage

# Dados fornecidos
G_s <- 205  # Gols Marcados
G_a <- 205  # Gols Sofridos
total_games <- 82

# Porcentagem de vitórias esperadas atual
expected_win_percentage <- 1 / (1 + (G_a / G_s) ^ 2)

# Vitórias esperadas atuais
expected_wins <- expected_win_percentage * total_games

# Função para calcular vitórias esperadas com aumento de gols marcados
calculate_additional_goals_for_extra_win <- function(G_s, G_a, total_games, current_expected_wins) {
  additional_goals <- 0
  repeat {
    additional_goals <- additional_goals + 1
    new_G_s <- G_s + additional_goals
    new_expected_win_percentage <- 1 / (1 + (G_a / new_G_s) ^ 2)
    new_expected_wins <- new_expected_win_percentage * total_games
    
    if (floor(new_expected_wins) > floor(current_expected_wins)) {
      return(list(additional_goals = additional_goals, new_expected_wins = new_expected_wins))
    }
  }
}

# Calcular o número de gols adicionais necessários para uma vitória extra
result <- calculate_additional_goals_for_extra_win(G_s, G_a, total_games, expected_wins)

#expected_wins
result$additional_goals
result$new_expected_wins
expected_wins



# 1. Determine Team “Marginal Goals” (MG) ---------------------------------
# Marginal Goals = Goals For – Goals Against + League Average Goals For
# Predicted Winning Percentage = MG / (2 x GF<) ('<') league average
# Average Marginal Goals per Win = (GF< + GA<) / GP = 2 x GF< / GP


dados_nhl_marginal$mg <- round(dados_nhl_marginal$gf - dados_nhl_marginal$ga + mean(dados_nhl_marginal$gf),1)


# 2. Allocate Marginal Goals to Marginal Goals Created (MGC) and Marginal Goals Prevented (MGP) --------
# Marginal Goals Created = MGC = GF - (GF< x Threshold Percentage)
# Marginal Goals Prevented = MGP = (GF< x (1 + Threshold Percentage)) - GA
# MGC + MGP = GF – GA + GF< = MG (Marginal Goals)

DA <- 58.346/100

dados_nhl_marginal$mgc <- round(dados_nhl_marginal$gf - (mean(dados_nhl_marginal$gf) * DA),2)
dados_nhl_marginal$mgp <- round((mean(dados_nhl_marginal$gf) * (1 + DA)) - dados_nhl_marginal$ga,1)


dados_nhl_marginal$mg_w <- round(dados_nhl_marginal$mg/dados_nhl_marginal$wins,1)


# 3. Split MGP into Marginal Goals Defense (MGD) and Marginal Goals Goaltending (MGG) --------
# Marginal Goals Prevented = Marginal Goals Defense + Marginal Goals Goaltending
# or
# MGP = MGD + MGG

# MGG = Marginal Goals Goaltending
#     = Scoring Opportunities x Save Percentage - Scoring Opportunities x Save Percentage Threshold
#     = Scoring Opportunities x (Save Percentage - Save Percentage Threshold)
#     = (Shots on Goal – Empty Net Goals) x (Save Percentage – Save Percentage Threshold)
# or
# MGG = (SOG – ENG) x (SV% -SPT)

# Alternatively:
# MGG = Threshold Goals – Actual Goals
#     = (Shots on Goal – Empty Net Goals) x (1–Save Percentage Threshold) – (Goals Against–Empty Net Goals)
# or
# MGG = (SOG – ENG) x (1 – SPT) – (GA – ENG)
#     = (SOG – ENG) x (1 – SPT) – (SOG x (1 – SV) – ENG)

# SPT = .893 =1 - (GA x 7/6 - ENG) / (SOG - ENG)

# GA = SOG x (1 – SV)

# GA = SQA x SOG x (1 – SQNSV) where SQA is the Shot Quality Against Index and SQNSV is the Shot
# Quality Neutral Save Percentage

# How do you calculate SQNSV? The two models both give us Goals Against, so:
# SOG x (1 – SV) = SQA x SOG x (1 – SQNSV),
# or
# (1 – SV) = SQA x (1 – SQNSV),
# which means
# SQNSV = 1 – (1- SV) / SQA

# MGG = (SOG – ENG) x (1 – SPT) – (SOG x (1 – SQNSV) – ENG)

dados_nhl$sqnsv <- round(1 - (1 - dados_nhl$sv)/dados_nhl$sqa,3) 

#nhl_goalteanding_stats$SQA <- (1 - nhl_goalteanding_stats$SV)/(1 - nhl_goalteanding_stats$SQNSV)

nhl_goalteanding_stats <- dados_nhl |> 
  select(team, sv, sqnsv, sqa)

#nhl_goalteanding_stats$SQNSV <- 1 - (1 - nhl_goalteanding_stats$SV) / nhl_goalteanding_stats$SQA


# 4. Allocate MGC and MGD to “Situations” ------------------------------------
# (Even Handed, Power Play, Shorthanded, Penalties)

# MGC = GF - (GF< x DA)
# So let’s just break it into its component parts:


mgc_mgd_df <- dados_nhl
mgc_mgd_df$mgc <- round(mgc_mgd_df$gf - (mean(mgc_mgd_df$gf) * DA))


# MGCEH = GFEH - (GFEH< x DA) -- Even Handed
mgc_mgd_df$mgcef <- round(mgc_mgd_df$ev_gf - (mean(mgc_mgd_df$ev_gf) * DA),1)
# MGCPP = GFPP - (GFPP< x DA) -- Power Play
mgc_mgd_df$mgcpp <- round(mgc_mgd_df$pp_gf - (mean(mgc_mgd_df$pp_gf) * DA),1)
# MGCSH = GFSH - (GFSH< x DA) -- Short Handed
mgc_mgd_df$mgcsh <- round(mgc_mgd_df$sh_gf - (mean(mgc_mgd_df$sh_gf) * DA),1)

#mgc_mgd_df <- mgc_mgd_df |> 
#  select(team, mgc2, mgcef, mgcpp, mgcsh)


# We can do the same for Marginal Goals Prevented, except that we need to take note of
# the part that has already been allocated to goaltending. To do that let’s define:

# G = MGG / MGP -- the percentage of MGP allocated to goaltending

mgc_mgd_df$mgg <- ((dados_nhl$sog - dados_nhl$eng) * (1-0.893)) - ((dados_nhl$sog * (1 - dados_nhl$sqnsv)) - dados_nhl$eng)
mgc_mgd_df$mgp <- (round((mean(mgc_mgd_df$gf) * (1 + DA)) - mgc_mgd_df$ga,1))
mgc_mgd_df$g <- mgc_mgd_df$mgg/ mgc_mgd_df$mgp


# Then:
# MGD = MGP – MGG =(1 – G) x MGP
mgc_mgd_df$mgd <- round((1 - mgc_mgd_df$g) *  mgc_mgd_df$mgp,1)
# MGDEH = (1 – G) x ((GAEH< x (1 + DA)) – GAEH)
mgc_mgd_df$mgdeh <- round((1 - mgc_mgd_df$g) * ((mean(mgc_mgd_df$ev_ga) * (1 + DA)) - mgc_mgd_df$ev_ga),1)
# MGDPP = (1 – G) x ((GAPP< x (1 + DA)) – GAPP)
mgc_mgd_df$mgdsh <- round((1 - mgc_mgd_df$g) * ((mean(mgc_mgd_df$pp_ga) * (1 + DA)) - mgc_mgd_df$pp_ga),1)
# MGDSH = (1 – G) x ((GASH< x (1 + DA)) – GASH)
mgc_mgd_df$mgdpp <- round((1 - mgc_mgd_df$g) * ((mean(mgc_mgd_df$sh_ga) * (1 + DA)) - mgc_mgd_df$sh_ga),1)


mgc_mgd_df$mg <- round(mgc_mgd_df$gf - mgc_mgd_df$ga + mean(mgc_mgd_df$gf),1)
mgc_mgd_df$`mgc_%` <- round(mgc_mgd_df$mgc/mgc_mgd_df$mg,1)
mgc_mgd_df$`mgd_%` <- round(mgc_mgd_df$mgd/mgc_mgd_df$mg,1)
mgc_mgd_df$`mgg_%` <- round(mgc_mgd_df$mgg/mgc_mgd_df$mg,1)

mgc_mgd_df <- mgc_mgd_df |> 
  select(team, mgc, `mgc_%`, mgcef, mgcpp, mgcsh, mgp, mgd, `mgd_%`, mgdeh, mgdpp, mgdsh, mgg, `mgg_%`, g)


# 5. Translate Marginal Goals to Wins and to Player Contribution (PC) --------
# Player Contribution = Player Contribution Factor x Marginal Goals
# or
# PC = PCF x MG
# where
# PCF = PCSF / (MG / W)
pcsf <- 20


# 6. Allocate MGC to Individual Players (PCO) -----------------------------




# 7. Allocate MGD to Individual Players (PCD) -----------------------------




# 8. Allocate MGG to Individual Goaltenders (PCG) -------------------------




