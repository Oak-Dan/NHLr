normalize_goals <- function(GFgR, GAgR, OTGP, EN_goals_for,
                            EN_goals_against, total_games = 82) {
  
  # Calcular propensões de marcação
  total_goals = GFgR + GAgR
  prop_for = GFgR / total_goals
  prop_against = GAgR / total_goals
  
  # Estimar gols de final de jogo
  endgame_goals = OTGP * 0.216
  endgame_goals_for = endgame_goals * prop_for
  endgame_goals_against = endgame_goals * prop_against
  
  # Ajustar valores por jogo
  endgame_goals_for_per_game = endgame_goals_for / total_games
  endgame_goals_against_per_game = endgame_goals_against / total_games
  EN_goals_for_per_game = EN_goals_for / total_games
  EN_goals_against_per_game = EN_goals_against / total_games
  
  # Calcular valores normalizados
  normalized_goals_for = GFgR - endgame_goals_for_per_game - EN_goals_for_per_game
  normalized_goals_against = GAgR - endgame_goals_against_per_game - EN_goals_against_per_game
  
  # Arredondar para duas casas decimais
  normalized_goals_for = round(normalized_goals_for, 2)
  normalized_goals_against = round(normalized_goals_against, 2)
  
  # Retornar os resultados como uma lista
  return(list(
    goals_for_normalized = normalized_goals_for,
    goals_against_normalized = normalized_goals_against
  ))
}

normalize_goals(2.45, 2.21, 30, 6, 4)

#estimate_K <- function(GFg, GAg, GFgR, GAgR, OTW, OTL, OTGP, league_avg_K = 1.1) {
# Calcular a taxa de sucesso em overtime
#  ot_success_rate <- (OTW + 0.5 * (OTGP - OTW - OTL)) / OTGP

# Calcular a diferença entre gols totais e gols em tempo regulamentar
#  gf_diff <- (GFg - GFgR) / GFg
#  ga_diff <- (GAg - GAgR) / GAg

# Ajustar K com base nesses fatores
#  K_adjust <- 1 + (ot_success_rate - 0.5) + (gf_diff - ga_diff)

# Calcular K estimado
#  K_estimated <- league_avg_K * K_adjust

# Limitar K a um intervalo razoável (por exemplo, entre 0.7 e 1.3)
#  K_final <- max(min(K_estimated, 1.3), 0.7)

#  return(K_final)
#}

# Exemplo de uso para St. Louis Blues (usando dados hipotéticos onde necessário)
#result <- estimate_K(
#  GFg = (2.24*82),   # Gols a favor por jogo (total)
#  GAg = (2.60*82),   # Gols contra por jogo (total)
#  GFgR = 2.20,  # Gols a favor por jogo em tempo regulamentar
#  GAgR = 2.50,  # Gols contra por jogo em tempo regulamentar
#  OTW = 4,     # Vitórias em overtime
#  OTL = 8,      # Derrotas em overtime
#  OTGP = 22     # Total de jogos em overtime
#)

#print(paste("K estimado:", result))

#estimate_endgame_goals <- function(GFgR, GAgR, OTGP, K) {
#  base_endgame_goals <- OTGP * 0.216
#  prop_GF <- GFgR / (GFgR + GAgR)
#  prop_GA <- GAgR / (GFgR + GAgR)

#  GFEGt <- base_endgame_goals * prop_GF * K
#  GAEGl <- base_endgame_goals * prop_GA * K

#  return(list(GFEGt = round(GFEGt, 2), GAEGl = round(GAEGl, 2)))
#}

# Usando o K estimado para calcular GFEGt e GAEGl
#endgame_goals <- estimate_endgame_goals(2.54, 2.91, 18, result)
#print(paste("GFEGt estimado:", endgame_goals$GFEGt))
#print(paste("GAEGl estimado:", endgame_goals$GAEGl))


# Posso dividir em como mandante e como visitante
# gf_mandante e ga_mandante
# gf_visitante e ga_visitante

