library(memoise)
library(zeallot)
library(future.apply)
library(assertthat)

#' Calculate the probability of winning in a sports game
#'
#' This function calculates the probability of winning, losing, or tying in a sports game,
#' given various parameters about the game state and scoring rates.
#'
#' @param u Numeric. Intensity of Goals For [Average Goals For per Game (>=0)]
#' @param v Numeric. Intensity of Goals Against [Average Goals Against per Game (>=0)]
#' @param time Numeric. Elapsed Percentage of Game (0<=time<=1)
#' @param lead Integer. Goal Differential at Time of Calculation
#' @param et Numeric. Scoring Rate (% of u and v) for trailing team in "Endgame"
#' @param el Numeric. Scoring Rate (% of u and v) for leading team in "Endgame"
#' @param ot Numeric. Percentage Extra Scoring Rate (% of u and v) in Overtime
#' @param ot_type Integer. Overtime Type: 0 = None, 1 = Sudden Death (Playoff), 2 = 5 Minute (Regular Season)
#' @param calc Integer. Calculation to perform: -1 = Pr(Loss), 0 = Pr(Tie), 1 = Pr(Win), 2 = Expected Points, 3 = Win Equivalent (W + T/2)
#'
#' @return Numeric. The calculated probability or point value based on the calc parameter
#'
#' @examples
#' pr_win(3.0, 2.5, (10+(16/60))/60, 1, 0, 0, 0, 0, 3)
#'
#' @export
pr_win <- function(u, v, time, lead, et, el, ot, ot_type, calc) {
  # Input validation
  assert_that(
    is.numeric(u), u >= 0,
    is.numeric(v), v >= 0,
    is.numeric(time), time >= 0, time <= 1,
    is.numeric(et), et >= 0,
    is.numeric(el), el >= 0,
    is.numeric(ot), ot >= 0,
    ot_type %in% c(0, 1, 2),
    calc %in% c(-1, 0, 1, 2, 3),
    msg = "Invalid input parameters. Check the function documentation for correct usage."
  )
  
  # Optimization for large values of u and v
  if (u > 100 || v > 100) {
    return(optimize_large_values(u, v, time, lead, et, el, ot, ot_type, calc))
  }
  
  # Main calculation
  result <- tryCatch({
    calculate_probability(u, v, time, lead, et, el, ot, ot_type, calc)
  }, error = function(e) {
    stop(paste("Error in probability calculation:", e$message))
  })
  
  return(result)
}

# u = GFgA x GAgB / GFg<
# v = GAgA x GFgB / GFg<

pr_win(2.495038, 2.221673, 0, 0, 0, 0, 0, 1, 3)

#' Memoized Poisson probability mass function
poisson_pmf <- memoise(dpois)

#' Memoized Poisson cumulative distribution function
poisson_cdf <- memoise(ppois)

#' Main probability calculation function
calculate_probability <- function(u, v, time, lead, et, el, ot, ot_type, calc) {
  # Input validation
  stopifnot(
    u >= 0, v >= 0,
    time >= 0, time <= 1,
    et >= 0, el >= 0,
    ot >= 0,
    ot_type %in% c(0, 1, 2),
    calc %in% c(-1, 0, 1, 2, 3)
  )
  
  # Set U, V at rate for remainder of game
  remaining_time <- 1 - time
  u <- remaining_time * max(u, 0.00001)
  v <- remaining_time * max(v, 0.00001)
  
  # If Trailing, Switch U, V
  if (lead < 0) {
    temp <- u
    u <- v
    v <- temp
  }
  
  # Set Initial Values
  c <- abs(lead)
  pr_win <- 0
  pr_tie <- 0
  pr_win_by_1 <- 0
  pr_loss_by_1 <- 0
  pdf <- 0
  pda <- 0
  cpa <- if (c > 0) poisson_cdf(c - 1, v) else 0
  
  # Calculate Basic Probabilities
  for (i in 0:29) {
    a <- pda
    pda <- poisson_pmf(i + c, v)
    pr_loss_by_1 <- pr_loss_by_1 + pdf * pda
    pdf <- poisson_pmf(i, u)
    pr_win_by_1 <- pr_win_by_1 + pdf * a
    pr_tie <- pr_tie + pdf * pda
    pr_win <- pr_win + pdf * cpa
    cpa <- cpa + pda
  }
  
  # Restore Full Game Intensities
  u <- u / remaining_time
  v <- v / remaining_time
  
  # Endgame Goals: Some Losses Become Ties, Some Wins Become Ties
  if (et > 0) {
    uet <- u * et
    vel <- v * el
    uel <- u * el
    vet <- v * et
    a <- pr_win_by_1 * (1 - exp(-uel - vet)) * vet / (uel + vet)
    b <- pr_loss_by_1 * (1 - exp(-uet - vel)) * uet / (uet + vel)
    pr_win <- pr_win - a
    pr_tie <- pr_tie + a + b
  }
  
  # If Trailing calculations were reversed
  if (lead < 0) {
    pr_win <- 1 - pr_win - pr_tie
  }
  
  # Adjust for Overtime
  ot_w <- u / (u + v)
  a <- if (ot_type == 2) (1 - exp(-(u + v) * (1 + ot) / 12)) else ot_type
  b <- pr_tie * a
  pr_win <- pr_win + b * ot_w
  pr_tie <- pr_tie - b
  
  if (ot_type == 1) {
    b <- 0
  }
  
  # Return result based on calc parameter
  switch(calc + 2,
         1 - pr_win - pr_tie,  # Pr(Loss)
         pr_tie,               # Pr(Tie)
         pr_win,               # Pr(Win)
         2 * pr_win + pr_tie + b * (1 - ot_w),  # Expected Points
         pr_win + 0.5 * pr_tie  # Pr(Win Equivalent)
  )
}

#' Optimization for large values of u and v
optimize_large_values <- function(u, v, time, lead, et, el, ot, ot_type, calc) {
  # Implement asymptotic approximations here
  # This is a placeholder and should be replaced with actual optimizations
  warning("Using asymptotic approximation for large u or v values")
  # ... implement optimizations ...
  return(calculate_probability(u, v, time, lead, et, el, ot, ot_type, calc))
}

#' Parallel computation of pr_win for multiple parameter sets
#'
#' @param params A list of parameter sets, each a list with named elements corresponding to pr_win arguments
#' @param cores Number of cores to use for parallel computation
#'
#' @return A vector of results corresponding to each parameter set
#'
#' @export
pr_win_parallel <- function(params, cores = 2) {
  plan(multisession, workers = cores)
  results <- future_lapply(params, function(p) {
    do.call(pr_win, p)
  })
  plan(sequential)
  unlist(results)
}

#u_norm <- (Team_A_GF * Team_B_GA)/league_avg
#v_norm <- (Team_A_GA * Team_B_GF)/league_avg

# Unit tests
library(testthat)

test_that("pr_win gives expected results", {
  expect_equal(pr_win(3.0, 2.5, (10+(16/60))/60, 1, 0, 0, 0, 0, 3), 0.7459751, tolerance = 1e-7)
  expect_error(pr_win(-1, 2.0, 0.75, 1, 1.1, 0.9, 0.1, 2, 1), "Invalid input parameters")
  expect_warning(pr_win(101, 2.0, 0.75, 1, 1.1, 0.9, 0.1, 2, 1), "Using asymptotic approximation")
})

test_that("pr_win_parallel works correctly", {
  params <- list(
    list(u = 3.0, v = 2.5, time = (10+(16/60))/60, lead = 1, et = 0, el = 0, ot = 0, ot_type = 0, calc = 3),
    list(u = 3.0, v = 2.5, time = 0, lead = 0, et = 0, el = 0, ot = 0, ot_type = 0, calc = 3)
  )
  results <- pr_win_parallel(params)
  expect_equal(length(results), 2)
  expect_true(all(results >= 0 & results <= 1))
})
