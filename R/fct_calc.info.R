source('R/fct_calc.prob.R')

calc.info <- function(bank, theta)
{

  # probability of answering correctly
  p <- calc.prob(theta, bank, u = 1)

  # parameters
  a <- bank[,1]
  b <- bank[,2]
  c <- bank[,3]

  # information
  info <- a^2*((p - c)^2/(1 - c)^2)*((1 - p)/p)

  return(info)
}
