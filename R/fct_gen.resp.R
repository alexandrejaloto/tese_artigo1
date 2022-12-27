source('R/fct_calc.prob.R')

gen.resp <- function(theta, bank)
{
  p <- lapply(theta, calc.prob, bank = bank)

  p <- do.call(rbind, p)

  resp <- apply(p > runif(length(theta)*nrow(bank)), 2, as.numeric)

  return(resp)

}

