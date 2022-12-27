calc.prob <- function(theta, bank, u = 1)
{
  a <- bank[,1]
  b <- bank[,2]
  c <- bank[,3]
  p <- c + (1 - c)*(exp(a*(theta-b)))/(1 + exp(a*(theta-b)))

  u <- as.numeric(u)

  p <- p^u*(1-p)^(1-u)

  return(p)
}
