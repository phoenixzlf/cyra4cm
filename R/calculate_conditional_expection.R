#' @title Calculate Conditional Expectation
#' @description This function calculates the conditional expectation of \eqn{X}, when the Value-at-Risk of \eqn{X} is given
#' @param x a numeric value representing the Value-at-Risk of random variable \eqn{X}
#' @param p a numeric vector of probabilities of the distribution of \eqn{X}
#' @param q a numeric vector of quantiles corresponding to `p`
#' @export

calcConditionalExpection <- function(x, p, q) {
  tail_idx <- which(q > x)
  e <- sum(p[tail_idx] * q[tail_idx]) / (1 - sum(p[q <= x]))
  if (is.nan(e)) e <- 0
  return(e)
}
