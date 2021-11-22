#' @title Weighted Penalty Function - CTE
#' @description \loadmathjax This is a the weighted penalty function defined as follows, \mjseqn{\frac{1_{\{X > x\}}}{\mathbb{P}(X > x)}}.
#' @param x a numeric value representing the parameter \eqn{x} in the weighted penalty function.
#' @param p a numeric vector that represents the probabilities of the loss distribution.
#' @param q a numeric vector that has the same length as `p` and represents the quantiles corresponding to `p`.
#'
#' @return a numeric value representing the penalty
#' @export calcPenaltyValue.CTE

calcPenaltyValue.CTE <- function(x, p, q) {
  if ((1 - sum(p[q <= x])) == 0) {
    return(0)
  }
  return(as.numeric(q > x) / (1 - sum(p[q <= x])))
}
