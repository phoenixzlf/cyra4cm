#' @title Built-in Weight Normalizers
#' @description These functions are possible choices of the normalizer of each expected deviance term.
#' @param term.pt either a numerical value or the character value `"TVaR"` (default). This is the terminal point such that the l1-norm of the quadratic function and that of the linear function are equal with in [0, `term.pt`]. If `term.pt = "TVaR"`, then the value of the terminal point is the TVaR of the loss.  This argument is needed only for function `calcNormalizer.l1norm`.
#' @param h.func a weighted penalty function. See [calcPenaltyValue.CTE()].
#' @param p a numeric vector representing the probabilities of the loss random variable in the expected deviance.
#' @param q a numeric vector that has the same length as `p` and represent the quantiles corresponding to `p`.
#' @param ... additional needed arguments in `h.func`.
#' @return a numeric value of the calculated normalizer.
#' @export calcNormalizer.l1norm
#' @export calcNormalizer.local
#' @export calcNormalizer.cte

#' @describeIn calcNormalizer This function calculates the normalizer that equates the l1-norm of the expected deviance to that of the linear reserve term, up to a terminal point.
calcNormalizer.l1norm <- function(term.pt = "TVaR", h.func, p, q, ...) {
  if(term.pt == "TVaR"){
    term.pt <- calcConditionalExpection(p = p, q = q, ...)
  }
  n <- sum(((q^2 - q * (term.pt) + (term.pt^2) / 3)) * h.func(p = p, q = q, ...) * p) / ((term.pt) / 2)
  return(n)
}

#' @describeIn calcNormalizer This function calculates the normalizer that equates the expected deviance to the linear reserve term at the point where the expected deviance is minimized.
calcNormalizer.local <- function(h.func, p, q, ...) {
  h <- h.func(p = p, q = q, ...)
  K_ref <- calcConditionalExpection(p = p, q = q, ...)
  n <- sum(((q-K_ref)^2)*p*h)/K_ref
  return(n)
}

#' @describeIn calcNormalizer This function calculates the normalizer that equates the average marginal decrease in expected deviance to the (marginal) increase in linear reserve term within the interval from no reserve to the amount that minimizes the expected deviance. In that case, the normalizer takes the value of the CTE of the loss random varible.

calcNormalizer.cte <- function(p, q, ...){
  n <- calcConditionalExpection(p = p, q = q, ...)
  return(n)
}
