#' @title Calculate Value-at-Risk of a Discretized Distribution
#' @description This function calculates the Value-at-Risk a discretized version of \eqn{X}
#' @param alpha a numeric value representing the probability corresponding to the VaR to be found
#' @param p a numeric vector of probabilities of the distribution of \eqn{X}
#' @param q a numeric vector of quantiles corresponding to `p`
#' @export

calcVaR <- function(alpha, p, q) {
  return(
    c(0, q)[which(cumsum(p) >= alpha)][1]
  )
}
