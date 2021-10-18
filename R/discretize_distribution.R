#' @title  Discretize Continuous Loss Distributions
#' @description This function discretizes a continuous distribution by allocating the probability within each interval to the beginning of that interval
#' @param q a numeric vector of quantiles, for which the discretized probabilites are calculated
#' @param pdist a character value, indicating the name of the continuous distribution, e.g., "lnorm", "weibull", etc.
#' @param ... a named numeric vector of parameters of the distribution.
#'
#' @return a numeric vector of probabilities that corresponds to the input quantiles `q`.
#' @export

generateDiscretizedProbs <- function(q, pdist, ...) {
  p_tmp1 <- c(0, do.call(pdist, c(list(q = q), ...)))
  p_tmp2 <- c(do.call(pdist, c(list(q = q), ...)), 0)
  p <- (p_tmp2 - p_tmp1)[1:(length(p_tmp2 - p_tmp1) - 1)]
  return(p)
}
