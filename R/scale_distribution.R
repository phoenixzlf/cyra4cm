#' @title Scale a Random Variable
#' @description Here are two built-in functions for scaling lognormal-distributed and Weibull-distributed random variables.
#' @export scaleWeibull
#' @export scaleLogNormal

#' @describeIn scaleDist scales a lognormal-distributed random variable, i.e., \eqn{Y = \theta X}, where X follows a lognormal distribution with parameters `meanlog` and `sdlog`. The function outputs the parameters of Y.
scaleLogNormal <- function(th, meanlog, sdlog) {
  meanlog <- unname(meanlog+log(th))
  return(
    c(meanlog = meanlog, sdlog = sdlog)
  )
}
#' @describeIn scaleDist scales a Weibull-distributed random variable, i.e., \eqn{Y = \theta X}, where X follows a Weibull distribution with parameters `scale` and `shape`. The function outputs the parameters of Y.

scaleWeibull <- function(th, shape, scale) {
  scale <- unname(th * scale)
  return(
    c(shape = shape, scale = scale)
  )
}
