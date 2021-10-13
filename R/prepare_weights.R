#' @title Prepare Weights for Capital Allocation
#' @description This function can be used to format the user-provided weights into data.frames that are compatible with the risk assessment and capital allocation framework.
#'
#' @param l,m,n integer values. l: number of threats; m: number of vulnerabilities; n: number of assets.
#' @param nu.ik,om.ik numeric vectors specifying standalone reserve weights, nu_ik and omega_ik.
#' @param nu,om numeric values specifying aggregate reserve weights, nu and omega.
#' @param eta.j a numeric vector specifying standalone cybersecurity investment weights.
#' @param eta a numeric value specifying aggregate cybersecurity investment weights.
#' @export createWeightsTables

createWeightsTables <- function(cyra.params, nu.ik, om.ik, eta.j, nu, om, eta) {
  l <- cyra.params$l
  m <- cyra.params$m
  n <- cyra.params$n
  nu_om_ik <- createNuOmegaikTable(l, n, nu.ik, om.ik)
  nu_om <- createNuOmegaTable(nu, om)
  etas <- createEtaTable(m, eta.j, eta)
  weights <- list(
    nu_om_ik = nu_om_ik,
    nu_om = nu_om,
    etas = etas
  )
  class(weights) <- "cyraWeights"
  return(
    weights
  )
}


#' @describeIn createWeightsTables create a table for standalone reserve weights, nu_ik and omega_ik
createNuOmegaikTable <- function(l, n, nu.ik, om.ik) {
  df <- expand.grid(1:l, 1:n)
  names(df) <- c("i", "k")
  df["nu"] <- nu.ik
  df["om"] <- om.ik
  return(df)
}

#' @describeIn createWeightsTables create a compatible table for investment weights, eta
createEtaTable <- function(m, eta.j, eta) {
  df <- data.frame(j = c(1:m, 0), eta = c(eta.j, eta))
  return(df)
}

#' @describeIn createWeightsTables create a compatible table for aggregate reserve weights, nu and omega
createNuOmegaTable <- function(nu, om) {
  return(
    data.frame(
      nu = nu,
      om = om
    )
  )
}
