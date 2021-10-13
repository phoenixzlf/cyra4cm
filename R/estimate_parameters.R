#' @title Estimate Parameters for Risk Assessment and Capital Allocation
#' @description Based on the dataset of industry cyber incidents, this functions estimates all parameters that are needed for cyber risk assessment and capital allocation.
#' @param ind.data A cyraIncidents class data.frame.
#' @param loss.dist.name A character value, specifies the name of the distribution that will be fitted to the loss data. Common choices are "lnorm" (by defauly, lognormal distribution) and "weibull" (Weibull distribution).
#' @param grid.ub.prob A numeric value, specifies the probability corresponding to the largest quantile that will be used in the process of discretizing loss distributions.
#' @param grid.num An integer value, specifies the number of grids when discretizing loss distributions.
#' @return This function returns a list of parameters with their values, which will be used for risk assessment and capital allocation.
#' @examples
#' risk_params <- generateRiskParameters(ind.data = sample_data)
#' @export prepareCyraParameters

prepareCyraParameters <- function(ind.data, loss.dist.name = "lnorm", grid.ub.prob = 0.95, grid.num = 1e3L){
  data_c <- ind.data[ind.data[, "company"], ]
  path_c <- unique(data_c[c("T", "V", "A")])

  l <- nrow(unique(data_c["T"]))
  m <- nrow(unique(data_c["V"]))
  n <- nrow(unique(data_c["A"]))
  year_min <- min(as.numeric(data_c[,"year"]))
  year_max <- max(as.numeric(data_c[,"year"]))
  lambda <- sum(table(data_c["year"]))/(year_max - year_min+1)
  lambda_ik <- split(data_c, f = c(data_c["T"], data_c["A"])) |>
    sapply(function(x) sum(table(x["year"]))/(year_max-year_min+1)) |>
    (\(x) {
      x[is.na(x)] <- 0
      x
    })() |>
    (\(x) createLambdaikTable(l,n,x))()

  loss_params <- NULL
  q_ub <- 0
  for (r in 1:nrow(path_c)) {
    i <- path_c[r, "T"]
    j <- path_c[r, "V"]
    k <- path_c[r, "A"]

    loss_data <- ind.data[(ind.data["T"] == i) &
                            (ind.data["V"] == j) &
                            (ind.data["A"] == k), "X"]
    zero_mass <- sum(loss_data == 0) / length(loss_data)
    truncated_data <- loss_data[which(loss_data != 0)] |> unique()
    params <- fitdistrplus::fitdist(truncated_data, loss.dist.name)
    q_temp <- do.call(paste0("q", loss.dist.name), c(list("p" = grid.ub.prob), params$estimate))
    if(q_temp > q_ub){
      q_ub <- q_temp
    }
    loss_params <- rbind(
      c(
        i = i,
        j = j,
        k = k,
        p0 = zero_mass,
        params$estimate
      ),
      loss_params
    ) |> as.data.frame()
  }
  q_interval <- q_ub/grid.num
  q <- generateQuantiles(q_ub, q_interval)

  p_i <- (table(data_c["T"])/nrow(data_c)) |>
    as.data.frame() |>
    (\(x) {colnames(x) <- c("i", "p");x})()

  params <- list(
    l = l,
    m = m,
    n = n,
    lambda = lambda,
    lambda_ik = lambda_ik,
    dist_name = loss.dist.name,
    dist_params_names = names(params$estimate),
    loss_params = loss_params,
    q = q,
    p_i = p_i
  )

  class(params) <- "cyraParams"
  return(params)
}


generateQuantiles <- function(ub, interval){
  q <- seq(interval, ub, interval)
  return(q)
}

createLambdaikTable <- function(l, n, la_vec){
  df <- expand.grid(1:l, 1:n)
  names(df) <- c('i', 'k')
  df['la'] <- la_vec
  return(df)
}

