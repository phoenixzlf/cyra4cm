#' @title Generate Synthetic Industry Cyber Incident Data
#'
#' @description This function generates synthetic industry cyber incident data by simulation. The generated dataset is compatible with the risk assessment and capital allocation framework proposed by Chong, et al. and is for demonstration and testing purposes only.
#'
#' @usage generateSyntheticData(attack.paths, lambda, years, prob.com, seed = 199)
#' @param attack.paths a list of attack path information, see `Details`.
#' @param lambda a number, which is parameter of the Poisson distribution used for generating yearly number of incidents in this industry.
#' @param years a numeric vector that represents the time span of the sample.
#' @param prob.comp a number, which represents probability of the incident occurring to the company of interest.
#' @param seed a number, seed of random number generator.
#' @return This function returns a cyraIncidents class data.frame of synthetic cyber incidents. Its structure is compatible with the risks assessment and capital allocation framework in this package.
#' @details To be added
#' @examples
#' # create attack paths information
#' path_info <- list(
#' list("T" = 1, "V" = 1,"A" = 1, "Xzero" = 0.90, "Xdist" = "lnorm", "Xparams" = c("meanlog" = 10, "sdlog" = 5), 'prob' = 0.3),
#' list("T" = 2, "V" = 2,"A" = 2, "Xzero" = 0.92, "Xdist" = "lnorm", "Xparams" = c("meanlog" = 2, "sdlog" = 10), 'prob' = 0.5),
#' list("T" = 2, "V" = 3,"A" = 2, "Xzero" = 0.85, "Xdist" = "lnorm", "Xparams" = c("meanlog" = 5, "sdlog" = 1), 'prob' = 0.2)
#' )
#' lambda <- 100
#' years <- 2001:2008
#' prob_comp <- 0.1
#' sample_data <- generateSyntheticData(path_info = path_info, lambda = lambda, years = years, prob_comp = prob_comp)
#' @export


generateSyntheticData <- function(attack.paths, lambda, years, prob.comp, seed = 199) {
  set.seed(seed)
  sample_data <- data.frame()
  prob_path <- sapply(attack.paths, "[[", "prob")
  for (y in years) {
    N_iy <- rpois(1, lambda = lambda)
    sample_iy <- data.frame(
      "T" = numeric(N_iy),
      "V" = numeric(N_iy),
      "A" = numeric(N_iy),
      "X" = numeric(N_iy),
      "company" = logical(N_iy),
      "year" = y
    )
    idx_sample <- sample(1:length(attack.paths), N_iy, replace = TRUE, prob = prob_path)
    for (i in 1:N_iy) {
      path <- idx_sample[i]
      sample_iy[i, c("T", "V", "A")] <- attack.paths[[path]][c("T", "V", "A")]
      if (runif(1) > attack.paths[[path]][["Xzero"]]) {
        sample_iy[i, "X"] <- do.call(paste0("r", attack.paths[[path]][["Xdist"]]), c(list(n = 1), attack.paths[[path]][["Xparams"]]))
      } else {
        sample_iy[i, "X"] <- 0
      }
      if (runif(1) > prob.comp) {
        sample_iy[i, "company"] <- FALSE
      } else {
        sample_iy[i, "company"] <- TRUE
      }
    }
    sample_data <- rbind(sample_data, sample_iy)
  }
  return(sample_data)
}
