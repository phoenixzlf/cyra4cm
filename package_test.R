library(cyra4cm)

attack_paths <- list(
  list("T" = 1, "V" = 1,"A" = 1, "Xzero" = 0.90, "Xdist" = "lnorm", "Xparams" = c("meanlog" = 10, "sdlog" = 5), 'prob' = 0.3),
  list("T" = 2, "V" = 2,"A" = 2, "Xzero" = 0.92, "Xdist" = "lnorm", "Xparams" = c("meanlog" = 2, "sdlog" = 10), 'prob' = 0.5),
  list("T" = 2, "V" = 3,"A" = 2, "Xzero" = 0.85, "Xdist" = "lnorm", "Xparams" = c("meanlog" = 5, "sdlog" = 1), 'prob' = 0.2)
)

lambda <- 100
years <- 2001:2018
prob_comp <- 0.1

init_th <- c(1,1,1)

sample_data <- generateSyntheticData(attack.paths = attack_paths, lambda = lambda, years = years, prob.comp = prob_comp)


params <- prepareCyraParameters(sample_data)
loss_dists <- generateLossDists(init_th, params)

inv_opt <- cyraInvestOptions(
  cyraInvestOption(1,0,1),
  cyraInvestOption(1,5E7,0.2),
  cyraInvestOption(2,0,1),
  cyraInvestOption(2,5E8,0.2),
  cyraInvestOption(3,0,1),
  cyraInvestOption(3,2E8,0.2)
)

invest_table <- createInvestTable(inv_opt)
weights <- createWeightsTable(params, rep(1, 4), rep(1,4), rep(1, 3), 1,1,1)

allo <- calcCapitalAllocation(
  invest.table = invest_table,
  cyra.weights = weights,
  cyra.params = params
)
