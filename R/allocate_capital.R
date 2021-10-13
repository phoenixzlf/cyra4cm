#' @title Develop Capital Allocation Strategy for Cyber Risk Management
#' @description This function outputs capital allocation strategies with discrete investment options according to the holistic principle
#' @export calcCapitalAllocation

# h(X)
calcPenaltyValue <- function(x, p, q){
  if((1-sum(p[q<=x])) == 0){
    return(0)
  }
  return(as.numeric(q>x)/(1-sum(p[q<=x])))
}

# E[Xh(X)]
calcConditionalExpection <- function(x, p, q){
  tail_idx <-  which(q>x)
  e <- sum(p[tail_idx] * q[tail_idx])/(1-sum(p[q<=x]))
  if(is.nan(e)) e <- 0
  return(e)
}

calcVaR <- function(alpha, mass_vec, q){
  return(
    c(0, q)[which(cumsum(mass_vec)>=alpha)][1]
  )
}

# calculate standalone optimal Kik
calcStandaloneKik <- function(penalty_thres, q, sik_mass_table, nu_om_ik, lambda_ik){
  Kik <- sik_mass_table[, c('i', 'k')]
  sik_mass <- subset(sik_mass_table, select = -c(i,k))
  Kik <- merge(Kik, nu_om_ik, by = c('i', 'k'), all = TRUE)
  Kik['Kik'] <- NA
  for(r in 1:nrow(Kik)){
    p <- unlist(sik_mass_table[r, paste0('p', c(0,q))])
    VaR <- calcVaR(alpha = penalty_thres, mass_vec = p, q = q)
    aik <- calcConditionalExpection(VaR, p, c(0,q))
    nuik <- Kik[r, 'nu']
    omik <- Kik[r, 'om']
    Kik[r,'Kik'] <- aik - nuik/(2*omik)
  }
  return(Kik)
}

# calculate optimal aggregate K
calcAggregateK <- function(penalty_thres, q, s_mass, nu_om){
  b <- calcConditionalExpection(calcVaR(alpha = penalty_thres, mass_vec = s_mass, q = q), s_mass, c(0,q))
  K <- b-nu_om$nu/(2*nu_om$om)
  return(K)
}

# calculate holistic allocation for each threat-asset pair
calcHolisticKik <- function(nu_om_ik, nu_om, Kik_bar, K_bar){
  Kik <- nu_om_ik
  denominator <- 1/nu_om$om + sum(1/nu_om_ik$om)
  Kik['Bik'] <- (1/nu_om_ik$om)/denominator
  Kik['Kik'] <- Kik_bar$Kik - Kik['Bik'] * (sum(Kik_bar$Kik) - K_bar)
  return(Kik)
}

# calculate holistic aggregate allocation
calcHolisticK <- function(nu_om_ik, nu_om, Kik_bar, K_bar){
  denominator <- 1/nu_om$om + sum(1/nu_om_ik$om)
  A <- (1/nu_om$om)/denominator
  K <- K_bar + A * (sum(Kik_bar$Kik) - K_bar)
  return(K)
}

# calculate the value of the objective function with given parameters
calcObjValue <- function(penalty_thres, q, Kik_holistic, M, nu_om, nu_om_ik, etas, sik_mass, s_mass){
  part1 <- sum(etas[etas$j != 0, 'eta', drop = TRUE] * M) + etas[etas$j==0, 'eta', drop = TRUE] * sum(M)
  part2 <- sum(Kik_holistic$Kik * Kik_holistic$nu) + nu_om$nu * sum(Kik_holistic$Kik)
  nu_om_ik['E'] <- 0
  for(r in 1:nrow(sik_mass)){
    p <- sik_mass[r, paste0('p', c(0,q))]
    h <- calcPenaltyValue(calcVaR(alpha = penalty_thres, mass_vec = unlist(p), q = q), p, c(0,q))
    quad <- (c(0,q) - Kik_holistic$Kik[which(Kik_holistic$i == sik_mass$i[r] & Kik_holistic$k == sik_mass$k[r])])^2
    e <- sum(p*h*quad)
    nu_om_ik[which(nu_om_ik$i == sik_mass$i[r] & nu_om_ik$k == sik_mass$k[r]),'E'] <- e
  }
  part3 <- sum(nu_om_ik$om * nu_om_ik$E)
  p <- s_mass
  h <- calcPenaltyValue(calcVaR(alpha = penalty_thres, mass_vec = p, q = q), p, c(0,q))
  quad <- (c(0,q) - sum(Kik_holistic$Kik))^2
  part3 <- part3+sum(p*h*quad)
  return(
    list(
      cost_tot = part1+part2+part3,
      cost_c = part1,
      cost_r = part2+part3
    )
  )
}

# calculate Bik in constrained case
calcConstrainedBik <- function(i,k, I_mat, nu_om, nu_om_ik){
  Iother <- I_mat[I_mat$i != i | I_mat$k != k, ]
  Iother <- merge(Iother, nu_om_ik, by= c('i', 'k'), all.x = TRUE)
  Tother <- subset(Iother, select = -c(i,k,nu,om)) * (1/Iother$om)
  T_sum <- colSums(Tother)
  omik <- nu_om_ik[nu_om_ik$i ==i & nu_om_ik$k ==k, ]$om
  om <- nu_om$om
  denominator <- T_sum + 1/omik + 1/om
  Bik <- (1/omik)/denominator
  return(Bik)
}

# calculate T2 for condition checking and holistic Kik in constrained case
calcConstrainedTik <- function(i,k,I_mat, Kik_bar){
  Iik <- subset(I_mat[I_mat$i == i & I_mat$k == k, ], select = -c(i,k))
  Kik <- Kik_bar[Kik_bar$i == i & Kik_bar$k == k,]$Kik
  Tik <- Iik*Kik
  return(Tik)
}

# generate a matrix for all possible constrained cases for each pair of ik
generateIMat <- function(l,n){
  comb_list <- list()
  for(i in 1:(l*n)){
    comb_list[[i]] <- c(0,1)
  }
  comb_grid <- t(expand.grid(comb_list))
  ik_grid <- expand.grid(1:l, 1:n)
  names(ik_grid) <- c('i', 'k')
  I_mat <- cbind(ik_grid, comb_grid)
  return(I_mat)
}

# find the possible cases for each ik pair
calcConstrainedIikIndex <- function(i,k,I_mat, nu_om, nu_om_ik, Kik_bar, K_bar){
  Iik <- unlist(subset(I_mat[I_mat$i == i & I_mat$k == k, ], select = -c(i,k)))
  Kik <- Kik_bar[Kik_bar$i==i & Kik_bar$k==k, ]$Kik
  Bik <- calcConstrainedBik(i, k, I_mat, nu_om, nu_om_ik)
  ik_other <- I_mat[I_mat$i != i | I_mat$k != k, c('i','k')]
  Tik_sum <- 0
  for(r in 1:nrow(ik_other)){
    Tik_sum <- Tik_sum + calcConstrainedTik(ik_other[r,]$i, ik_other[r,]$k, I_mat, Kik_bar)
  }
  condition_val <- Kik - Bik*(Tik_sum + Kik - K_bar)
  condition_bool <- as.numeric(condition_val>=0)
  return(which(condition_bool == Iik))
}

# find the feasible ik pair that meets all constraints
calcFeasibleIik <- function(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar){
  ik <- I_mat[, c('i','k')]
  Iik_sets <- list()
  for(r in 1:nrow(ik)){
    Iik_sets[[r]] <- calcConstrainedIikIndex(ik[r,]$i,ik[r,]$k,I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)
  }
  feasible_Iik_idx <- Reduce(intersect, Iik_sets)
  I_mat_noik <- subset(I_mat, select = -c(i, k))
  feasible_Iik <- cbind(ik, Iik = I_mat_noik[, feasible_Iik_idx])
  return(feasible_Iik)
}

# calculate the holistic Kik in constrained case
calcConstrainedHolisticKik <- function(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar){
  feasible_Iik <- calcFeasibleIik(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)
  holistic_Iik <- feasible_Iik
  holistic_Iik['Kik'] <- 0
  for(r in 1:nrow(feasible_Iik)){
    if(feasible_Iik[r,]$Iik != 0){
      Kik <- Kik_bar[Kik_bar$i== feasible_Iik[r,]$i & Kik_bar$k==feasible_Iik[r, ]$k, ]$Kik
      Bik <- calcConstrainedBik(feasible_Iik[r,]$i, feasible_Iik[r,]$k, feasible_Iik, nu_om, nu_om_ik)
      ik_other <- feasible_Iik[feasible_Iik$i != feasible_Iik[r,]$i | feasible_Iik$k != feasible_Iik[r,]$k, c('i','k')]
      Tik_sum <- 0
      for(s in 1:nrow(ik_other)){
        Tik_sum <- Tik_sum + calcConstrainedTik(ik_other[s,]$i, ik_other[s,]$k, feasible_Iik, Kik_bar)
      }
      holistic_Iik$Kik[r] <- unlist(Kik - Bik*(Tik_sum + Kik - K_bar))
    }
  }
  return(holistic_Iik)
}

# calculate A in constrained case
calcConstrainedA <- function(I_mat, nu_om, nu_om_ik){
  I_mat <- merge(I_mat, nu_om_ik, by= c('i', 'k'), all.x = TRUE)
  T_all <- subset(I_mat, select = -c(i,k,nu,om)) * (1/I_mat$om)
  T_sum <- colSums(T_all)
  om <- nu_om$om
  denominator <- T_sum + 1/om
  A <- (1/om)/denominator
  return(A)
}

# calculated holistic K in constrained case
calcConstrainedHolisticK <- function(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar){
  feasible_Iik <- calcFeasibleIik(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)
  ik <- feasible_Iik[, c('i','k')]
  Tik_sum <- 0
  for(s in 1:nrow(ik)){
    Tik_sum <- Tik_sum + calcConstrainedTik(ik[s,]$i, ik[s,]$k, feasible_Iik, Kik_bar)
  }
  A <- calcConstrainedA(feasible_Iik, nu_om, nu_om_ik)
  K_H <- K_bar + A*(Tik_sum - K_bar)
  return(K_H)
}

# wrapper function for calculating the allocation
calcCapitalAllocation <- function(
  invest.table,
  cyra.weights,
  cyra.params,
  scale.fun = scaleLogNormal,
  penalty_thres = 0.9
){
  # params
  l <- cyra.params$l
  m <- cyra.params$m
  n <- cyra.params$n
  p_i <- cyra.params$p_i
  q <- cyra.params$q
  loss_params <- cyra.params$loss_params
  lambda_ik <- cyra.params$lambda_ik
  lambda <- cyra.params$lambda
  dist_params_names <- cyra.params$dist_params_names
  # weights
  nu_om_ik <- cyra.weights$nu_om_ik
  nu_om <- cyra.weights$nu_om
  etas <- cyra.weights$etas
  th_table <- extractFromInvestTable(invest.table, "theta")
  M_table <- extractFromInvestTable(invest.table, "cost")
  plan_table <- th_table
  plan_table[c('cost_tot', 'cost_c', 'cost_r')] <- NA

  params_df <- prepXParams(l, m, n, loss_params, dist_params_names)
  Kik_holistic_table <- NULL
  Kik_bar_table <- NULL
  K_bar_list <- NULL
  for(r in 1:nrow(th_table)){
    th <- unlist(th_table[r,])
    M <- unlist(M_table[r,])

    x_mass <- generateXMassTable(th, params_df, cyra.params, scale.fun)
    zik_mass <- generateZikMassTable(q, th, x_mass)
    zi_mass <- generateZiMassTable(q, zik_mass)
    l_mass <- generateLMassTable(p_i, zi_mass)
    s_mass <- generateSMassTable(q, lambda, l_mass)
    sik_mass <- generateSikMassTable(q, lambda_ik, zik_mass)

    Kik_bar <- calcStandaloneKik(penalty_thres, q, sik_mass, nu_om_ik, lambda_ik)
    K_bar <- calcAggregateK(penalty_thres, q, s_mass, nu_om)
    I_mat <- generateIMat(l,n)
    Kik_holistic <- calcConstrainedHolisticKik(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)

    obj <- calcObjValue(penalty_thres, q, Kik_holistic, M, nu_om, nu_om_ik, etas, sik_mass, s_mass)
    plan_table[r, c('cost_tot', 'cost_c', 'cost_r')] <- obj
    Kik_holistic_table <- rbind(Kik_holistic_table, Kik_holistic$Kik)
    Kik_bar_table <-rbind(Kik_bar_table, Kik_bar$Kik)
    K_bar_list <- c(K_bar_list, K_bar)
  }
  colnames(Kik_holistic_table) <- paste0('K', Kik_holistic$i, Kik_holistic$k)
  colnames(Kik_bar_table) <- paste0('K', Kik_bar$i, Kik_bar$k)

  return(
    list(
      plan_table = cbind(M_table, plan_table),
      Kik_bar_table = Kik_bar_table,
      Kik_holistic_table = Kik_holistic_table,
      K_bar_list = K_bar_list
    ))
}
