#' @title Aggregate Loss Distribution Functions
#' @description The function generate a discretized version of the aggregate loss (S) distribution and distributions of losses on individual threat-asset pairs (S_{ik}).
#'
#'
#' @export generateLossDists


generateLossDists <- function(th, cyra.params, scale.fun = scaleLogNormal) {
  if(class(cyra.params) != "cyraParams"){
    stop("cyra.params has to be a list of class cyraParams")
  }
  params_df <- prepXParams(cyra.params$l, cyra.params$m, cyra.params$n, cyra.params$loss_params, cyra.params$dist_params_names)
  x_mass <- generateXMassTable(th, params_df, cyra.params, scale.fun)
  zik_mass <- generateZikMassTable(cyra.params$q, th, x_mass)
  zi_mass <- generateZiMassTable(cyra.params$q, zik_mass)
  l_mass <- generateLMassTable(cyra.params$p_i, zi_mass)
  s_mass <- generateSMassTable(cyra.params$q, cyra.params$lambda, l_mass)
  sik_mass <- generateSikMassTable(cyra.params$q, cyra.params$lambda_ik, zik_mass)

  out <- list(
    X_dist = x_mass,
    Zik_dist = zik_mass,
    Zi_dist = zi_mass,
    L_dist = l_mass,
    S_dist = s_mass,
    Sik_dist = sik_mass
  )
}

generatePanjerMass <- function(lambda, mass_vec){
  p_0 <- dpois(0, lambda)
  ## The (a, b, 0) class
  a <- 0
  b <- lambda
  ## Recursion
  len <- length(mass_vec)
  s <- numeric(len)
  s[1] = p_0 * exp(mass_vec[1] * b)
  tmp = mass_vec[2:len]

  for (k in 1:(len-1)) {
    coef_1 <- a + b*seq(1, k, 1) / k
    coef_2 <- tmp[1:k]
    coef_3 <- s[k:1]
    s[k+1] <- sum(coef_1 * coef_2 * coef_3)
  }

  return(s)
}

generateSMassTable <- function(q, lambda, l_mass_table){
  s_mass <- generatePanjerMass(lambda, unlist(l_mass_table))
  names(s_mass) <- paste0("p", c(0, q))
  s_mass <- t(as.data.frame(s_mass))
  return(s_mass)
}

generateSikMassTable <- function(q, lambda_ik, zik_mass_table){
  df <- merge(zik_mass_table, lambda_ik, by = c('i', 'k'), all = TRUE)
  sik_mass <- data.frame()
  for(r in 1:nrow(df)){
    sik_mass[r, c('i', 'k')] <- df[r, c('i', 'k')]
    sik_mass[r, paste0('p', c(0,q))] <- generatePanjerMass(df$la[r], unlist(zik_mass_table[r, paste0('p', c(0,q))]))
  }
  return(sik_mass)
}


generateXMassTable <- function(th, params.df, cyra.params, scale.fun){
  p_names <- paste0('p', cyra.params$q)
  params.df[p_names] <- NA
  for(i in 1:nrow(params.df)){
    if((!is.na(params.df[i,5]) & (th[params.df[i,'j']] != 0))){
      X <- unlist(params.df[i,])
      new_params <- do.call(scale.fun, c(list(th = th[params.df[i,'j']]), params.df[i,cyra.params$dist_params_names]))
      params.df[i, p_names] <- (1-X['p0'])*do.call(generateDiscretizedProbs, c(list(q = cyra.params$q, pdist = paste0("p", cyra.params$dist_name), new_params)))
    } else {
      params.df[i, p_names] <- rep(0, length(cyra.params$q))
      params.df[i, 'p0'] <- 1
    }
  }

  return(params.df)
}

convolveDiscreteRV <- function(q, p1, p2){
  q <- c(0,q)
  n <- length(q)
  p <- numeric(n)
  for (i in 1:n){
    p[i] <- sum(p1[1:i] * p2[i:1])
  }
  return(p)
}

generateEachZikMass <- function(q, pair_table){
  pair_p <- pair_table[, paste0('p', c(0,q))]
  pair_p_each <- split(as.matrix(pair_p), f=1:nrow(pair_p))
  z_mass <- Reduce(function(x, y) convolveDiscreteRV(q,x,y), pair_p_each)
  names(z_mass) <- paste0('p', c(0,q))
  z_mass <- data.frame(t(z_mass))
  z_mass['i'] <- pair_table[1, 'i']
  z_mass['k'] <- pair_table[1, 'k']
  return(z_mass)
}

generateZikMassTable <- function(q, th, x_mass_table){
  full_ctrl_idx <- which(th==0) # this needs to be changed if control takes values other than 0 and 1
  x_mass_w_ctrl <- x_mass_table[! x_mass_table$j %in% full_ctrl_idx, ]
  if(nrow(x_mass_w_ctrl) == 0 ){
    zik_table <- unique(x_mass_table[,c('i', 'k')])
    zik_table['p0'] <- 1
    zik_table[paste0('p', q)] <- 0
    return(zik_table)
  }

  ik_pairs <- split(x_mass_w_ctrl, f=list(x_mass_w_ctrl$i, x_mass_w_ctrl$k), drop = TRUE)
  zik_list <- lapply(ik_pairs, generateEachZikMass, q=q)
  zik_table <- do.call('rbind', zik_list)
  return(zik_table)
}

generateZiMassTable <- function(q, z_mass_table){
  zis <- split(z_mass_table, f=z_mass_table$i)
  zi_list <- lapply(zis, function(zi){
    p <- zi[, paste0('p', c(0, q))]
    p_each <- split(as.matrix(p), f=1:nrow(p))
    zi_mass <- Reduce(function(x,y) convolveDiscreteRV(q,x,y), p_each)
    names(zi_mass) <- paste0('p', c(0,q))
    zi_mass <- data.frame(t(zi_mass))
    zi_mass['i'] <- zi[1, 'i']
    return(zi_mass)
  })
  zi_table <- do.call('rbind', zi_list)
  return(zi_table)
}

generateLMassTable <- function(p_i, zi_table){
  all_i <- p_i$i
  l <- p_i[p_i$i==all_i[1],]$p * zi_table[zi_table$i==all_i[1],]
  for(i in all_i[2:length(all_i)]){
    l <- l+p_i[p_i$i==i,]$p * zi_table[zi_table$i==i,]
  }
  l <- subset(l, select = -i)
  return(l)
}

prepXParams <- function(l,m,n, loss.params, dist.params.names){
  params_df <- expand.grid(1:l, 1:m, 1:n)
  colnames(params_df) <- c('i', 'j', 'k')
  params_df['p0'] <- NA
  params_df[dist.params.names] <- NA
  params_df <- merge(params_df, loss.params, by = c('i', 'j', 'k'), all = TRUE, suffixes = c('drop',''))
  params_df[paste0(c(dist.params.names,'p0'), 'drop')] <- NULL
  params_df[which(is.na(params_df$p0)), 'p0'] <- 1
  return(params_df)
}

