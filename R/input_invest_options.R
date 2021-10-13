#' @title Create Cyber Control Investment Table with Discrete Investment Options
#' @description This function
#' @export createInvestTable
#' @export cyraInvestOptions
#' @export cyraInvestOption

createInvestTable <- function(inv.opt){
  if (class(inv.opt) != "cyraInvestOptions") stop("inv.opt must be of class cyraInvestOptions")
  tb <- do.call(rbind, inv.opt) |> as.data.frame()
  tb_by_idx <- split(tb, f=tb["idx"]) |> lapply(function(x) {
    subx <- subset(x, select = -idx)
    return(split(subx, f = 1:nrow(subx)))
  })
  invest_tb <- expand.grid(tb_by_idx)

  class(invest_tb) <- append(class(invest_tb), "cyraInvestTable")
  return(invest_tb)
}

cyraInvestOptions <- function(...){
  option_list <- list(...)
  class(option_list) <- "cyraInvestOptions"
  return(option_list)
}

cyraInvestOption <- function(idx, cost, theta){
  option <- c(
    idx = idx, cost = cost, theta = theta
  )
  class(option) <- "cyraInvestOption"
  return(option)
}

extractFromInvestTable <- function(invest.table, field){
  th_table <- data.frame()
  for (i in 1:nrow(invest.table)) {
    th_row <- sapply(1:ncol(invest_table), function(x) invest_table[[i, x]][[field]])
    th_table <- rbind(th_table, th_row)
  }
  rownames(th_table) <- NULL
  colnames(th_table) <- paste0(field, 1:ncol(invest.table))
  return(th_table)
}
