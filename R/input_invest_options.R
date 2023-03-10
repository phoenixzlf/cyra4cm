#' @title Create Cyber Control Investment Table with Discrete Investment Options
#' @description Assume that there are discrete investment options on individual cybersecurity vulnerabilities, these functions help populates all possible combinations of investment decisions on individual vulnerabilities.
#' @param inv.opt a list of class `cyraInvestOptions` generated by function [cyraInvestOptions()].
#'
#' @export createInvestTable
#' @export cyraInvestOptions
#' @export cyraInvestOption
#' @export extractFromInvestTable

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

#' @describeIn createInvestTable this function creates a list of class `cyraInvestOptions`, which is used in function [createInvestTable()].
#' @param ... objects of class `cyraInvestOption`
cyraInvestOptions <- function(...){
  option_list <- list(...)
  class(option_list) <- "cyraInvestOptions"
  return(option_list)
}

#' @describeIn createInvestTable this function creates an individual `cyraInvestOption` item.
#' @param idx an integer indicating the index of the vulnerability that this investment option is for.
#' @param invest a numeric value representing the amount spent on this vulerability
#' @param theta a numeric value between 0 and 1, indicating the remaining vulnerability after the investment is made.

cyraInvestOption <- function(idx, invest, theta){
  option <- c(
    idx = idx, invest = invest, theta = theta
  )
  class(option) <- "cyraInvestOption"
  return(option)
}

#' @describeIn createInvestTable this is a helper function that extracts `invest` or `theta` information from a `cyraInvestTable` object (see [createInvestTable()]).
#' @param invest.table a `cyraInvestTable` object.
#' @param field a character value, of which the value is either `invest` or `theta`


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
