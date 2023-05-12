#' A function that transforms pvalues in stars ***
#'
#' @author Julien Bousquet (2021)
#' @sources Rainforest Action Network, BankTrack, Indigenous Environmental Network, Oilchange
#' International, ReclaimFinance et SierraClub : "Banking on Climate Change : Fossile Fuel
#' Finance Report 2020 "
#'
#' @examples star(0.015)
#' #for a vector of p-values
#' pvals <- c(0.01, 0.05, 0.015, 0.5)
#' sapply(pvals, star)

