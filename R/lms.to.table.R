#' Make a table to summary coefficients of a list of linear models
#' 
#' @author Julien Bousquet (2021)
#' @param x vector of names of the columns to test in data, or dataframe of the vectors to test, or  a name of data column
#'
#' @return a dataframe with all the terms of the different linear models and their coefficient.
#' The minus sign - is set for absence of the term in the model
#'  
#' @details The different linear models must be computed separetely, and then 
#' pass to the argument in a list format, using function list().
#'
#' @examples # Simple example
#' 
#'
#' @export
lms.to.table <- function(L){
  DF <- data.frame(x=0)
  foreach(reg=L)%do%{
    #find terms of different lm, and place them as name in DF first column

  }
  foreach(reg=L, packages='broom')%do%{
    #write coeffs of different lm, and place them in DF on the row of their terms 
	GLANCE <- broom::glance(reg) #get main informations from reg
	adj.r.squared <- GLANCE$`adj.r.squared`
	pval <- GLANCE$`p.value`
  }
  return(DF)
}
