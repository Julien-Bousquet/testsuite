#' A function that transforms pvalues in stars ***
#'
#' @author Julien Bousquet (2021)
#' @param p.val numeric, the value of the pvalue.
#' @param probs a vector of the four probabilities of changes in increasing order.
#' @return A character string with stars, dot or blank space.
#'
#' @examples star(0.015)
#' #for a vector of p-values
#' pvals <- c(0.01, 0.05, 0.015, 0.5)
#' sapply(pvals, star)
#' @export
star <- function(p.val, probs=c(0.001, 0.01, 0.05, 0.1))UseMethod("star", p.val)

#' @export
star.default <- function(p.val, probs=c(0.001, 0.01, 0.05, 0.1)){ 
  p.val <- abs(p.val)
  #Check probs vector
  if(length(probs)!=4)stop("Vector probs must have 4 values.")
  if(min(probs)<0 | max(probs>1))stop("Probabilities of probs must be between 0 and 1.")
  if(sum(rank(probs)==1:4)!=4)stop("Probabilities of probs must be in increasing order.")

 #If p.val is a vector, use sapply()
  if(length(p.val)>1)return(sapply(p.val, star))

  #Convert p.value to stars and return
  if(p.val>probs[4])return(' ')
  if(p.val>probs[3])return('.')
  if(p.val>probs[2])return('*')
  if(p.val>probs[1])return('**')
  return('***')
}

#' Get stars for a linear model
#' @param p.val a linear model
#' @details Use the function glance from package broom to get the p.value of
#' the linear model, and then convert it into stars with function star().
#' @export
star.lm <- function(p.val, ...){
  #p.val is a lm
  return(star(broom::glance(LM)['p.value']))
}