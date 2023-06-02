#' Clean factors 
#'
#' `cleanFactor()` check that the factor given in argument
#' allows the application of statistical test.
#' 
#' @author Julien Bousquet (2023)
#' @param x  a vector of values, corresponding to g. 
#' @param g  a vector of categories, which must be checked.
#' @return Give comprehensive error messages. 
#' Return a list with x and g, cleaned if possible.
#' @examples
#' cleanFactor( 1:6, factor(rep(c('a', 'b', NA), times=2) ))
#' @export
cleanFactor <- function(x, g){
  
   g <- as.factor(g)
   T <- table(g) 

  #Length of vectors
   if(length(x)!=length(g)) stop('x and g must have same length')
   if(length(x)==0) stop('x or g is empty')
   if(length(levels(g))<=1) stop('Not enough levels in g. See levels(g)')

  #Remove na
  na.idx <- c(which(is.na(x)), which( is.na(g) ) )
  if(length( na.idx) >0) {
    warning('NA omited.')
    x <- x[-na.idx]
    g <- g[-na.idx]
    return(cleanFactor(x,g))
  }

  #Remove empty groups 
  emptyLevels <- levels(g)[T ==0 ]
  if( length(emptyLevels) > 0) { 
     warning(paste('Group', emptyLevels, 'is removed, because empty.\n'))
     g <- factor(g, names(T)[T>0])
     return(cleanFactor(x,g))
  }

  #Only one data groups
  oneLevels <- levels(g)[T == 1 ]
  if( length(oneLevels) > 0) { 
     warning(paste('Group', oneLevels, 'is removed, because only one record.\n'))
     x <- x[ g %in% names(T)[T>1] ] 
     g <- factor(g[ g %in% names(T)[T>1]], names(T)[T>1])
     return(cleanFactor(x,g))
  }

  #Constant groups 
  std <- as.vector(by(x, g, sd, na.rm=TRUE)) 
  names(std) <- levels(g)
  if( sum(std==0) >0){
    x <- x[ g %in% names(std)[std>0]]
    g <- factor(g[ g %in% names(std)[std>0]] )
    warning(paste(' Group', names(std)[std==0], 'is omitted, because constant.\n'))
    return(cleanFactor(x,g))
  }
 
  list(x=x, g=g)

}


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

#' Write a part title  
#'
#' @author Julien Bousquet (2023)
#' @param x  a string to write
part <- function(x)    message('\n\n                           ',x)

#' Write an intro text  
#'
#' @author Julien Bousquet (2023)
#' @param x  a string to write
intro <- function(x)message('\n\n------------------   ',x,'   ------------------')

#' Write a warning about the result of a test
#'
#' Use color to emphasis the result of the test.
#' @author Julien Bousquet (2023)
#' @param name the name of the test.
#' @param pval a numeric, between 0 and 1 : the resulting p-value of a test. 
#' @param signe one character '>' or '<' giving the expected
#' @param conf.level  level of confidence for decision.
#' position of `pval`.
#' expected way for the p-value to be 
warns <-  function(name, signe=c('<','>'), pval, conf.level=0.95) {
    #signe is the aim we hope for. If not : warning
     phrase <- paste0(
            'The p-value of the ',name,'  test is ', signif(pval,4), 
    '\nThe p-value should be ', signe, 1-conf.level)
     if( signe=='<')    phrase <- paste(phrase, ifelse(  pval>1-conf.level, crayon::red("WARNING"), crayon::green("OK")))
     if( signe=='>')    phrase <- paste(phrase, ifelse(  pval<1-conf.level, crayon::red("WARNING"), crayon::green("OK"))) 
     message(phrase)
  }
