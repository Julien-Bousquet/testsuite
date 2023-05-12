#' Clean factors 
#'
#' `cleanFactor()` check that the factor given in argument
#' allows the application of statistical test.
#' 
#' @author Julien Bousquet (2023)
#' @param x  a vector of values, corresponding to g. 
#' @param g  a vector of categories, which must be checked.
#' @ return Give comprehensive error messages. 
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
