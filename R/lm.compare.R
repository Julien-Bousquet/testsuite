#' Compare two linear models
#'
#' Execute the tests of  `lmtest` package to compare 
#' two linear model, fitted with `lm()`.
#'
#' @param reg1 ,reg2 two  models to compare.
#' @export
lm.compare <- function(reg1, reg2, nested=c(TRUE, FALSE),
        code=TRUE, verbose=TRUE, conf.level=0.95){ 

  LIST <- list() #list of pvalues  
  regname1 <- deparse1(substitute(reg1))
  regname2 <- deparse1(substitute(reg2))

  if(code)cat('library(lmtest)\n')
 if(nested){
  #Test of Wald  between two  models
  if(verbose) intro("The Wald Test")
  tryCatch(
    expr = { 
      LIST$Wald <-  lmtest::waldtest(reg1, reg2)["Pr(>F)"][2,1]
      if(verbose){
        message("The null hypothesis of a Wald test is that models are equivalent. Same search as Likelihood Ratio Test.")
        warns("Wald", ">",   LIST$Wald, conf.level)
      }
      if(code)cat("waldtest(",regname1,",",regname2  ,")")
    },
    error = function(e){
      print(e)
      message(paste("lmtest::waldtest() send an error, and do not apply in this situation. \n Are the two models nested?
\n\n lrtest( ) is merging the two models for", regname1," to be nested in ",regname1,'+',regname2, '.\n
You may use nested=FALSE') )
    },
    warning = function(w){
      print(w)
      message("lmtest::waldtest() sens a warning.") 
    }
  ) #end of Wald tryCatch( )


  #Likelihood ratio Test -- same idea than Wald
  if(verbose) intro("Likelihood ratio test")
  tryCatch(
    expr = { 
      LIST$likelihood <-  lmtest::lrtest(reg1, reg2)["Pr(>Chisq)"][2,1]
      if(verbose){
         cat("Compare ", regname1, " with the model (",regname1,'+',regname2,").
Null hypothesis is that models are equivalent. Same search as Wald Test.
Usually obtained with function anova(",regname1,', ',regname2,").\n")
        warns('Likelihood ratio', '>',   LIST$likelihood, conf.level)
     }
        if(code)cat("lrtest(",regname1,",", regname2,")\n")
    },
    error=function(e){
      message("lmtest::lrtest( ) send an error, and do not apply in this situation. \n") 
    },
    warning = function(w){
        print(w)
        message("lmtest::lrtest() sends a warning.") 
    }
  ) #end of Likelihood ratio test tryCatch( )
#end if(nested)
 } else { #beginning of !nested


  LIST$encomp <- lmtest::encomptest(reg1,reg2)
  if(verbose){
    part('Non-nested model comparison')
    intro("The Encompassing test  ")
    message("Fits a model E with every regressor from both model.
A Wald test compare separatedly each of the model with the encompassing
one : E. The null hypothesis is that every model is equivalent to E.
Two p-values  :")
     warns(paste('Encompassing model vs ', regname1), '>', LIST$encomp[1,4] , conf.level=conf.level)       
     warns(paste('Encompassing model vs ', regname2), '>', LIST$encomp[2,4] , conf.level=conf.level)       
  }
  if(code)message(paste("encomptest(",regname1,", ",regname2,')' ))


  LIST$Cox <- lmtest::coxtest(reg1,reg2)
  if(verbose){
    intro("The Cox test")
    message("The null hypothesis is that the first model contains the correct
set of regressors, then a fit of regressors form the other
model to the fitted values from first model should have
no further explanatory power. Two p-values, inverting model roles :")
     warns(paste('Cox model 2 vs fitted', regname1), '>', LIST$Cox[1,4] , conf.level=conf.level)       
     warns(paste('Cox model 1 vs fitted', regname2), '>', LIST$Cox[2,4] , conf.level=conf.level)       
  }
  if(code)message(paste("coxtest(",regname1,", ",regname2,')' ))


  LIST$j <- lmtest::jtest(reg1,reg2)
  if(verbose){
    intro("The J test")
    message("The null hypothesis is that the first model contains the correct
set of regressors, then including the fitted values of the second model into
the set of regressors should provide no improvement.
 Two p-values, inverting model roles :")
     warns(paste('J test model 1 and values of fitted', regname1), '>', LIST$Cox[1,4] , conf.level=conf.level)       
     warns(paste('J test model 2 and values of fitted', regname2), '>', LIST$Cox[2,4] , conf.level=conf.level)       
  }
  if(code)message(paste("jtest(",regname1,", ",regname2,')' ))
 }#end of !nested
 return(LIST)
} 