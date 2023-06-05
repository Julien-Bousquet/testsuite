#' Execute the tests of  `lmtest` package to validate 
#' a fit of linear model with `lm()`. 
#' @param reg the result of a linear model, obtained with `lm()`.
#' @param code boolean. If TRUE, write the code to replay, in the response.
#' @param verbose boolean. If TRUE, write a short remainder comments about
#' the interpretation of each test. 
#' @param order.by  a vector z. The
#' observations in the model are ordered by the size of z. 
#' If set to NULL (the default), the observations are 
#' assumed to be ordered (e.g., a time series).
#' @export
lm.test <- function (reg,  order.by=NULL, 
      code=TRUE, verbose=TRUE, conf.level=0.95) {
  #Answers :
  LIST <- list() #list of pvalues
  regname <- deparse1(substitute(reg))
  ordername <- deparse1(substitute(order.by))

  if(code)message('library(lmtest)')

  #Test of Wald  anova against null model, Wald test
  LIST$Wald <-  lmtest::waldtest(reg)["Pr(>F)"][2,1]
  if(verbose){
    part("Validation")
    intro("The Wald Test")
    message("The null hypothesis of a Wald test is that all the coefficient 
of the model are equal to zero. Same search as Likelihood Ratio Test. ")
     warns('Wald', '<',   LIST$Wald, conf.level=conf.level)
  }
  if(code)cat("waldtest(",regname,")")


  #Likelihood ratio Test -- same idea than Wald
  LIST$likelihood <-  lmtest::lrtest(reg)["Pr(>Chisq)"][2,1]
  if(verbose){
    intro("Likelihood ratio test")
    message("Compare the fitted model with the null model. 
Null hypothesis is that there is no difference of quality in the fitting. ")
     warns('Likelihood ratio', '<',   LIST$likelihood, conf.level=conf.level)
  }
  if(code)cat("lrtest(",regname,")")



  #Normality of residuals
  LIST$Shapiro <- shapiro.test(residuals(reg))$p.value
  if(verbose){
    intro("The Shapiro-Wilk test on residuals")
    message("The null hypothesis of a Shapiro-Wilk test is that the
residuals of the model are normally distributed.")
     warns('Shapiro', '>',   LIST$Shapiro, conf.level=conf.level )       
  }
  if(code)message(paste("shapiro.test(residuals(",regname,"))"))
  
# Rainbow test
  LIST$Rainbow <- lmtest::raintest(reg , fraction = 0.7, order.by='mahalanobis')$p.value
  if(verbose){
    intro('The rainbow test')
    message('The null hypothesis of a Rainbow test is that this model is identical if 
fitted on a subset of the "best" individuals (those closests of prediction).')
     warns('Rainbow', '>', LIST$Rainbow , conf.level=conf.level)       
  }
  if(code)message(paste("raintest(",regname,", fraction = 0.7, order.by='mahalanobis')"))

# Heteroskedasticity
  LIST$Durbin <- lmtest::dwtest(reg , order.by=order.by)$p.value
  if(verbose){
    part('Search of heteroskedasticity')
    intro('The Durbin Watson test on residuals')
    message("The Durbin Watson test checks the presence of autocorrelation
in the residuals of the model. Its null hypothesis assumes 
there is no autocorrelation.
This test use the order.by= argument to define autocorrelation.")
     warns('Durbin Watson', '>', LIST$Durbin , conf.level=conf.level )       
  }
  if(code)message(paste("dwtest(",regname,", order.by=",ordername,")"))

  #Breusch Godfrey
  LIST$Breusch <- lmtest::bgtest(reg , order.by=order.by, order=3)$p.value
  if(verbose){
    intro('The Breusch-Godfrey test on residuals')
    message("The Breusch-Godfrey test is an extension of 
the Durbin Watson test, that checks the presence of autocorrelation
in the residuals of the model, at higher orders. Its null hypothesis assumes 
there is no autocorrelation.
This test use the order.by= argument to define autocorrelation.")
     warns('Breusch-Godfrey', '>', LIST$Breusch , conf.level=conf.level)       
  }
  if(code)message(paste("bgtest(",regname,", order.by=",ordername,", order=3)"))

  #Harrison McCabe
  LIST$Harrison <- lmtest::hmctest(reg , order.by=order.by, point=0.5)$p.value
  if(verbose){
    intro('The Harrison-McCabe test on residuals')
    message("The Harrison-McCabe test checks the presence 
of correlation in the residuals of the model, computing the fraction
of residual sums of squares that relates to the fraction of 
the data before the breakpoint (here 50% of data ordered by ",ordername,".
Its null hypothesis assumes there is no correlation.
This test use the order.by= argument to define breakpoint.")
     warns('Harrison-McCabe', '>', LIST$Harrison , conf.level=conf.level)       
  }
  if(code)message(paste("hmctest(",regname,", order.by=",ordername,", point=0.5)"))

  #Ramsey test
  LIST$Ramsey <- lmtest::resettest(reg , power=2)$p.value
  if(verbose){
    part('Validation of functional form')
    intro("The Ramsey's RESET test for functional form")
    message("The Ramsey's RESET test is used to check 
the adequacy of a linear regression model. The test checks 
for the presence of omitted variables or functional misspecification 
in the model by evaluating whether the addition of a 
quadratic (or more) term of the fitted values improves the model's fit.")
     warns('Ramsey', '>', LIST$Ramsey , conf.level=conf.level)       
  }
  if(code)message(paste("resettest(",regname,", power=2)"))

  # Harvey-Collier
  LIST$Harvey <- lmtest::harvtest(reg , order.by=order.by)$p.value
  if(verbose){
    intro("The Harvey-Collier test for linearity")
    message("The Harvey-Collier test is based on the squared residuals 
of the regression model, which should have a constant variance 
across the range of the independent variables if the model is correctly
 specified. The test involves regressing the squared residuals on 
the fitted values and their squares, and then testing the null 
hypothesis of no functional misspecification against the alternative 
hypothesis that the residuals have a nonlinear relationship with 
the independent variables.")
     warns('Harvey-Collier', '>', LIST$Harvey, conf.level=conf.level )       
  }
  if(code)message(paste("harvtest(", regname,", order.by=",ordername,")"))

try({
  # MacKinnon-White-Davidson PE test 
  regData <-  reg$model #get model's data
  mins <- apply(regData,2,min) #min values
  regDataPositive <- regData+
        as.data.frame(matrix(rep(ifelse(mins<0, -mins+0.01, 0),  nrow(regData)), nrow= nrow(regData), byrow=TRUE))
  D <- as.data.frame(apply( regDataPositive, 2, log)) #evaluate log of data
  D <- na.omit(D)
  regln <- lm( eval(reg$call[[2]]), data=D)
  LIST$Pe <- min(lmtest::petest(reg , regln)[['Pr(>|t|)']])
  if(verbose){
    intro("MacKinnon-White-Davidson PE test for log-linearity")
    message("The MacKinnon-White-Davidson PE test compares 
two non-nest models where one has a linear specification of 
type y ~ x1 + x2 and the other has a log-linear specification 
of type log(y) ~ z1 + z2. Typically, the regressors in the latter
 model are logs of the regressors in the former, 
i.e., z1 is log(x1) etc.")
    warns('MacKinnon-White-Davidson PE', '>', LIST$Pe, conf.level=conf.level )       
  }
  if(code)message(paste("petest(", regname,", log(", regname,"))
  where log(",regname,") is the linear model fit with the same formula but with all variables passed in log.
(You'll need to implement it yourself :
   Every variables should be increased to be greater or equal than 0.01.)"))
}, FALSE)

invisible(LIST)
}
