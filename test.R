# objectif : comprendre pairwise.table()

# Implementation du test de student sur iris sur Sepal.Length
ttest <- function(i, j){
  message(i)
  t.test(iris[ as.numeric(iris$Species)==i, 1], 
           iris[ as.numeric(iris$Species)==j, 1])$p.value
}

ttest('versicolor', 'virginica')
M <- pairwise.table(ttest, levels(iris$Species), p.adj="holm")

pairwise.result <- pairwise.t.test(iris$Sepal.Length, g=iris$Species, p.adj="bon")


# TO DO
# test :
#categories <- c('A', 'B', 'C', 'D')
#M <- matrix(c(1,1,1,1,1,1,0.01,0.01,1), byrow=TRUE, ncol=3, dimnames=list(categories[-1],categories[-length(categories)]))

# TODO : 

pairwise.formula <- function(){
  # see t1way programmation
          if (missing(data)) {
          mf <- model.frame(formula)
        }
        else {
          mf <- model.frame(formula, data)
        }
        cl <- match.call()
    x <- split(model.extract(mf, "response"), mf[, 2])
}


# ajouter au cours sur les packages : programmer une fonction 
#  Got variable name given to the parameter of a function
argname <- function(x, y, z){ 
  print( deparse1(substitute(x)))
  print( substitute(y))
  print( substitute(z))
}
argname(xx,yy,ztoto)


# tester ça et aussi sur les 100 premiers 
x <- iris$Sepal.Length
g <- iris$Species
m.test(x,g, code=TRUE)

bartlett.test( x, g)$p.value
lawstat::levene.test( x, g)$p.value
fligner.test( x,g)$p.value
oneway.test( x~g,  var.equal=FALSE)$p.value
oneway.test( x~g,  var.equal=TRUE)$p.value
kruskal.test( x, g)$p.value
WRS2::t1way( x~g)$p.value
WRS2::med1way( x~g)$p.value

catego( x, g, test_fnct="t.test")
agricolae::SNK.test( aov(x~g),trt="g", group=TRUE, alpha=0.05)$p.value
catego( x, g, test_fnct="wilcox.test")

 # Manipulation des formules
f <- formula(Sepal.Length~ Sepal.Width + Petal.Length)  
f[[3]]
all.vars(f)

f <- formula(Sepal.Length~ Sepal.Width + Petal.Length)  
reg2 <- lm(f)
attach(iris)
library(magrittr)
eval(getCall(reg1)$data) %>% colnames
eval(getCall(reg2)$data)

reg$model #  got data from lm result 
eval(reg$call[[2]]) # got formula from lm result

f1 <- Sepal.Length ~  Petal.Length
f2 <- Sepal.Length ~ .  - Petal.Length
reg1 <- lm( f1, data=iris)
reg2 <- lm( f2, data=iris)

lm.compare(reg1, reg2)


# TO DO 
#' Check if two linear models are nested
nested <- function(){
 #f1 <- eval(reg1$call[[2]])
 #f2 <- eval(reg2$call[[2]])
 #indvar1 <- all.vars(f1)[-1]
#indvar2 <- all.vars(f2)[-1]
#depvar1 <- all.vars(f1)[1]
#depvar2 <- all.vars(f2)[1]
#xnames1 <- colnames(  eval(getCall(reg1)$data) )
#xnames2 <- colnames(  eval(getCall(reg2)$data) )
#if('.'  %in% indvar1 ) indvar1 <- xnames1[-which(xnames1==depvar1)]
#if('.'  %in% indvar2 ) indvar2 <- xnames2[-which(xnames2==depvar2)]
#if(indvar1 %in%   indvar2 |   indvar2 %in%   indvar1 ) return(TRUE)
#  return(FALSE)
 #TODO
#Need to take care of - sign in formula
}
