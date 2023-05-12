#' Pairwise comparisons tests
#'
#' `pairwise()` compute p-value for all the possible
#' combinaison of groups. The p-values are corrected by 
#' the `pairwise.table()` function of package stats.
#' @author Julien Bousquet (2021)
#' @param x the values : a vector of values, off the samples to be compared, of a formula.
#' @param g the groups : the vector of factors, same length than parameter `x`
#' @param  test_fnct the fonction used to calculate p-value. Actually
#' 't.test', 'wilcox.test', 'yuen','var.test' are supported.
#' @param p.adjust.method the method to correct the p-value. 
#' See `p.ajust.methods` to see the whole list. 
#' @param ... other parameters to give to `pairwise.table()`.
#' @ return A list of class "pairwise.htest". Find the table of p-values 
#' in field $p.value. 
#' @examples
#' # Classical Student test :
#' #pairwise(x=iris$Sepal.Length, g=iris$Species, test_fnct='t.test')
#'
#' # Robust Yuen's test of WRS2 package :
#' #pairwise(x=iris$Sepal.Length,g=iris$Species, test_fnct='yuen')
#'
#' # Also compare variance :
#' #pairwise(x=iris$Sepal.Length,g=iris$Species, test_fnct='var.test')
#'
#' @export
pairwise <- function(x, 
  test_fnct='t.test', 
  p.adjust.method=p.adjust.methods, ...){
  UseMethod('pairwise', x)
}

#' @rdname pairwise
#' @export
pairwise.numeric <- function(x, g, 
  test_fnct='t.test', 
  p.adjust.method=p.adjust.methods, ...){
  test_fnct <- match.arg(test_fnct, 
                        choices=c('t.test', 'wilcox.test', 'yuen','var.test'))
  p.adjust.method <- match.arg(p.adjust.method)

  DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(g)))

  compare.levels <-   switch( test_fnct,
    't.test' = function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, ... )$p.value
        },
    'wilcox.test' = function(i,j){
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            wilcox.test(xi, xj, ...)$p.value
        },
     'yuen' = function(i,j){
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            WRS2::yuen( x~g,
                data=data.frame(x=c(xi, xj), g= c(rep(1, length(xi)), rep(2, length(xj)))),
                ...
            )$p.value
        },
    'var.test' = function(i,j){
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            wilcox.test(xi, xj, ...)$p.value
        }
    )
    
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = test_fnct, data.name = DNAME, p.value = PVAL, 
        p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

#' @rdname pairwise
#' @param formula the formula giving  `x~g` where `x` are the quantitatives measures
#' and `g` is a factor of the same length.
#' @param data the dataframe where `x` and `g` are 
#' columns names. 
#' @param g the groups : the vector of factors, same length than parameter `x`
#' @export
pairwise.formula <- function(formula,
  test_fnct='t.test', 
  p.adjust.method=p.adjust.methods, data, ...){
message('Not yet implemented')
}


#' Compute pairwise tests and categorize groups from the results of the pairwise function
#'
#' @author Julien Bousquet (2021)
#' @inheritParams pairwise 
#' @ return A list  : 
#' * a dataframe $cat with a named category for each group,
#' * an object of class `pairwise.htest`.
#' If a group are between two category, and are 
#' name with the two letters. 
#' @examples
#' #catego(x=iris$Sepal.Length, g=iris$Species, p.adjust.method='bonferroni')
#' @export
catego <- function(x, ...){
  UseMethod('catego', x )
}

#' @rdname catego
#' @export
catego.numeric  <- function ( x, g, 
  test_fnct='t.test', 
  p.adjust.method=p.adjust.methods,
  conf.level=0.95, ...
  ){
 # Order groups from bigger  to smaller
  ord <- order(as.vector(by(x, g, median)  ) , decreasing=TRUE) 
  g <- ordered( g, levels(g)[ord] )
  ANS <- list() #final answer

  ANS$pairwise <- M <- pairwise(x=x, g=g, test_fnct=test_fnct,
     p.adjust.method=p.adjust.methods, ...)$p.value

  categories <- c(colnames(M), rownames(M)[nrow(M)])

  # Make M symetric
  Msym <- matrix(1, ncol=length(categories), 
                                nrow=length(categories),
                                dimnames=list(categories, categories))

  for(i in 1:length(categories)-1){
    for(j in (i+1):length(categories)){
       #print(paste('i=',i, 'j=',j,'\n'))
       Msym[j, i] <- Msym[i,j] <- M[j-1,i]
    }
  }

  # Grep low pval to create groups
  groups <- as.list(categories)
  for(i in 1:length(categories)){
    groups[[i]] <- categories[Msym[i,]> 1-conf.level]
  }  

   recode <- vector(mode='list', length=length(categories))
   recode[[1]] <- 'a'
   idx <- 1 # number of the letter
  # recode groups with letters
  for(i in 2:length(categories)){
   # if groups[[i-1]] inside groups[[i]] : same letter
    if(sum(!(groups[[i-1]] %in% groups[[i]]))==0){
      recode[[i]] <- c(recode[[i]], recode[[i-1]])
    }

    if(sum(!(groups[[i]] %in% groups[[i-1]]))==0){
      recode[[i]] <- c( recode[[i]], recode[[i-1]][ length( recode[[i-1]]) ])
    }

    if(sum(!(groups[[i]] %in% groups[[i-1]]))>0){
      idx <- idx+1
      recode[[i]] <- c(recode[[i]], letters[idx])
    }
  }  
  names(recode) <- categories
  recode <- lapply(recode, unique)
  recode <- lapply(recode, paste0, collapse='')
  ANS$cat <- cbind( categories, t(as.data.frame(recode)))
  colnames(ANS$cat) <- c('categories','groups')
  ANS
}


#' @rdname catego
#' @param formula the formula giving  `x~g` where `x` are the quantitatives measures
#' and `g` is a factor of the same length.
#' @param data the dataframe where `x` and `g` are 
#' columns names. 
#' @export
catego.formula  <- function ( formula, 
  test_fnct='t.test', 
  p.adjust.method=p.adjust.methods,
  conf.level=0.95, data, ...
  ){
  message('Not yet implemented')
}