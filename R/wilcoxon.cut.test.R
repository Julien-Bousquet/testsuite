#source('libs.R')

to.vector <- function(col, data){
# Get the x column in data, except if x is already a 
# existing vector it will return
  if(length(col)!=1){ cat('to.Vector(): problem in column name\n')}
  #col stand for column
 if(is.character(col)){
      if(!(col %in% colnames(data))){ 
        warning("The column ", col, " is unknown. \n")
        }else{
          return(data[,col])
        }
  }
  if(is.integer(col)){
      return(data[,col])
  }  
  if(is.vector(col)){
        return(col)
  }
}

#' Indexes around median.
#'
#'  Compute index of a proportion of values of group around the median
#' @author Julien Bousquet (2021)
#' @param group a vector with a calculable median
#' @param prop  a proportion between 0 and 1 of values to find around the median. The median is always returned among the indexes, and the proportion is symetricaly define around it.
#' @return The function return the vector of indexes of prop values around the median of the group
#' @details ind.median() is used in wilcoxon.cut.test() when prop>0. 
#'  The value of the median is always excluded.
#'   
#' @export
ind.median <- function(group, prop){
  number.out <- length(group)*prop # number of data out for prop
  if( length(group)%%2==0){#odd length of data  return
    return( 
      order(group)[round(length(group)/2-number.out/2):(round(length(group)/2+number.out/2)+1)]
    )
  }else{ #even length of data  return
       return(order(group)[round(length(group)/2-number.out/2):(round(length(group)/2+number.out/2)+1)])
  }
}

#' Compare two halves of a population by Wilcoxon test.
#' 
#'  Test the global efficiency of an index (group) on a mesure. 
#' This function is use in B. Jamet -- (2022) to show the efficiency of different banks 
#' informations product by ONG on  reals financial information (see examples).
#'
#' @author Julien Bousquet (2021)
#' @param x vector of names of the columns to test in data, or dataframe of the vectors to test, or  a name of data column
#' @param group a vector of values, or the name or the number of the column  variable in data to use to cut x : a vector with 2 values or a quantitative vector, with same length than x.
#' @param data data.frame with the data.
#' @param prop proportion of values excluded around the median of group. Make sense only if group is numerical, not factorial.
#' @param verbose if TRUE, always print the  wilcox.test(), if verbose=F print the result only if  p-value < 0.05
#' @param bootstrap  0 if no bootstrap, number of  bootstrap, only pertient on qualitative groups, not on a 2-values  group vector.
#' @param seed  NULL, an integer random seed for reproductibility of bootstrap.
#' @return a dataframe with wilcoxon p-values, or with confidence intervall of p-values, if bootstraped. 
#' Note that a p value is always positive. Here, the minus sign in front of a negative p-value means that the 
#' index (in group) is in the opposite order with x.
#'  
#' @details `wilcoxon.cut.test()` take the `group` vector and cut it in two halves around the median. 
#' This group vector may be numerical or factorial with 2 levels. 
#' A proportion prop= of data around the median may be possibly 
#' excluded. The two remaining samples of data in  `x`, associated  for  lower an upper parts of the group 
#' are passed to the test of Wilcoxon to compare their median. This allow us to validate the efficiency of the 
#' index `group` on the measured `x` tendency.
#'
#' @examples # Call with names of columns of dataframe
#' #wilcoxon.cut.test(x=c('Sepal.Width', 'Petal.Length'), group='Sepal.Length', data=iris) 
#'
#' @export
wilcoxon.cut.test <- function(x, 
	group, 
	data=NULL, 
	prop=0,
	verbose=FALSE, 
	bootstrap=0, 
	seed=NULL 
	){
   debug. <- FALSE # to facilitate debug for developpement
    if(!exists('x',inherits=FALSE) | !exists('group', inherits = FALSE)){
    print(' parameters x= or group= missing.')
  }
  # get data
  if(!is.null(data)){
      if(length(x)==1){# get variables names
        if(is.character(x)){x.name <- x
        }else{x.name <- colnames(data)[x] }
        x <- data[,x.name]
      
        if(is.character(group)){group.name <- group
        }else{group.name <- colnames(data)[group.name]}
          
        x   <- to.vector(x.name, data)  #get data for wilcoxon test
        if(debug.){ cat('\nStructure of x (wilcoxon.cut.test) : '); print(str(x))}        
        group <- to.vector(group, data) #get cuting vector

        if(debug.){cat('\n Structure de group (wilcoxon.cut.test) :'); print(str(group))}

        #if x and group don't have same length : stop
        if(length(x)!=length(group))stop('Lengthes of x and group differ.')

        # if missing data, omit individus from x and group
        ind.na <- which(is.na(x) | is.na(group)) # index of missing in x or group
        if(debug.){ cat('Index of missing data :\n');print(ind.na)}

        if(length(ind.na)>0){
          warning('Missing data removed.')
          x <- x[-ind.na] # omitting in x
          group <- group[-ind.na] # omitting in group
        }
        
        if(bootstrap==0){# calcul unique sans bootstrap
          # # if group vector already in factor : use it
          if(length(unique(group))==2){
            group <- as.integer(factor(group))
	cat("\n Boot = 0 & prop= 0. \n")
            if(prop>0)warning("Impossible to exclude proportion of values around the median : group is a factor variable with 2 values.\n
	It must be numerical to fix a non zero prop=. Fix it to prop=0 or change group.")
          }else{
          # else,
            if(prop>0 & prop<0.9){
	cat("\n Boot = 0 & prop> 0. \n")
              if(debug.)print("Exlude proportion prop of values.")
              # get out p% data, from x et group,around the median
              # keep same number on each side (erease the median)
              ind.out <- ind.median(group, prop)# indexes of p% data around group median
              group <- group[-ind.out] # 
              x <- x[-ind.out]
            }

            #built group
            group <- ifelse(group>median(group),2,1)
          }
          if(debug.){cat('\n Structure de group (wilcoxon.cut.test) : '); print(str(group))}
    
          #  test of wilcoxon
          if(verbose)cat('\n\n-------------------------------------------------------\n')
          w.t <- wilcox.test(x=x[group==1], y=x[group==2], exact=FALSE)
          if(verbose){
            cat("Test of ", x.name, 
                "cut by ", group.name,
                ': p-value = ',w.t$p.value,'\n')
          }
          if(debug.)cat('before return\n')
          DF <- data.frame(tested=x.name, 
                            group.by=group.name, 
                            p.value=sign(median(x[group==1]) - median(x[group==2]))*w.t$p.value,
                            star=star(w.t$p.value)
                            )
          return(DF) 
          
        }else{# with bootstrap
          if(verbose)print("Starting bootstrap")
          if(bootstrap<20){
            print("At least  20 repetition of bootstrap")
             bootstrap <- 20
          }
          
          if(debug.){print(' Beginning of bootstrap')}
          if(verbose){
            cat('\n\n-------------------------------------------------------\n')
            cat("Test of ",x.name,  "separated by ", group.name ,"\n")
          }
          if(debug.){cat('\n l. 130 bootstrap : ', bootstrap,' \n'); print(1:bootstrap)}
          
          foreach(k = 1:bootstrap, .combine=c )%do% {
            if(debug.)cat('Make Sample of bootstrap\n')
            # Check fixed random seed
            if(exists('seed', inherits = FALSE))set.seed(seed+k)
            # randomize index
            ind <- sample(1:length(x), length(x), replace=TRUE)
            if(debug.){cat('l. 136 ind. boot =');print(ind)}
            # Bootstrap sample of x
            x.boot <- x[ind]
            if(debug.){cat('x.boot : ');print(x.boot)}
            # Bootstrap sample of group
            group.boot <- group[ind]
            if(debug.){cat('group.boost = ');print(group.boot)}
            # Effectue le traitement du test :
            ## group checking (2 values) and making if needed
            if(length(unique(group.boot))==2){
              group.boot <- as.integer(factor(group.boot))
              if(k==1)cat("\nPlease convert groups in reals. Or use bootstrap=0.\n")
            }else{
            # else,
              if(prop>0 & prop<0.9){
                # extract p% of data, from x and  group, around median
                ind.out <- ind.median(group.boot, prop)# indexes of p% data around group median
                group.boot <- group.boot[-ind.out] # 
                x.boot <- x.boot[-ind.out]
              }
              # making bootstrapped groups
              group.boot <- ifelse( group.boot>median(group.boot), 2, 1)
            }
            if(debug.){cat('\n Structure de group.boot L. 152 (wilcoxon.cut.test) : ');   print(str(group.boot))}

            if(debug.){cat('p.value = ',wilcox.test(x=x.boot[group.boot==1],
                        y=x.boot[group.boot==2],
                        exact=FALSE)$p.value,'\n');
		cat('Sign = ',
			sign(median(x.boot[group.boot==1])-median(x.boot[group.boot==2])),
			'\n'
		)
			}
            
          	sign(median(x.boot[group.boot==1])-
		median(x.boot[group.boot==2]))*wilcox.test(x=x.boot[group.boot==1],
                        y=x.boot[group.boot==2], exact=FALSE)$p.value

          }-> pval
            q <- quantile(abs(pval),c(0.5,0.95)) #determine quantiles of pvalues
            if(verbose==TRUE|q[1]<0.05){
              cat('p-value is in [',min(pval),'; ',max(pval),']\n',
              'with quantile 50%=',q[1],'\n',
              'and quantile 95%=', q[2],' \n')
            }
            DF <- data.frame(tested=x.name, 
                            group.by=group.name, 
                             p.value.50=q[1],
                            p.value.95=q[2],
                            p.value=pval[ which.min(abs(abs(pval)-q[2])) ][1],#minimize the distance between pval and quantile
                            star=star(q[2])
                            )
          return(DF) 
        }
      }else{#length(x)>1
        # re-run wilcoxon.cut.test
        foreach(i = x, .combine=rbind)%do%{
          wilcoxon.cut.test(x=i, group=group, data=data, prop=prop,
                      verbose = verbose, bootstrap=bootstrap)
        } -> res
        return(res)
      }
  }
  if(is.null(data)){
  # x must be a vector of value, or an other dataframe
    data <- data.frame(x,group) # make a dataframe
    wilcoxon.cut.test(x=colnames(data)[1:(ncol(data)-1)], 
                group=colnames(data)[ncol(data)], 
                data=data,
                verbose=verbose,
                bootstrap = bootstrap,
	prop=prop) 
  }
}  





