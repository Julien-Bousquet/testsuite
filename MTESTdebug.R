# Push in github
# configuration
git config --global user.name 'Julien-Bousquet'
git config --global user.email "julien.bousquet@u-bordeaux.fr"
# Création du répertoire pour Benoit
git init BankInvestmentOnCarbon-2022
# Connexion au dossier local du git
cd BankInvestmentOnCarbon-2022/ 
# Crée un fichier local
echo "test" > README # fabrique un fichier

# Connexion à distance
git remote add origin https://github.com/Julien-Bousquet/Benoit-Jamet-BankInvestmentOnCarbon-2022.git
git branch -M main
# Effectuer un commit avant un push
git commit -m "add README" 

# Envoi le git local au git distant
git push -u origin main




library(foreach); library(parallel)
cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)
stopCluster(cl)
wil.cut.env <- function(){
# cree des variables dans l'environnement global qui permettent de tester la fonction wilcoxon.cut.test( ) 
#en pas a pas.
 x.name<<-'FEF1'
 group.name<<-'FEF2'
 data<<-X
 bootstrap<<-20
 x <<- to.vector(x.name, data)
 group <<- to.vector(group.name, data)
 bootstrap <<- 5
}
#wil.cut.env()

devtools::document('monpackage'); library(monpackage) 
DF <- data.frame(x=sample(1:20,replace=T), group=rep(c('A','B'),each=10))
wilcoxon.cut.test('x', 'group', data=DF, prop=0.3)

devtools::document('monpackage'); library(monpackage) 
DF2 <- data.frame(x1=sample(1:20,replace=T), x2=sample(1:20,replace=T), group=sample(1:20))
wilcoxon.cut.test(c('x1','x2'), 'group', data=DF2, prop=0.3)
wilcoxon.cut.test(c('x1','x2'), 'group', data=DF2, prop=0.3, boot=100)
library(foreach)


M.env <- function(){
x <<- rnorm(75)
group <<-  rep(1:3, each=25)
maxcat <<- 50
debug. <<- TRUE
i <<-1
L <<- R
pval<<-0.05
paired<<-TRUE
arr.vspace <<-2.5
}
M.env()

devtools::document('monpackage'); library(monpackage)
# test à 2 groupes appariés
k1 <- 50; k2 <- 50 ; M.test(x=rnorm(k1+k2), g=rep(1:2, c(k1,k2)),paired=TRUE) -> L;L

# Test de chargement de la librairie
#  Julien-Bousquet /Benoit-Jamet-BankInvestmentOnCarbon-2022 
devtools::install_github('Julien-Bousquet/Benoit-Jamet-BankInvestmentOnCarbon-2022')


#tests non paired goupes
k1 <- 50; k2 <- 102 ; 
M.test(x=c(rnorm(k1),rnorm(k2,4,5)), g=rep(c('A','B'), c(k1,k2))) -> L; L	

# test with 4 groupes
k1 <- 50; k2 <- 100 ; k3 <- 50;k4 <- 50; 	
x4 <- c(rnorm(k1),rnorm(k2,4,5), rnorm(k3,1,1), rnorm(k4,5,1))
group4<- rep(c('A','B','C','D'), c(k1,k2,k3,k4))
M.test(x=x4, g=group4) -> L; L	

#test with 10 groups
k5<-k6<-k7<-k8<-k9<-k10<- k4
x10 <- c(rnorm(k1),rnorm(k2,4,5), rnorm(k3,1,1), rnorm(k4,5,1), rnorm(k5,5,1), rnorm(k6,5,1), rnorm(k7,5,1), rnorm(k8,5,1), rnorm(k9,5,1), rnorm(k10,5,1))
group10<- rep(c('A','B','C','D','E','F','G','H','I','J'), c(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10))
M.test(x=x10, g=group10) -> L; L	



aov(Sepal.Length~ Species, data=iris)-> AOV1; AOV1
agricolae::SNK.test(AOV1, trt="Species", group=TRUE, alpha=pval)$groups
aov(Sepal.Length~ Species, data=iris)-> AOV; summary(AOV)

cbind(x,group) -> df
as.data.frame(df) -> d
agricolae::SNK.test(aov(x~ group, data=df), trt="group", group=TRUE)$groups



# TODO ##########""
## test d'affichage d'une data.frame
# library(foreach) ne s'installe pas toute seule

