# Chapter_13-FactorAnalysis.R
# This file contains all code examples from chapter 13 in 
# Mehmetoglu & Mittner (2021). Applied Statistics Using R. SAGE.
##

## setup
library(tidyverse)
# library(astatur)
# theme_set(theme_astatur())


#data
load("workout3.rda")
library(dplyr)
workout3_comp <- filter(workout3,
                        complete.cases(workout3))


#determine the number of factors (see other criteria in the course)
#i)parallel analysis 
library(psych)
paranalysis <- fa.parallel(workout3_comp, 
      fm="pa", fa="fa", SMC="TRUE")
print(paranalysis)

#ii)squared multiple correlations (SMC are commonalities at the first iteration)
squaredmc <- smc(workout3_comp)
squaredmc
mean(squaredmc)


#extracting the factors
library(psych) 
fmodel1 <- fa(workout3_comp,
              nfactors = 2, 
              fm="pa",
              rotate = "varimax")

print(fmodel1$n.obs)
print(fmodel1$loadings, digits=4, cutoff=0) 
# for better interpretation play with cutoff

#relationship between factors and variables geometrically
fa.plot(fmodel1)


#commonalities after estimation
comm <- fmodel1$communality
comm
#total variance explained
sum(comm)
# [1] 4.772239; in PCA it would be 6 (6X1)

# diving the eigenvalues by the total variance explained
shareFactor1 = 2.42/4.77
shareFactor2 = 2.36/4.77

#communilities and uniquenesses
cbind(h2=fmodel1$communality, u2=fmodel1$uniquenesses) 


#estimated factor scores
fmodel1$scores

#generated factors scores (average of the var expressing each factor)
itemlist <- list(relaxation=c("Var1","Var2","Var3"),
                 appearance=c("Var4","Var5","Var6"))
summateds <- scoreItems(itemlist, workout3_comp, 
                         min=1, max=6, totals = FALSE)
factordata <- as.data.frame(summateds$scores)

# add new var to the dataset
workout3_comp <- cbind(workout3_comp, factordata)
names(workout3_comp)


#Cronbach´s alpha coef
relaxation <- data.frame(workout3_comp[,1:3])
psych::alpha(relaxation)$total$std.alpha

appearanc <- data.frame(workout3_comp[,4:6])
psych::alpha(appearance)$total$std.alpha

