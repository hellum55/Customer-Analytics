# R script for the analysis of the HBAT example in the model-based clustering
# lecture
library(readstata13)
rm(list=ls())

## Preparation
HBAT <- read.dta13("hbat.dta")
# first we make our own id variable based on the original order
nobs <- nrow(HBAT)
HBAT$id <- seq(1,nobs)
# pick the variables for cluster analysis and summarize them
hbat <- c("x6","x8","x12","x15","x18")
summary(HBAT[,hbat])
apply(HBAT[,hbat],2,sd)
# multicollinearity should not be a problem
cor(HBAT[,hbat])
# which is confirmed
# look for outliers
dev <- t(t(HBAT[,hbat])-apply(HBAT[,hbat],2,mean))
dev2 <- dev^2
sumdev2 <- rowSums(dev2)
sort(sqrt(sumdev2))
# observations 6 and 87 are candidates for deletion
HBAT <- subset(HBAT,id!="6"&id!="87")
nobs <- nrow(HBAT)
HBAT$id <- seq(1,nobs)

## Model-based clustering
library("mclust")
(mc <- Mclust(HBAT[,hbat]))
# Assess different solutions
plot(mc, HBAT[,hbat], what = "BIC", col = "black")
# Summarize
summary(mc)
# Look at a 4-cluster solution
mc4 <- Mclust(HBAT[,hbat],G=4)
summary(mc4)
