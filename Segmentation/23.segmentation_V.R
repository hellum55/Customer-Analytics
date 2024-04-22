# R script for the analysis of the McDonald's example in LCA II lecture
library(flexmix)
library("flexclust")
rm(list=ls())

## Preparation
# Read in data
mcdonalds <- read.csv("c:/users/au78328/OneDrive - Aarhus Universitet/home/customer_analytics/s24/analysis/mcdonalds.csv")
# Look into content
names(mcdonalds)
dim(mcdonalds)
head(mcdonalds, 3)
# Change Yes/No to 1/0
MD.x <- as.matrix(mcdonalds[, 1:11])
MD.x <- (MD.x == "Yes") + 0

## Exploring data
# What fraction sees McDonald's possessing each of the attributes
round(colMeans(MD.x), 2)
# PCA and perceptual map
MD.pca <- prcomp(MD.x)
summary(MD.pca)
print(MD.pca, digits = 1)
plot(predict(MD.pca), col = "grey")
projAxes(MD.pca)

## Extracting segments
# Mixture regression
# Need to fix the representation of like and use "like" (not "Like") onwards
mcdonalds$like <- factor(mcdonalds$Like,levels=c("I love it!+5","+4","+3","+2","+1","0","-1","-2","-3","-4","I hate it!-5"))
rev(table(mcdonalds$like))
mcdonalds$like.n <- 6 - as.numeric(mcdonalds$like)
table(mcdonalds$like.n)
# Prepare the regression function for the finite mixture of linear regression models
f <- paste(names(mcdonalds)[1:11], collapse = "+")
f <- paste("like.n ~ ", f, collapse = "")
f <- as.formula(f)
f
set.seed(1234)
MD.reg2 <- stepFlexmix(f, data = mcdonalds, k = 2,nrep = 10, verbose = FALSE)
MD.reg2
MD.ref2 <- refit(MD.reg2)
summary(MD.ref2)
plot(MD.ref2, significance = TRUE)
