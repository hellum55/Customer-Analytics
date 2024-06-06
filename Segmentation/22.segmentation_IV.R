# R script for the analysis of the McDonald's example in LCA I lecture
library("flexclust")
library("flexmix")
rm(list=ls())

## Preparation
# Read in data
mcdonalds <- read.csv("mcdonalds.csv")
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
# k-Means
set.seed(28)
MD.km28 <- stepFlexclust(MD.x, 2:8, nrep = 10,verbose = FALSE)
plot(MD.km28, xlab = "number of segments")
# Segment stability within the same number of segments
set.seed(1234)
MD.b28 <- bootFlexclust(MD.x, 2:8, nrep = 10,nboot = 100)
plot(MD.b28, xlab = "number of segments",ylab = "adjusted Rand index")
# Gorge plot
MD.km28 <- relabel(MD.km28)
histogram(MD.km28[["4"]], data = MD.x, xlim = 0:1)
# Segment level stability across solutions
slsaplot(MD.km28)
# Save the four-segment solution
MD.k4 <- MD.km28[["4"]]
MD.r4 <- slswFlexclust(MD.x, MD.k4)
plot(MD.r4, ylim = 0:1, xlab = "segment number",ylab = "segment stability")

# mixture distribution
set.seed(1234)
MD.m28 <- stepFlexmix(MD.x ~ 1, k = 2:8, nrep = 10, model = FLXMCmvbinary(), 
                      verbose = FALSE)
MD.m28
plot(MD.m28,ylab = "value of information criteria (AIC, BIC, ICL)")
MD.m4 <- getModel(MD.m28, which = "4")
table(kmeans = clusters(MD.k4),mixture = clusters(MD.m4))
MD.m4a <- flexmix(MD.x ~1, cluster = clusters(MD.k4),model = FLXMCmvbinary())
table(kmeans = clusters(MD.k4),mixture = clusters(MD.m4a))
logLik(MD.m4a)
logLik(MD.m4)

