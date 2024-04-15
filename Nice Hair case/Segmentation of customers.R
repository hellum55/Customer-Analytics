# R script for the analysis of the HBAT example in the cluster analysis lecture
library(readstata13)
library(NbClust)
rm(list=ls())

## Preparation
data <- read.csv("nicehair_clean.csv")
# first we make our own id variable based on the original order
nobs <- nrow(data)
data$id <- seq(1,nobs)

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

## Hierarchical clustering
# first create distance matrix
dist <- dist(HBAT[,hbat],method="euclidean")
dist2 <- dist^2
# Wards method
H.fit <- hclust(dist2,method="ward.D")
# draw a dendogram
plot(H.fit)
# assess pct. increase
denominator <- cumsum(H.fit[[2]])
length(denominator) <- nobs-2
denominator <- c(1,denominator)
pct <- H.fit[[2]]/denominator
tail(pct,n=10)
# apart from going from 2-1 the largest jump is from 4-3, we stop just before
grp <- as.factor(cutree(H.fit,k=4))
table(grp)
# illustrate a 4 cluster solution
rect.hclust(H.fit,k=4,border="red")
# assess outcome
aggregate(HBAT[,hbat],list(grp),mean)
summary(aov(x6~grp,data=HBAT))
summary(aov(x8~grp,data=HBAT))
summary(aov(x12~grp,data=HBAT))
summary(aov(x15~grp,data=HBAT))
summary(aov(x18~grp,data=HBAT))
# Complete linkage
H.fit <- hclust(dist,method="complete")
# draw a dendogram
plot(H.fit)
# assess pct. increase
denominator <- cumsum(H.fit[[2]])
length(denominator) <- nobs-2
denominator <- c(1,denominator)
pct <- H.fit[[2]]/denominator
tail(pct,n=10)
# results from Ward's method are supported -
grp <- as.factor(cutree(H.fit,k=4))
table(grp)
# but size of the clusters differ
rect.hclust(H.fit,k=4,border="red")
# assess outcome
aggregate(HBAT[,hbat],list(grp),mean)
summary(aov(x6~grp,data=HBAT))
summary(aov(x8~grp,data=HBAT))
summary(aov(x12~grp,data=HBAT))
summary(aov(x15~grp,data=HBAT))
summary(aov(x18~grp,data=HBAT))
# use NbClust
res<-NbClust(HBAT[,hbat], distance = "euclidean", min.nc=2, max.nc=8, 
             method = "ward.D", index = "all")
res$All.index
res$Best.nc


## Nonhierarchical clustering
set.seed(4118)
NH.fit<-kmeans(HBAT[,hbat],4,nstart=25)
print(NH.fit)
grp <- as.factor(NH.fit[[1]])
table(grp)
# assess outcome
aggregate(HBAT[,hbat],list(grp),mean)
summary(aov(x6~grp,data=HBAT))
summary(aov(x8~grp,data=HBAT))
summary(aov(x12~grp,data=HBAT))
summary(aov(x15~grp,data=HBAT))
summary(aov(x18~grp,data=HBAT))
# snake plot
matplot(t(NH.fit[[2]]),type="l")
# criterion validity
aggregate(HBAT[,c("x19","x20","x21","x22")],list(grp),mean)
summary(aov(x19~grp,data=HBAT))
summary(aov(x20~grp,data=HBAT))
summary(aov(x21~grp,data=HBAT))
summary(aov(x22~grp,data=HBAT))
# profiling
tbl <- table(HBAT$x1,grp)
round(100*prop.table(tbl,2))
chisq.test(tbl)
tbl <- table(HBAT$x2,grp)
round(100*prop.table(tbl,2))
chisq.test(tbl)
tbl <- table(HBAT$x3,grp)
round(100*prop.table(tbl,2))
chisq.test(tbl)
tbl <- table(HBAT$x4,grp)
round(100*prop.table(tbl,2))
chisq.test(tbl)
tbl <- table(HBAT$x5,grp)
round(100*prop.table(tbl,2))
chisq.test(tbl)


## Toy example
inc <- c(5,6,15,16,25,30)
edu <- c(5,6,14,15,20,19)
toy <- data.frame(inc,edu)
toy.dist <- dist(toy,method="euclidean")
toy.dist2 <- toy.dist^2
toy.H.single <- hclust(toy.dist2,method="single")
# the actual amalgamation
toy.H.single$merge
# the associated increase in distance/heterogeneity
toy.H.single$height
# a permutation of the original observations suitable for plotting
toy.H.single$order
# plot
plot(toy.H.single)

# other linkage possibilities
toy.H.complete <- hclust(toy.dist,method="complete")
toy.H.average <- hclust(toy.dist,method="average")
toy.H.centroid <- hclust(toy.dist,method="centroid")
# Wards method
toy.H.ward <- hclust(toy.dist2,method="ward.D")

## Distance vs shape
scores<-matrix(c(21,34,17,42,62,75,58,85),nrow=4,byrow=F)
matplot(scores,type="l")
dist(t(scores),method="euclidean")
cor(scores)