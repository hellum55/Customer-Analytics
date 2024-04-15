# R script for the analysis of the McDonald's example in the segmentation lecture
library("flexclust")
# install.package(clue) - should be installed as part of the flexclust but isn't
library("partykit")
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
# k-means
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

## Profiling segments
# Segment profile plot
MD.vclust <- hclust(dist(t(MD.x)))
barchart(MD.k4, shade = TRUE,which = rev(MD.vclust$order))
# Segment separation plot
plot(MD.k4, project = MD.pca, data = MD.x,hull = FALSE, simlines = FALSE,
     xlab = "principal component 1",ylab = "principal component 2")
projAxes(MD.pca)

## Describing segments
# Extract the segment membership for each consumer
mcdonalds$k4 <- clusters(MD.k4)
# Prepare the like/hate variable
mcdonalds$like <- factor(mcdonalds$Like,levels=c("I love it!+5","+4","+3","+2","+1","0","-1","-2","-3","-4","I hate it!-5"))
# Make mosaic plot of like/hate
mosaicplot(table(mcdonalds$k4, mcdonalds$like), shade = TRUE,main = "", 
           xlab = "segment number")
# Make mosaic plot of gender
mosaicplot(table(mcdonalds$k4, mcdonalds$Gender), shade = TRUE)
boxplot(mcdonalds$Age ~ mcdonalds$k4,varwidth = TRUE, notch = TRUE)

## Predict segment 3 membership using a classification tree
# Prepare visit frequency variable
mcdonalds$visitfrequency <- factor(mcdonalds$VisitFrequency,
                                   levels=c("Never","Once a year","Every three months","Once a month","Once a week","More than once a week"))
tree <- ctree(factor(k4==3) ~ like+Age+visitfrequency+factor(Gender),
              data=mcdonalds)
plot(tree)

## Selecting (the) target segment(s)
# Visit frequency is scored as 1=Never,...,6=More than once a week, 
# not strictly aligned with the ratio properties hidden in the scale
visit <- tapply(as.numeric(mcdonalds$visitfrequency),mcdonalds$k4,mean)
visit
# Turn the current like scale upside down and recenter 
# (1=I love it,...,11=I hate it -> 5=I love it,...,-5=I hate it)
like <- tapply(-as.numeric(mcdonalds$like)+6,mcdonalds$k4,mean)
like
# Create the gender variable
female <- tapply((mcdonalds$Gender=="Female")+0,mcdonalds$k4,mean)
female
# Make segment evaluation plot
plot(visit, like, cex = 10 * female,xlim = c(2, 4.5), ylim = c(-3, 3))
text(visit, like, 1:4)
