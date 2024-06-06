#Question 2 of part 2:
###############################################
########### evaluate the mode##################
##############################################
# R script for the illustrations in chapter 5
rm(list=ls())
library(seminr)
library(readr)
## Preparation
# Read in data 
corp_rep_data <- read.csv(file ="PLS_O_exam.csv",header=TRUE,sep=";")

## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("COMP",multi_items("comp_",1:3),weights = mode_A),
  composite("LIKE",multi_items("like_",1:3),weights = mode_A),
  composite("CUSA",single_item("cusa")),
  composite("CUSL",multi_items("cusl_",1:2),weights = mode_A))
# Create structural model
simple_sm <- relationships(paths(from = c("COMP"), to = c("LIKE")),
                          paths(from = c("COMP","LIKE"), to = c("CUSA")),
                           paths(from = c("CUSA"), to = c("CUSL")))

## Estimation
# Estimate the model
corp_rep_simple_model <- estimate_pls(data = corp_rep_data,
                                      measurement_model = simple_mm,
                                      structural_model = simple_sm,
                                      inner_weights = path_weighting,
                                      missing = mean_replacement,
                                      missing_value = "-99")

## Summarizing the results
# Summarize the model results
summary_simple_corp_rep <- summary(corp_rep_simple_model)
# Collinearity issues?
summary_simple_corp_rep$vif_antecedents
# Inspect the model's fsquare
summary_simple_corp_rep$fSquare
# Inspect the model's path coefficients and the R^2 values
summary_simple_corp_rep$paths

# Bootstrap the model
boot_simple_corp_rep <- bootstrap_model(seminr_model = corp_rep_simple_model,
                                        nboot = 1000,cores= parallel::detectCores(), seed = 123)

# Store the summary of the bootstrapped model
sum_boot_simple_corp_rep <- summary(boot_simple_corp_rep)
#significance and relevance of the structural model relationships 
sum_boot_simple_corp_rep$bootstrapped_paths

#Predictive power
predict_corp_rep_ext <- predict_pls(
  model = corp_rep_simple_model,
  technique = predict_DA,
  noFolds = 10,
  reps = 10)

sum_predict_corp_rep_ext <- summary(predict_corp_rep_ext)

#Inspect the results:
par(mfrow=c(1,2))
plot(sum_predict_corp_rep_ext,
     indicator = "cusl_1")
plot(sum_predict_corp_rep_ext,
     indicator = "cusl_2")
par(mfrow=c(1,1))
########################################################################
######################## Mediation analysis ############################
########################################################################
#Question 4 of part 2:
summary_simple_corp_rep$total_indirect_effects
# Summarize the results of the bootstrap
summary_boot_corp_rep_ext <- summary(boot_simple_corp_rep,alpha= 0.05)
summary_boot_corp_rep_ext

# Inspect indirect effects
specific_effect_significance(summary_boot_corp_rep_ext,from= 'COMP', through= 'LIKE', to= 'CUSA', alpha= 0.05)
# Inspect the confidence intervals for direct effects
summary_simple_corp_rep$paths
sum_boot_simple_corp_rep$bootstrapped_paths

# Calculate the sign of p1*p2*p3
summary_simple_corp_rep$paths['COMP', 'CUSA']*summary_simple_corp_rep$paths['COMP','LIKE']*summary_simple_corp_rep$paths['LIKE','CUSA']

#######################################################################
################### Cluster analysis ##################################
#######################################################################
#Part 3, Question 1:
library(readstata13)
library(NbClust)
#Create dataframe
var1 <- c(4, 5, 6, 7)
var2 <- c(5, 5, 2, 1)
var3 <- c(1, 1, -1, -1)

df <- data.frame(var1=c(4, 5, 6, 7),
                    var2=c(5, 5, 2, 1),
                    var3=c(1, 1, -1, -1))

## Hierarchical clustering
# first create distance matrix
dist <- dist(df,method="euclidean")
dist2 <- dist^2
dist2

# Complete linkage
fit <- hclust(dist,method="complete")
# draw a dendogram
plot(fit)
# assess pct. increase
denominator <- cumsum(fit[[2]])
length(denominator) <- nobs-2
denominator <- c(1,denominator)
pct <- fit[[2]]/denominator
tail(pct,n=10)
# results from Ward's method are supported -
grp <- as.factor(cutree(fit,k=2))
table(grp)
# but size of the clusters differ
rect.hclust(fit,k=2,border="red")
# assess outcome
aggregate(df,list(grp),mean)
summary(aov(var1~grp,data=df))
summary(aov(var2~grp,data=df))
summary(aov(var3~grp,data=df))

#Non-hierachical clustering
set.seed(4118)
fit2<-kmeans(df,2,nstart=3)
print(fit2)
grp <- as.factor(fit2[[1]])
table(grp)

# snake plot
matplot(t(fit2[[2]]),type="l")
# criterion validity
aggregate(df,list(grp),mean)
summary(aov(var1~grp,data=df))
summary(aov(var2~grp,data=df))
summary(aov(var3~grp,data=df))
