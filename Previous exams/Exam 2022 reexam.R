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
corp_rep_data <- read.csv(file ="PLS_R_exam.csv",header=TRUE,sep=";")

## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("COMP",multi_items("comp_",1:2),weights = mode_A),
  composite("LIKE",multi_items("like_",1:3),weights = mode_A),
  composite("CUSA",single_item("cusa")),
  composite("CUSL",multi_items("cusl_",1:3),weights = mode_A))
# Create structural model
simple_sm <- relationships(paths(from = c("LIKE"), to = c("COMP")),
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

########################################################################
######################## Mediation analysis ############################
########################################################################
#Question 4 of part 2:
summary_simple_corp_rep$total_indirect_effects
# Summarize the results of the bootstrap
summary_boot_corp_rep_ext <- summary(boot_simple_corp_rep,alpha= 0.05)
summary_boot_corp_rep_ext

# Inspect indirect effects
specific_effect_significance(summary_boot_corp_rep_ext,from= 'LIKE', through= 'COMP', to= 'CUSA', alpha= 0.05)
# Inspect the confidence intervals for direct effects
summary_simple_corp_rep$paths
sum_boot_simple_corp_rep$bootstrapped_paths

# Calculate the sign of p1*p2*p3
summary_simple_corp_rep$paths['LIKE', 'CUSA']*summary_simple_corp_rep$paths['LIKE','COMP']*summary_simple_corp_rep$paths['COMP','CUSA']

###########################################################################
####################### Rand Index ########################################
###########################################################################
library("flexclust")
# install.package(clue) - should be installed as part of the flexclust but isn't
library("partykit")
df <- data.frame(c(1, 2, 2, 3, 1),
                 c(3, 3, 3, 1, 2))
set.seed(1234)
MD.b28 <- bootFlexclust(df,nrep = 10,nboot = 100)
plot(MD.b28, xlab = "number of segments",ylab = "adjusted Rand index")
slsaplot(df)
