# R script for the illustrations in chapter 4
rm(list=ls())
library(seminr)

## Preparation
# Read in data 
corp_rep_data <- read.csv(file ="Corporate Reputation Data.csv",header=TRUE,sep=";")
# Create measurement model
corp_rep_mm <- constructs(
  composite("COMP",multi_items("comp_",1:3),weights = mode_A),
  composite("LIKE",multi_items("like_",1:3),weights = mode_A),
  composite("CUSA",single_item("cusa")),
  composite("CUSL",multi_items("cusl_",1:3),weights = mode_A))
# Create structural model
corp_rep_sm <- relationships(
  paths(from = c("COMP","LIKE"), to = c("CUSA","CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

## Estimation
# Estimate the model
corp_rep_pls_model <- estimate_pls(data = corp_rep_data,
                                   measurement_model = corp_rep_mm,
                                   structural_model = corp_rep_sm,
                                   inner_weights = path_weighting,
                                   missing = mean_replacement,
                                   missing_value = "-99")

## Summarizing the results
# Summarize the model results
summary_corp_rep <- summary(corp_rep_pls_model)
# Iterations to converge
summary_corp_rep$iterations

## Evaluation of reflective measurement model
# Inspect the indicator loadings
summary_corp_rep$loadings
# Inspect the indicator reliability
summary_corp_rep$loadings^2
# Inspect the Cronbachs alpha and composite reliability
summary_corp_rep$reliability
# Plot the reliabilities of constructs
plot(summary_corp_rep$reliability)
# Table of the FL criteria
summary_corp_rep$validity$fl_criteria
# HTMT criterion
summary_corp_rep$validity$htmt
# Bootstrap the model
boot_corp_rep <- bootstrap_model(seminr_model = corp_rep_pls_model,
                                 nboot = 1000,cores = NULL,seed = 123)
sum_boot_corp_rep <- summary(boot_corp_rep, alpha = 0.10)
# Extract the bootstrapped HTMT
sum_boot_corp_rep$bootstrapped_HTMT
