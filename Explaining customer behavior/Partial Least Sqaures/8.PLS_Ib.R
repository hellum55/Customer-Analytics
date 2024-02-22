# R script for the illustrations in chapter 3
rm(list=ls())
library(seminr)
# Read in data and inspect the first few observations
corp_rep_data <- read.csv(file ="Corporate Reputation Data.csv",header=TRUE,sep=";")
head(corp_rep_data)

## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("COMP",multi_items("comp_",1:3),weights = mode_A),
  composite("LIKE",multi_items("like_",1:3),weights = mode_A),
  composite("CUSA",single_item("cusa")),
  composite("CUSL",multi_items("cusl_",1:3),weights = mode_A))
# Create structural model
simple_sm <- relationships(paths(from = c("COMP","LIKE"), to = c("CUSA","CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

## Estimation
# Estimate the model
corp_rep_simple_model <- estimate_pls(data = corp_rep_data,
                                      measurement_model = simple_mm,
                                      structural_model = simple_sm,
                                      inner_weights = path_weighting,
                                      missing = mean_replacement,
                                      missing_value = "-99")
# Estimate the model with default settings
corp_rep_simple_model <- estimate_pls(data = corp_rep_data,
                                      measurement_model = simple_mm,
                                      structural_model = simple_sm,
                                      missing_value = "-99")

## Summarizing the results
# Summarize the model results
summary_simple_corp_rep <- summary(corp_rep_simple_model)
# Iterations to converge
summary_simple_corp_rep$iterations
# Inspect the model's loadings
summary_simple_corp_rep$loadings
# Inspect the model's path coefficients and the R^2 values
summary_simple_corp_rep$paths
# Bootstrap the model
boot_simple_corp_rep <- bootstrap_model(seminr_model = corp_rep_simple_model,
                                        nboot = 1000,cores = NULL, seed = 123)
# Store the summary of the bootstrapped model
sum_boot_simple_corp_rep <- summary(boot_simple_corp_rep)
# Inspect the bootstrapped indicator loadings
sum_boot_simple_corp_rep$bootstrapped_loadings
# Inspect the bootstrapped structural paths
sum_boot_simple_corp_rep$bootstrapped_paths
