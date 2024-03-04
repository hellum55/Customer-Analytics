# R script for the illustrations in chapter 5
rm(list=ls())
library(seminr)

## Preparation
# Read in data 
corp_rep_data <- read.csv(file ="Corporate Reputation Data.csv",header=TRUE,sep=";")
# Create measurement model
corp_rep_mm_ext <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3), weights = mode_A),
  composite("LIKE", multi_items("like_", 1:3), weights = mode_A),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3), weights = mode_A))
# Create structural model
corp_rep_sm_ext <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

## Estimation
# Estimate the model
corp_rep_pls_model_ext <- estimate_pls(data = corp_rep_data,
                                       measurement_model = corp_rep_mm_ext,
                                       structural_model = corp_rep_sm_ext,
                                       missing = mean_replacement,
                                       missing_value = "-99")
## Summarizing the results
# Summarize the model results
summary_corp_rep_ext <- summary(corp_rep_pls_model_ext)
# Missingness
summary_corp_rep_ext$descriptives$statistics
# Iterations to converge
summary_corp_rep_ext$iterations

## Evaluation of the reflective measurement model
# Inspect the indicator loadings
summary_corp_rep_ext$loadings
# Inspect the indicator reliability
summary_corp_rep_ext$loadings^2
# Inspect the internal consistency reliability
summary_corp_rep_ext$reliability
# Table of the FL criteria
summary_corp_rep_ext$validity$fl_criteria
# HTMT criterion
summary_corp_rep_ext$validity$htmt
# Bootstrap the model
boot_corp_rep_ext <- bootstrap_model(seminr_model = corp_rep_pls_model_ext,
                                     nboot = 1000,cores = NULL,seed = 123)
sum_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.10)
# Extract the bootstrapped HTMT
sum_boot_corp_rep_ext$bootstrapped_HTMT

## Evaluation of the formative measurement model
# Redundancy analysis
# ATTR
# Create measurement model
ATTR_redundancy_mm <- constructs(
  composite("ATTR_F", multi_items("attr_", 1:3), weights = mode_B),
  composite("ATTR_G", single_item("attr_global")))
# Create structural model
ATTR_redundancy_sm <- relationships(
  paths(from = c("ATTR_F"), to = c("ATTR_G")))
# Estimate the model
ATTR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = ATTR_redundancy_mm,
                                          structural_model = ATTR_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")
# Summarize the model
sum_ATTR_red_model <- summary(ATTR_redundancy_pls_model)
# CSOR
# Create measurement model
CSOR_redundancy_mm <- constructs(
  composite("CSOR_F", multi_items("csor_", 1:5), weights = mode_B),
  composite("CSOR_G", single_item("csor_global")))
# Create structural model
CSOR_redundancy_sm <- relationships(
  paths(from = c("CSOR_F"), to = c("CSOR_G")))
# Estimate the model
CSOR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = CSOR_redundancy_mm,
                                          structural_model = CSOR_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")
# Summarize the model
sum_CSOR_red_model <- summary(CSOR_redundancy_pls_model)
# PERF
# Create measurement model
PERF_redundancy_mm <- constructs(
  composite("PERF_F", multi_items("perf_", 1:5), weights = mode_B),
  composite("PERF_G", single_item("perf_global")))
# Create structural model
PERF_redundancy_sm <- relationships(
  paths(from = c("PERF_F"), to = c("PERF_G")))
# Estimate the model
PERF_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = PERF_redundancy_mm,
                                          structural_model = PERF_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")
# Summarize the model
sum_PERF_red_model <- summary(PERF_redundancy_pls_model)
# QUAL
# Create measurement model
QUAL_redundancy_mm <- constructs(
  composite("QUAL_F", multi_items("qual_", 1:8), weights = mode_B),
  composite("QUAL_G", single_item("qual_global")))
# Create structural model
QUAL_redundancy_sm <- relationships(
  paths(from = c("QUAL_F"), to = c("QUAL_G")))
# Estimate the model
QUAL_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = QUAL_redundancy_mm,
                                          structural_model = QUAL_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")
# Summarize the model
sum_QUAL_red_model <- summary(QUAL_redundancy_pls_model)
# Check the path coefficients for convergent validity
sum_ATTR_red_model$paths
sum_CSOR_red_model$paths
sum_PERF_red_model$paths
sum_QUAL_red_model$paths
# Collinearity analysis
summary_corp_rep_ext$validity$vif_items
# Assess indicator weights for significance and relevance
# Summarize the results of the bootstrap
# alpha sets the specified level for significance, i.e. 0.05
sum_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.05)
# Inspect the bootstrapping results for indicator weights
sum_boot_corp_rep_ext$bootstrapped_weights
# Inspect the bootstrapping results for indicator loadings
sum_boot_corp_rep_ext$bootstrapped_loadings