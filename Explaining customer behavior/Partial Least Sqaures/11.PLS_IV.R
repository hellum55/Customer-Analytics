# R script for the illustrations in chapter 5
rm(list=ls())
library(seminr)
##########################
### Mediation analysis ###
##########################
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
# Bootstrap the model
boot_corp_rep_ext <- bootstrap_model(seminr_model = corp_rep_pls_model_ext,
                                     nboot = 1000,cores = NULL,seed = 123)
# Summarize the results of the bootstrap
summary_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.10)

## Mediation analysis
# Inspect total indirect effects
summary_corp_rep_ext$total_indirect_effects
# Inspect indirect effects
specific_effect_significance(boot_corp_rep_ext, from = "COMP", through = "CUSA", 
                             to = "CUSL", alpha = 0.05)
specific_effect_significance(boot_corp_rep_ext, from = "LIKE", through = "CUSA",
                             to = "CUSL", alpha = 0.05)
# Inspect the direct effects
summary_corp_rep_ext$paths
# Inspect the confidence intervals for direct effects
summary_boot_corp_rep_ext$bootstrapped_paths
# Calculate the sign of p1*p2*p3
summary_corp_rep_ext$paths["LIKE", "CUSL"] *
  summary_corp_rep_ext$paths["LIKE","CUSA"] *
  summary_corp_rep_ext$paths["CUSA","CUSL"]

###########################
### Moderation analysis ###
###########################
## Preparation
# Create measurement model
corp_rep_mm_mod <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3), weights = mode_A),
  composite("LIKE", multi_items("like_", 1:3), weights = mode_A),
  composite("CUSA", single_item("cusa")),
  composite("SC",   multi_items("switch_", 1:4),  weights = mode_A),
  composite("CUSL", multi_items("cusl_", 1:3), weights = mode_A),
  interaction_term(iv = "CUSA", moderator = "SC", method = two_stage))
# Create structural model
corp_rep_sm_mod <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA","SC","CUSA*SC"), to = c("CUSL")))

## Estimation
# Estimate the model
corp_rep_pls_model_mod <- estimate_pls(data = corp_rep_data,
                                       measurement_model = corp_rep_mm_mod,
                                       structural_model = corp_rep_sm_mod,
                                       missing = mean_replacement,
                                       missing_value = "-99")

## Summarizing the results
# Summarize the model results
summary_corp_rep_mod <- summary(corp_rep_pls_model_mod)
# Bootstrap the model
boot_corp_rep_mod <- bootstrap_model(seminr_model = corp_rep_pls_model_mod,
                                     nboot = 1000,cores = NULL,seed = 123)
# Summarize the results of the bootstrap
summary_boot_corp_rep_mod <- summary(boot_corp_rep_mod, alpha = 0.10)

## Evaluation of the reflective measurement model
# Inspect the indicator loadings
summary_corp_rep_mod$loadings
# Inspect the indicator reliability
summary_corp_rep_mod$loadings^2
# Inspect the internal consistency reliability
summary_corp_rep_mod$reliability
# Table of the FL criteria
summary_corp_rep_mod$validity$fl_criteria
# HTMT criterion
summary_corp_rep_mod$validity$htmt
# Extract the bootstrapped HTMT
summary_boot_corp_rep_mod$bootstrapped_HTMT

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
summary_corp_rep_mod$validity$vif_items
# Summarize the results of the bootstrap
# alpha sets the specified level for significance, i.e. 0.05
sum_boot_corp_rep_mod <- summary(boot_corp_rep_mod, alpha = 0.05)
# Inspect the bootstrapping results for indicator weights
sum_boot_corp_rep_mod$bootstrapped_weights
# Inspect the bootstrapping results for indicator loadings
sum_boot_corp_rep_mod$bootstrapped_loadings

## Moderation analysis
# Inspect the bootstrapped structural paths
sum_boot_corp_rep_mod$bootstrapped_paths
# Simple slope analysis plot
slope_analysis(moderated_model = corp_rep_pls_model_mod,dv = "CUSL",
               moderator = "SC", iv = "CUSA", leg_place = "bottomright")



