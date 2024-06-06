#Part 2 - Question 2:
rm(list=ls())
library(seminr)
library(readr)
## Preparation
# Read in data 
df <- read.csv(file ="PLS_data_reexam.csv",header=TRUE,sep=",")
head(df)

## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("SIC",multi_items("sic_",1:6, 'global_sic'),weights = mode_B),
  composite("PL",multi_items("pl_",1:3)),
  composite("PQ",multi_items("pq_",1:4)),
  composite("PI",multi_items("pi_",1:5)),
  composite("WTP",single_item("wtp")))

# Create structural model
simple_sm <- relationships(paths(from = c("SIC"), to = c("PL","PQ", "PI")),
                           paths(from = c("PL", "PQ"), to = c("PI")),
                           paths(from = c("PI"), to = c("WTP")))
# Estimating the model
# - note, the influencer_data dataset is bundled with seminr
# Estimate the model with default settings
pls_model <- estimate_pls(data = df,
                          measurement_model = simple_mm,
                          structural_model = simple_sm,
                          inner_weights = path_weighting,
                          missing = mean_replacement,
                          missing_value = "NA")
# Summarize the model results
summary_pls <- summary(pls_model)

# Bootstrap the model
boot_pls <- bootstrap_model(seminr_model = pls_model, nboot = 1000,cores= parallel::detectCores(),seed= 123)
# Store the summary of the bootstrapped model
sum_boot_pls <- summary(boot_pls,alpha= 0.10)

# Redundancy analysis
# SIC 
# Create measurement model
sic_redundancy_mm <- constructs(composite('SIC', multi_items('sic_', 1:6),weights= mode_B),
                                composite('SIC_G', single_item('global_sic')))
# Create structural model
sic_redundancy_sm <- relationships(paths(from= c('SIC'),to= c('SIC_G')))
# Estimate the model
sic_redundancy_pls_model <- estimate_pls(data= df,
                                         measurement_model= sic_redundancy_mm,
                                         structural_model= sic_redundancy_sm,
                                         missing= mean_replacement,
                                         missing_value= '-99')
# Summarize the model
sum_sic_red_model <- summary(sic_redundancy_pls_model)
# Check the path coefficients for convergent validity
sum_sic_red_model$paths

# Collinearity analysis
summary_pls$validity$vif_items

# Summarize the results of the bootstrap
# alpha sets the specified level for significance, i.e. 0.05
sum_boot_pls <- summary(boot_pls,alpha= 0.05)
# Inspect the bootstrapping results for indicator weights
sum_boot_pls$bootstrapped_weights

sum_boot_pls$bootstrapped_loadings
