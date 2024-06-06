#Part 2 - Question 2:
rm(list=ls())
library(seminr)
library(readr)
## Preparation
# Read in data 
df <- read.csv(file ="PLS_data_exam2023.csv",header=TRUE,sep=",")
head(df)

## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("SIC",multi_items("sic_",1:6),weights = mode_B),
  composite("PL",multi_items("pl_",1:4)),
  composite("PQ",multi_items("pq_",1:4)),
  composite("PI",multi_items("pi_",1:5)),
  composite("WTP",single_item("wtp")))

# Create structural model
simple_sm <- relationships(paths(from = c("SIC"), to = c("PL","PQ", "PI")),
                           paths(from = c("PL", "PQ"), to = c("PI")),
                           paths(from = c("PI"), to = c("WTP")))

# Estimate the model with default settings
pls_model <- estimate_pls(data = df,
                                      measurement_model = simple_mm,
                                      structural_model = simple_sm,
                                      inner_weights = path_weighting,
                                      missing = mean_replacement,
                                      missing_value = "NA")
# Summarize the model results
summary_pls <- summary(pls_model)
# Iterations to converge
summary_pls$iterations

# Inspect the indicator loadings
summary_pls$loadings
# Inspect the indicator reliability
summary_pls$loadings^2

summi <- summary(influencer_pls)
summi$loadings
# Inspect the composite reliability
summary_pls$reliability

# HTMT criterion
summary_pls$validity$htmt

# Bootstrap the model
boot_pls <- bootstrap_model(seminr_model= pls_model,nboot= 1000)
sum_boot <- summary(boot_pls, alpha = 0.10)

# Extract the bootstrapped HTMT
sum_boot$bootstrapped_HTMT
