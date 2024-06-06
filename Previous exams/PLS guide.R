##########################################################################################
####################### Evaluation of Reflective Measurement Models #####################
#########################################################################################
library(seminr)
library(readr)
## Preparation
# Read in data 
df <- read.csv(file ="PLS_data_reexam.csv",header=TRUE,sep=",")
## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("SIC",multi_items("sic_",1:6),weights = mode_B),
  composite("PL",multi_items("pl_",1:4), weights = mode_A),
  composite("PQ",multi_items("pq_",1:4), weights = mode_A),
  composite("PI",multi_items("pi_",1:5), weights = mode_A),
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
                          missing_value = "-99")

summary_pls <- summary(pls_model)

# Inspect the indicator loadings
summary_pls$loadings
# Inspect the indicator reliability
summary_pls$loadings^2

# Inspect the composite reliability
summary_pls$reliability

# Table of the FL criteria
summary_pls$validity$fl_criteria

# HTMT criterion
summary_pls$validity$htmt

# Bootstrap the model
boot_pls <- bootstrap_model(seminr_model= pls_model,nboot= 1000,cores= parallel::detectCores(),seed= 123)
# Summarize the results of the bootstrap
summary_boot_pls <- summary(boot_pls,alpha= 0.05)
# Extract the bootstrapped HTMT
summary_boot_pls$bootstrapped_HTMT

##########################################################################################
####################### Evaluation of Formative Measurement Models #####################
#########################################################################################
## Preparation
# Read in data 
df <- read.csv(file ="PLS_data_reexam.csv",header=TRUE,sep=",")
## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("SIC",multi_items("sic_",1:6),weights = mode_B),
  composite("PL",multi_items("pl_",1:3), weights = mode_A),
  composite("PQ",multi_items("pq_",1:4), weights = mode_A),
  composite("PI",multi_items("pi_",1:5), weights = mode_A),
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
                          missing_value = "-99")

#Summary
summary_pls <- summary(pls_model)

# Bootstrap the model
boot_pls <- bootstrap_model(seminr_model= pls_model,nboot= 1000,cores= parallel::detectCores(),seed= 123)
# Summarize the results of the bootstrap
summary_boot_pls <- summary(boot_pls,alpha= 0.05)

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

# Inspect the bootstrapping results for indicator weights
summary_boot_pls$bootstrapped_weights

#Absolute importance
summary_boot_pls$bootstrapped_loadings
###################################################################################################
################################ Evaluation of the structural model ###############################
###################################################################################################
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
  composite("SIC",multi_items("sic_",1:6),weights = mode_B),
  composite("PL",multi_items("pl_",1:4), weights = mode_A),
  composite("PQ",multi_items("pq_",1:4), weights = mode_A),
  composite("PI",multi_items("pi_",1:5), weights = mode_A),
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
                          missing_value = "-99")

# Summarize the results of the model estimation
summary_pls <- summary(pls_model)
# Bootstrap the model
boot_pls <- bootstrap_model(seminr_model= pls_model,nboot= 1000,cores= parallel::detectCores(),seed= 123)
# Summarize the results of the bootstrap
summary_boot_pls <- summary(boot_pls,alpha= 0.05)

# Inspect the structural model collinearity VIF
summary_pls$vif_antecedents

# Inspect the structural paths
summary_boot_pls$bootstrapped_paths
# Inspect the total effects
summary_boot_pls$bootstrapped_total_paths
# Inspect the model RSquares
summary_pls$paths
# Inspect the effect sizes
summary_pls$fSquare

# Generate the model predictions
predict_pls_model<- predict_pls(model= pls_model,technique= predict_DA,noFolds= 10,reps= 10)
# Summarize the prediction results
sum_predict_pls <- summary(predict_pls_model)

# Analyze the distribution of prediction error
par(mfrow=c(1,3))
plot(sum_predict_pls,indicator = 'wtp')
par(mfrow=c(1,1))

# Compute the prediction statistics
sum_predict_pls

##########################################################################################
############################## Mediation analysis #######################################
#########################################################################################
## Preparation
# Create measurement model
simple_mm <- constructs(
  composite("SIC",multi_items("sic_",1:6),weights = mode_B),
  composite("PL",multi_items("pl_",1:4), weights = mode_A),
  composite("PQ",multi_items("pq_",1:4), weights = mode_A),
  composite("PI",multi_items("pi_",1:5), weights = mode_A),
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
                          missing_value = "-99")

# Summarize the results of the model estimation
summary_pls <- summary(pls_model)
# Bootstrap the model
boot_pls <- bootstrap_model(seminr_model= pls_model,nboot= 1000,cores= parallel::detectCores(),seed= 123)
# Summarize the results of the bootstrap
summary_boot_pls <- summary(boot_pls,alpha= 0.05)

# Inspect total indirect effects
summary_pls$total_indirect_effects

# Inspect indirect effects
specific_effect_significance(boot_pls,from= 'SIC', through ='PL', to = 'PI',alpha= 0.05)
specific_effect_significance(boot_pls,from= 'SIC', through= 'PQ', to= 'PI',alpha= 0.05)

# Inspect the direct effects
summary_pls$paths
# Inspect the confidence intervals for direct effects
summary_boot_pls$bootstrapped_paths

# Calculate the sign of p1*p2*p3
summary_pls$paths['SIC', 'PI']*summary_pls$paths['SIC','PQ']*summary_pls$paths['PQ','PI']
summary_pls$paths['SIC', 'PI']*summary_pls$paths['SIC','PL']*summary_pls$paths['PL','PI']

##########################################################################################
############################## Moderation analysis #######################################
#########################################################################################
demo(topic = 'seminr-pls-influencer', package = 'seminr')

# Bootstrap the model
boot_pls1 <- bootstrap_model(seminr_model= influencer_pls,nboot= 1000,cores= parallel::detectCores(),seed= 123)
# Summarize the results of the bootstrap
summary_boot_pls <- summary(boot_pls1,alpha= 0.05)
summary_pls2 <- summary(influencer_pls)

# Inspect the bootstrapped structural paths
summary_boot_pls$bootstrapped_paths

# Simple slope analysis plot
slope <- slope_analysis(moderated_model = influencer_pls,dv = 'PI',moderator = 'PIC',iv = 'PQ',leg_place = 'bottomright')
slope$text
