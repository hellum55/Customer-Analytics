# Access the demo file for the ECSI dataset
demo(topic = 'seminr-pls-influencer', package = 'seminr')

# Bootstrap the model
boot_pls1 <- bootstrap_model(seminr_model= influencer_pls,nboot= 1000,cores= parallel::detectCores(),seed= 123)
# Summarize the results of the bootstrap
summary_boot_pls <- summary(boot_pls,alpha= 0.05)

summary_pls2 <- summary(influencer_pls)
summary_pls2$total_indirect_effects
specific_effect_significance(boot_pls1,from= 'SIC', through = c('PL','PQ'), to = 'WTP',alpha= 0.05)


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