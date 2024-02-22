library(cSEM)
library(seminr)

############################################################
# Example of PLS-SEM algorithm
############################################################

# Get data
data(satisfaction)
# Selecting indicators 
sat_reduc <- satisfaction[,c("imag1", "imag2",
                             "expe1", "expe2",
                             "loy1", "loy2", "loy3")]

# Specifying the measurement model matrix
W <- matrix(c(1,0,0,
              1,0,0,
              0,1,0,
              0,1,0,
              0,0,1,
              0,0,1,
              0,0,1), 
            ncol = 3,
            byrow = TRUE)
# Specifying the structural model matrix
P <- matrix(c(0,0,1,
              0,0,1,
              0,0,0), 
            ncol = 3,
            byrow = TRUE)

#####
# Step 1: Initialization
#####

# Standardizing and transforming to matrix
X <- as.matrix(scale(sat_reduc))

# Creating initial latent variables with weights equal to one
Y_hat=X%*%W

for (i in 1:300){

#####
# Step 2, inner approximation
#####
# Using factorial weighting scheme
#P[1:2,3] <- cor(Y_hat)[1:2,3]
#C <- P+t(P)
# Using path weighting
P[1:2,3] <- coef(lm(Y_hat[,3]~0+ Y_hat[,1:2]))
C <- P+t(P)
# Each latent variable is created as a weighted sum of its neighboring LVs
Y_hat <- Y_hat%*%C
# Standardizing latent variables
Y_hat <- scale(Y_hat)

#####
# Step 3, outer approximation
#####

# The reflective part (mode A)
W[5,3]=coef(lm(X[,5] ~ Y_hat[,3]))[2]
W[6,3]=coef(lm(X[,6] ~ Y_hat[,3]))[2]
W[7,3]=coef(lm(X[,7] ~ Y_hat[,3]))[2]
# The formative part (mode B)
W[1:2,1]=coef(lm(Y_hat[,1] ~ X[,1:2]))[2:3]
W[3:4,2]=coef(lm(Y_hat[,2] ~ X[,3:4]))[2:3]
# Estimate LV approximations
Y_hat=X%*%W
# Standardizing latent variables
Y_hat <- scale(Y_hat)

#####
# Step 4, convergence of weights
#####
# To simplify the code this step is skipped, and instead we set the number
# of iteration sufficiently high an hope for convergence. E.g. number of iteration=300.
# Notice that the algorithm normally converge must faster than after 300 iteration,
# but just setting a high number of iterations also means that we cannot be sure that
# the algorithm actually did converge.

}

########
# Step 5: Final estimates
########
# Inner path coefficients
paths<-coef(lm(Y_hat[,3] ~ 0 + Y_hat[,1] + Y_hat[,2]))
# weights for formative models
weights<-c(coef(lm(Y_hat[,1] ~ 0 + X[,1] + X[,2])),
           coef(lm(Y_hat[,2] ~ 0 + X[,3] + X[,4])),
           coef(lm(Y_hat[,3] ~ 0 + X[,5] + X[,6] + X[,7]))
           )
# loadings for reflective model
loadings<-c(cor(Y_hat[,1],X[,1]),cor(Y_hat[,1],X[,2]),
            cor(Y_hat[,2],X[,3]),cor(Y_hat[,2],X[,4]),
            cor(Y_hat[,3],X[,5]),cor(Y_hat[,3],X[,6]),cor(Y_hat[,3],X[,7])
            )

############################################################
# Using seminR package
############################################################
# Original model ----------------------------------------------------------
# Specifying the measurement model 
simple_mm <- constructs(
  composite("IMAG", multi_items("imag", 1:2), weights = mode_B),
  composite("EXPE", multi_items("expe", 1:2), weights = mode_B),
  composite("LOY", multi_items("loy", 1:3), weights = mode_A))

# Specifying the structural model 
simple_sm <- relationships(
  paths(from = c("IMAG","EXPE"), to = c("LOY")))
# Estimate the model
sat_model <- estimate_pls(data = sat_reduc,
                          measurement_model = simple_mm,
                          structural_model = simple_sm,
#                          inner_weights = path_factorial,
                          inner_weights = path_weighting,
                          missing = mean_replacement,
                          missing_value = "-99")
# Summarize the model results
summary_sat_model <- summary(sat_model)
# Inspect the model's path coefficients and the R^2 values
summary_sat_model$paths
paths
# Inspect the indicator loadings
summary_sat_model$loadings
loadings
# Inspect the indicator weights
summary_sat_model$weights
weights