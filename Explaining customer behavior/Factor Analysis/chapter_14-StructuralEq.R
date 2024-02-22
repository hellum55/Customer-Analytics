
# This is an extended script of Chapter_14-StructuralEq.R
# This file contains main code from chapter 14 in 
# Mehmetoglu & Mittner (2021). Applied Statistics Using R. SAGE.
# + more discussion 
# + moderation effects (multigroup analysis)



# to extract data
# install.packages("devtools")
# devtools::install_github("ihrke/astatur")


## setup
library(tidyverse)
library(astatur) 
library(lavaan)

# data
workout2<- astatur::workout2
str(workout2)

# read hypotheses (p. 395) and poposed model (p. 396)

## cfa
meas.lpa.mod <- '
                Attractive =~ face + sexy
                Appearance =~ body + appear + attract
                Muscle =~ muscle + strength + endur
                Weight =~ lweight + calories + cweight
                '
est.meas.lpa.mod <- cfa(meas.lpa.mod, data=workout2)
summary(est.meas.lpa.mod, fit.measures=TRUE, standardized=TRUE)
# fit not acceptable 
modindices(est.meas.lpa.mod, minimum.value = 3.84)
# 107     muscle ~~    endur 20.263 -0.553  -0.553   -0.585   -0.585
# 90        body ~~  lweight 16.845 -0.285  -0.285   -0.354   -0.354
# 111   strength ~~    endur 27.772  0.577   0.577    1.042    1.042. # high MI but not mentioned in the textbook



## modified cfa (allowing correlations between the error variances - this is not
## considered good practice and it reveals the data is not good quality - but we
# keep to the case study for this demo)
meas.lpa.mod2 <- '
                Attractive =~ face + sexy
                Appearance =~ body + appear + attract
                Muscle =~ muscle + strength + endur
                Weight =~ lweight + calories + cweight
                muscle ~~ endur 
                lweight ~~ body'
# strength ~~ endur is not incl. to avoid negative variance


est.meas.lpa.mod2 <- cfa(meas.lpa.mod2, data=workout2)
summary(est.meas.lpa.mod2, fit.measures=TRUE, standardized=TRUE)
# acceptable fit


# convergent and discriminnat analysis, and scale reliability
# loadings all high and sig.
condisc(est.meas.lpa.mod2)
relicoef(est.meas.lpa.mod2)
# all tests are in line with recommendations


# structural part
full.lpa.mod <- '
              #Measurement model 
                Attractive =~ face + sexy
                Appearance =~ body + appear + attract
                Muscle =~ muscle + strength + endur
                Weight =~ lweight + calories + cweight
                muscle ~~ endur 
                lweight ~~ body
                Muscle ~~ 0*Weight #set covariance to 0          # note extra assumption
              #Structural model 
                Appearance ~ Attractive
                Muscle ~ Appearance
                Weight ~ Appearance
                '
est.full.lpa.mod <- sem(full.lpa.mod, data=workout2)
summary(est.full.lpa.mod, fit.measures=TRUE, standardized=TRUE)
# check fit

# Go to output Regressions: and interpret sign, significance and size of the path coefficients
# here basically we assess our theoretical hypotheses
# guidelines: 
  # 0.09 small effects
  # 0.1-0.2 medium effects
  # 0.2 and above large effects 


#get R-squares
inspect(est.full.lpa.mod, what="rsquare")

# plot
library(lavaanPlot)
lavaanPlot(model = est.full.lpa.mod, 
           node_options = list(shape = "box", 
                               fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE, covs=TRUE, 
           stand=TRUE, sig=.05, stars="regress")
# - summarize the findings 



# assess indirect effects and mediation 
full.lpa.mod2 <- '
              #Measurement model (latent variables)
                Attractive =~ face + sexy
                Appearance =~ body + appear + attract
                Muscle =~ muscle + strength + endur
                Weight =~ lweight + calories + cweight
                muscle ~~ endur 
                lweight ~~ body
                Muscle ~~ 0*Weight #set covariance to 0
              #Structural model (regressions)
                Appearance ~ a*Attractive
                Muscle ~ b1*Appearance
                Weight ~ b2*Appearance
              #Indirect effects
                #of Attraction on Muscle
                ind1 := a*b1 
                #of Attraction on Weight
                ind2 := a*b2 
                '
est.full.lpa.mod2 <- sem(full.lpa.mod2, data=workout2)
summary(est.full.lpa.mod2, standardized=TRUE)

# recheck with se="bootstrap"
est.full.lpa.mod2 <- sem(full.lpa.mod2, data=workout2, se="bootstrap")
summary(est.full.lpa.mod2, standardized=TRUE)




# assess moderating effects (moderation analysis)
# If we have two groups of customers, women and men and want to see the differences between them
# we can implement multigroups analysis 

# for this example, I created a fictitious variable representing women and men in our sample
# also I imputed the missing values 
# upload the new data
workout2_imputed <- read.csv(".../workout2_imputed.csv")


# Generate random binary variable
# workout2$gender <- sample(0:1, nrow(workout2), replace = TRUE)
# also as we have many missing I will impute with knn the values for this demo
# library(mice)
# imputed_data <- complete(mice(workout2, method = "pmm", m = 5))
# write.csv(imputed_data, file="workout2_imputed.csv", row.names = FALSE)


# cfa multigroup to assess configural invariance
est.meas.lpa.mod_configural <- cfa(meas.lpa.mod, 
                         data=imputed_data,
                         group = "gender")
summary(est.meas.lpa.mod_configural, fit.measures=TRUE, standardized=TRUE)
# check output - not excellent but we have very small sample in each group
# let us consider it acceptable.
# We save this for later:
    # Test statistic                               280.675
    # Degrees of freedom                                76
    


# modified cfa multigroup - metric invariance (factor loadings are equal between groups)
est.meas.lpa.mod_metric <- cfa(meas.lpa.mod, 
                            data=imputed_data,
                            group = "gender",
                            group.equal = c("loadings"))
summary(est.meas.lpa.mod_metric, fit.measures=TRUE, standardized=TRUE)

# Test statistic                               288.578
# Degrees of freedom                                83 


# Test of difference in fit
(Chisquare_Dif = 288.578 - 280.675)
#[1] 7.903
(df_Dif=83-76)
#[1] 7
(p_value <- 1 - pchisq(7.903, 7))
# 0.341225 
# Increase in chi-square is n.s., meaning that a model imposing equal loadings
# performs equally good. Thus the loadings are not sig. different between groups
# Therefore metric invariance is fulfilled.



# once configureal and metric invariance are established, we test SEM multi-group
est.full.lpa.mod_MG <- sem(full.lpa.mod, 
                           data=imputed_data, 
                           group = "gender")
summary(est.full.lpa.mod_MG)


# Go to output Regressions: and evaluate the size and sig. of path coefficients
# Are they significantly different in the two groups? 
# In other words, is gender a moderator?

# Impose them to be equal and assess the modification in model fit (chi-square dif.)
est.full.lpa.mod_eqpath <- sem(full.lpa.mod, 
                           data=imputed_data, 
                           group = "gender", 
                           group.equal = c("regressions"))
summary(est.full.lpa.mod_eqpath)



# Test of difference in fit
(Chisquare_Dif = 224.756 - 222.613)
#[1] 2.143
(df_Dif=79-76)
#[1] 3
(p_value <- 1 - pchisq(2.143, 3))
# 0.53
# Conclusion: the increase in chi-square is n.s., meaning the model imposing equal path
# coeficients is not signbiifcantly worse than the standard model 
# Therefore, we conclude that paths are not significantly different between groups.
# In other words, gender does not moderate the relationships between constructs.
# For both women and men the relationships between constructs are the same.
# Note: recall that we created gender. 
# This is a demo and does not mean the results correspond to reality.



