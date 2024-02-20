library(foreign)
library(lavaan)
####Load the data:
data <- readxl::read_excel("~/Cand. Merc./Customer-Analytics/Explaining customer behavior/Case Study/Dataset.xlsx")
data$No <- NULL
str(data)

variable_names <- names(data)
for (variable in variable_names) {
  data[[variable]] <- as.factor(data[[variable]])
  data[[variable]] <- as.numeric(data[[variable]])
}
summary(data)
mean(data$Q1)
####Firstly we will do the EFA analysis:
####Check if Exploratory factor analysis (EFA) applies
cormatrix <- cor(data[, c(1:25)])
round(cormatrix, 2)
#When there are 30 questions it is hard for the eye to see if the correlations are as they are supposed to be, but it will be obvious when we go further with 
#the analysis.

# Bartlett Sphericity Test. The null hypothesis is that the data dimension reduction is not possible. 
# If p-value < 0.05, we reject the null hypothesis and apply FA. This test is sensitive to N.
library(psych)
print(cortest.bartlett(cor(data[, c(1:25)]), nrow(data[, c(1:25)])))
#P-value is very low so we can reject the null-hypothesis - meaning that it is possible to reduce the dimension of the data set.

####Kaiser-Meyer-Oklin Test (KMO). MSO overall should be .60 or higher to proceed with factor analysis
KMO(data[, c(1:25)])
#Overall the MSA is 0.78 which means that we can proceed with factor analysis

#####Apply EFA with Common Factor Analysis
# Extracting as many factors as variables and check eigenvalues
library(nFactors)
ev <- eigen(cor(data[, c(1:25)]))
ev$values
# the first 7 factors have an eigenvalue >1
plot(ev$values, type="line")   
# the screeplot suggests 7-10 factors
# we go for 7 factors and see later if necessary to reduce this number

fit1 = factanal(~ Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11
                + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + 
                  Q21 + Q22 + Q23 + Q24 + Q25, factors = 7, data = data, lower=0.1, rotation = "varimax")
# factanal() function can analyze raw data or a correlation matrix or covariance matrix; 
# factanal() applies Maximum Likelihood by default. 
print(fit1, sort=TRUE, cutoff=0.2)
#Q9 seems to explain more than just one construct. The factor has loadings in 3 factors. (not good)
#Overall it looks quite all right. The ss-loadings are also above 1.

# Uniquesness for each item are 1-communality
1- apply(fit1$loadings^2,1,sum)
#fit2$uniquenesses
# Communalities
apply(fit1$loadings^2,1,sum)
# as one can observe, some items like x15 have very low communality

# Refining EFA without Q9
fit2 = factanal(~ Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q10 + Q11
                + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + 
                  Q21 + Q22 + Q23 + Q24 + Q25, factors = 7, data = data, lower=0.3, rotation = "varimax")
print(fit2, sort=TRUE, cutoff=0.3)
# Test of the hypothesis that 7 factors are sufficient.
# The chi square statistic is 194.7 on 129 degrees of freedom.
# The p-value is 0.000161. Which means p-value is smaller than before and
#chi is lower as well.


#Next, we set up a confirmatory factor analysis to confirm the measurement model (CFA). 
#Finally, given the measurement model has been examined and validated in the CFA analysis, we set up a SEM model, 
#to test the structural relationships between the four constructs identified and the customers´ likelihood to continue doing business with the company (X19-Satisfaction, X20-Likelihood of recommendation and X21-Likelihood of future purchase).
#### Confirmatory Factor Analyis (CFA)
CFA.model <- 'CPA =~ Q1 + Q2 + Q3 + Q4
             RA =~ Q5 + Q6 + Q7 + Q8 + Q9
             CPL =~ Q10 + Q11 + Q12
             TRI =~ Q13 + Q14 + Q15
             OBS =~ Q16 + Q17 + Q18
             ATT =~ Q19 + Q20 + Q21
             INT =~ Q22 + Q23 + Q24 + Q25
    # Correlations between exogeneous constructs are optional because
    # by default, all exogenous latent variables in a CFA model are allowed to correlate
        CPA ~~ RA
        CPA ~~ CPL
        CPA ~~ TRI
        CPA ~~ OBS
        CPA ~~ ATT
        CPA ~~ INT
        RA ~~ CPL
        RA ~~ TRI
        RA ~~ OBS
        RA ~~ ATT
        RA ~~ INT
        CPL ~~ TRI
        CPL ~~ OBS
        CPL ~~ ATT
        CPL ~~ INT
        TRI ~~ OBS
        TRI ~~ ATT
        TRI ~~ INT
        OBS ~~ ATT
        OBS ~~ INT
        ATT ~~ INT'
# fit the model
fit <- cfa(CFA.model, data = data)
# display summary output
summary(fit, fit.measures=TRUE, standardized = TRUE, modindices = FALSE)
modificationindices(fit, sort = T, minimum.value = 10, op = "~~")
# MI reveal that Q3 is correlated with Q1 and Q4; it means that Q3 has substantial cross-loading 
#on two factors (as in EFA). Cross-loading goes against one of the principles of unidimensionality in SEM. 
#We delete Q3 from the analysis and re-run CFA. 
set.seed(1234)
CFA.model <- 'CPA =~ Q1 + Q2 + Q3 + Q4
             RA =~ Q5 + Q6 + Q7 + Q8 + Q9
             CPL =~ Q10 + Q11 + Q12
             TRI =~ Q13 + Q14 + Q15
             OBS =~ Q16 + Q17 + Q18
             ATT =~ Q19 + Q20 + Q21
             INT =~ Q22 + Q23 + Q24 + Q25'
# fit the model
fit <- cfa(CFA.model, data = data)
# display summary output
summary(fit, fit.measures=TRUE, standardized = TRUE, modindices = FALSE)
# Examine the model fit; guidelines for a good model: 
#     CFI >.90, TLI=.95, RMSEA< 0.029, SRMR <.0.041. 

# Examine RELIABILITY of the factors - 
#   Reliability = degree of consistency between multiple measurements of a variable 
#   install.packages("semTools")
library(semTools)
semTools::reliability (fit)

# Examine DISCRIMINANT VALIDITY of the factors
# the latent variables can be thought of representing two distinct contructs.
discriminantValidity(fit, merge=TRUE)



