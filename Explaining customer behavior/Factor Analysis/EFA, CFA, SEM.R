# .........
# Case study
# .........

# Data
Dataset <- readxl::read_excel("Dataset.xlsx")
read_excel("... /Dataset.xlsx")
View(Dataset) 
str(Dataset)

# Create a named vector for mapping text labels to numbers
label_to_number <- c("Totally disagree" = 1, 
                     "Disagree" = 2, 
                     "More or less disagree" = 3, 
                     "Neutral" = 4, 
                     "More or less agree" = 5, 
                     "Agree" = 6, 
                     "Totally agree" = 7)

# Loop through each column and convert
for (i in 2:26) {
  Dataset[[i]] <- label_to_number[Dataset[[i]]]
}


# libraries
library(lavaan)      # for sem
library(DataExplorer) # for descriptives
library(ppcor)       # to partial correlation
library(psych)       # for efa, cfa, sem, barlett test, KMO test
library(nFactors)    # for extracting the eigenvalues
library(semTools)    # for getting reliability (CR, AVE)
library(semPlot)     # for ploting sem

means <- sapply(Dataset[, c(2:26)], mean, na.rm = TRUE) 
sds <- sapply(Dataset[, c(2:26)], sd, na.rm = TRUE)
summary_table <- data.frame(Mean = means, SD = sds)  
(round(summary_table, 3))


# EFA in lavaan
cormatrix <- cor(Dataset[, c(2:26)])
round(cormatrix, 2)
pcor(Dataset[, c(2:26)])

print(cortest.bartlett(cor(Dataset[, c(2:26)]), nrow(Dataset[, c(2:26)])))
KMO(Dataset[, c(2:26)])

ev <- eigen(cor(Dataset[, c(2:26)]))
ev$values
plot(ev$values, type="line")  

fit1 = factanal(~ Q1 + Q2 + Q3 + Q4 + Q5 +
                  Q6 + Q7 + Q8 + Q9 + Q10 + 
                  Q11 + Q12 + Q13 + Q14 + Q15 +
                  Q16 + Q17 + Q18 + Q19 + Q20 +
                  Q21 + Q22 + Q23 + Q24 + Q25,
                factors = 7, data = Dataset, 
                lower=0.10, rotation = "varimax", method="ML")
print(fit1, sort=TRUE, cutoff=0.3)
# Obs. loadings are slightly different from what is reported in the paper
# because of different optimization method in SPSS


# EFA in psych
fmodel1 <- fa(Dataset[, c(2:26)],
              nfactors = 7, 
              fm="pa",
              rotate = "varimax")
print(fmodel1$loadings, digits=3, cutoff=0.3, sort=TRUE) 
# Obs. loadings are slightly different from what is reported in the paper
# because of different optimization method in SPSS


# CFA
CFA.model <- 'CPA =~ Q1 + Q2 + Q3 + Q4
             RA =~ Q5 + Q6 + Q7 + Q8 + Q9
             CPL =~ Q10 + Q11 + Q12
             TRI =~ Q13 + Q14 + Q15
             OBS =~ Q16 + Q17 + Q18
             ATT =~ Q19 + Q20 + Q21
             INT =~ Q22 + Q23 + Q24 + Q25'

# fit the model
fit <- cfa(CFA.model, data = Dataset)
# display summary output
summary(fit, fit.measures=TRUE, standardized = TRUE, modindices = FALSE)
# Obs. same loadings as in the paper
modificationindices(fit, sort = T, minimum.value = 10, op = "~~")

# reliability and convergent validity
matrixrel = semTools::reliability (fit)
matrixrel

# discriminant validity
semTools::discriminantValidity(fit, merge=TRUE) #corr in the first column
AVEs = matrixrel[5,]
sqrtAVEs = sqrt(AVEs)
sqrtAVEs # this values should be higher than corr

# plot
library(lavaanPlot)
lavaanPlot(model = fit, 
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE, 
           covs=TRUE, 
           stand=TRUE, 
           sig=.05, 
           stars="covs") 

# sem 
# H1-H3: Compatibility (CPA), Triability (TRI) and Attitude (ATT)
#     towards the app are positively related with 
#     customer´s continuance intention (INT)
# H4-H5: Perceived relative advantage (RA) of the app is positively related with 
#     continuance intention (INT) and the effect is mediated by 
#     the app triability (TRI). 
# H6-H7: Observability (OBS) is positively related with
#     continuance intention (INT) and the effect is mediated by
#     customer positive attitudes (ATT).

SEM.model1 <- '
# Measurement model
             CPA =~ Q1 + Q2 + Q3 + Q4
             RA =~ Q5 + Q6 + Q7 + Q8 + Q9
             CPL =~ Q10 + Q11 + Q12
             TRI =~ Q13 + Q14 + Q15
             OBS =~ Q16 + Q17 + Q18
             ATT =~ Q19 + Q20 + Q21
             INT =~ Q22 + Q23 + Q24 + Q25
# Structural model
        INT ~ CPA + TRI + ATT
        TRI ~ RA
        ATT ~ OBS'

# fit the model
fitSEM1 <- sem(SEM.model1, data=Dataset, estimator = "ML") 
summary(fitSEM1, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

# plot the model
semPaths(fitSEM1, "std", intercepts = FALSE, style="lisrel", layout="tree2")

# Results: significant evidence for all hypotheses

# Implications:
# H1-H3: The positive relationship between compatibility (CPA), 
# trialability (TRI), and attitude (ATT) towards the app 
# with customers' continuance intention (INT) underscores 
# the importance of creating a user-friendly app that aligns
# with users' needs and preferences. Businesses should focus
# on personalizing the user experience and offering trial 
# features that allow users to fully explore the app's 
# functionalities. This can lead to a more favorable 
# attitude toward the app, enhancing customer retention.


# H4-H5: The positive relationship between the perceived 
# relative advantage (RA) of the app and continuance intention
# mediated by trialability, indicates that introducing 
# innovative features and updates can significantly impact 
# users' decisions to continue using the app. 
# It also highlights the role of trialability in allowing 
# users to experience these advantages firsthand.

# H6-H7: The positive relationship between observability (OBS) 
# and continuance intention, mediated by positive attitudes (ATT),
# highlights the importance of social proof and word-of-mouth in 
# driving app retention. Observability refers to how visible the 
# benefits of using the app are to others, which can influence 
# potential users' attitudes and their decision to adopt and continue 
# using the app.

# Overall Business Implications

# Customer-Centric Approach: These findings emphasize the need for a 
# customer-centric approach in app development , focusing on 
# personalization, innovation, and community building.

# Strategic Product Development: Businesses should strategically 
# plan product development to ensure that new features and updates
# not only offer real value but are also easily trialable and observable 
# to users.

# Leverage Social Proof: By leveraging social proof through 
# observability and positive word-of-mouth, businesses can enhance 
# user attitudes and foster a supportive community that encourages 
# continued app usage.

# Conclusion 
# In conclusion, understanding and leveraging the relationships between 
# compatibility, trialability, perceived relative advantage, observability, 
# attitude, and continuance intention can provide businesses with strategic 
# insights into enhancing customer retention in the competitive app market.
