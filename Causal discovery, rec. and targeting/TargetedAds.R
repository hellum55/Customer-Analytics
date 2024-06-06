# -----------------------------------------
# Bayesian Nets, Course Customer Analytics
# -----------------------------------------



################################################################################
# Application 2: 
# Identifying populations with Positive Expected Lift in Profit (ELP)
# and targeting
################################################################################
# The idea is to use BN to identify segments of customers that will most likely 
# purchase when sending the ad (persuadable segments) and avoid sending the ad 
# to the rest (to the ones who will not buy the advertised product ever, 
# to the ones who will be offended by receiving an unwanted advertisement or 
# call, or to the ones who will always buy)

# If historical data is available, we start learning the relationships between 
# the variables train and select the best model as a preliminary step. 
# First we use the model structure from the text (given),and we only have to 
# learn the parameters (probabilities).
# Next, we focus exclusively on how model is used as a decision support for 
# marketing managers.


library(bnlearn)
library(Rgraphviz) 


targeted.adv <- read.csv("targeted.adv.csv", header = T, colClasses = "factor")
head (targeted.adv)
str (targeted.adv)  


# Build the structure
dagTA <- model2network("[Income][Sex][Mailed][Buy|Income:Sex:Mailed]")
plot(dagTA)


# Learn the parameters
bnTA.mle <- bn.fit (dagTA, data = targeted.adv[, c(2:5)], method = "mle")
bnTA.mle


# ELP
# ELP = P(Buy = Yes| Mailed = yes) * r_s  - P(Buy = Yes| Mailed = no) r_u - c, 
# for any given population Y, where:
#  - c = cost of mailing the ad to a give person
#  - r_u = the income obtained from a sale to en unsolicited customer
#  - r_c = the income obtained from a sale to en solicited customer
#  - r_u and r_s are different because we may offer some discount in the ad


# Asumming 
c=0.5
r_s = 8
r_u = 10

# a. ) Compute the ELP for the population consisting of individuals with medium 
# income who are male. Should we mail an ad to this population?

# set evidence and get the ctp 
library (gRain)
junctionTA <- compile (as.grain(bnTA.mle))
Med_male_yes <- setEvidence (junctionTA, nodes = c("Income", "Sex", "Mailed"), 
                             states = c("medium", "male", "yes"))
querygrain(Med_male_yes, nodes = "Buy")
# p(buy) = 0.4
Med_male_no <- setEvidence (junctionTA, nodes = c("Income", "Sex", "Mailed"), 
                            states = c("medium", "male", "no"))
querygrain(Med_male_no, nodes = "Buy")
# p(buy) = 0.2

options(digits=2)
ELP = querygrain(Med_male_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain(Med_male_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# Since the ELP is positive, we may decide to mail to this population



# Asumming 
c=0.6
r_s = 7
r_u = 9

# b. ) Compute the ELP for the population consisting of individuals with medium 
# income who are female. Should we mail the ad to this population?

Med_fem_yes <- setEvidence (junctionTA, nodes = c("Income", "Sex", "Mailed"), 
                            states = c("medium", "female", "yes"))
querygrain(Med_fem_yes, nodes = "Buy")
# p(buy) = 0.7
Med_fem_no <- setEvidence (junctionTA, nodes = c("Income", "Sex", "Mailed"), 
                           states = c("medium", "female", "no"))
querygrain(Med_fem_no, nodes = "Buy")
# p(buy) = 0.4

options(digits=2)
ELP = querygrain(Med_fem_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain(Med_fem_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# Since the ELP is positive, we mail to this population



# c. ) Finally, let us compute the ELP for the population consisting of 
# individuals with low income. Should we mail an ad this population?

Low_yes <- setEvidence (junctionTA, nodes = c("Income", "Mailed"), 
                        states = c("low", "yes"))
querygrain( Low_yes, nodes = "Buy")
# 0.6
Low_no <- setEvidence (junctionTA, nodes = c("Income", "Mailed"), 
                       states = c("low", "no"))
querygrain( Low_no, nodes = "Buy")
# 0.5

options(digits=2)
ELP =  querygrain( Low_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain( Low_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# Since the ELP is negative, we do not mail to this population


# Discussion: 
# Using BN, this application allows to identify persuadable segments of 
# individuals who would buy only if they are sent an ad. It avoids sending 
# ads to those who will never buy, those who always buy (thus avoid wasting the 
# ad), and those who are turned off by the advertisement when they receive it.
# The network can be extended with more nodes according to the characteristics 
# of the population in the dataset.

################################################################################
# Application 2_beta (extension)
################################################################################

targeted.adv.beta <- read.csv("simulated_targeted_adv_data.csv", header = T, colClasses = "factor")
head (targeted.adv.beta)
str (targeted.adv.beta)  

# Build the structure
nb_structure <- tree.bayes(targeted.adv.beta[, -1], "Buy")
plot(nb_structure)

# Learn the parameters
bnTA.mle <- bn.fit (nb_structure, data = targeted.adv.beta[, -1], method = "mle")
bnTA.mle

# Asumming 
c=0.5
r_s = 8
r_u = 10

# a. ) Compute the ELP for the population consisting of individuals with the 
# following characteristics: 


# Married, using desktop, average age
library (gRain)
junctionTA <- compile (as.grain(bnTA.mle))
Query_yes <- setEvidence (junctionTA, nodes = c("Marital.Status", 
                                                "Device.Usage",
                                                   "Age", 
                                                  "Mailed"), 
                             states = c("Married", 
                                        "Desktop",
                                        "33-54",
                                        "Yes"))
querygrain(Query_yes, nodes = "Buy")


Query_no<- setEvidence (junctionTA, nodes = c("Marital.Status", 
                                                "Device.Usage",
                                                "Age", 
                                                "Mailed"), 
                          states = c("Married", 
                                     "Desktop",
                                     "33-54",
                                     "No"))
querygrain(Query_no, nodes = "Buy")

options(digits=2)
ELP = querygrain(Query_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain(Query_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# Since the ELP is positive and high, we mail to this population




# Single, using desktop, young age 
Query_yes <- setEvidence (junctionTA, nodes = c("Marital.Status", 
                                                "Device.Usage",
                                                "Age", 
                                                "Mailed"), 
                          states = c("Single", 
                                     "Desktop",
                                     "18-24",
                                     "Yes"))
querygrain(Query_yes, nodes = "Buy")


Query_no <- setEvidence (junctionTA, nodes = c("Marital.Status", 
                                                "Device.Usage",
                                                "Age", 
                                                "Mailed"), 
                          states = c("Single", 
                                     "Desktop",
                                     "18-24",
                                     "No"))
querygrain(Query_no, nodes = "Buy")

options(digits=2)
ELP = querygrain(Query_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain(Query_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# Since the ELP is negative, we do not mail to this population



# Rural, Blue-Collar, with Online Behavior: Low
# [Hypothesis: Individuals in rural areas with low engagement in online 
# activities might also neglect advertisements, 
# possibly due to a general disinterest in advertising or a preference for local,
# in-person shopping experiences]


Query_yes <- setEvidence (junctionTA, nodes = c("Geographic.Location", 
                                                "Occupation",
                                                "Social.Media.Activity",
                                                "Mailed"), 
                          states = c("Rural", 
                                     "Blue-Collar",
                                     "Low",
                                     "Yes"))
querygrain(Query_yes, nodes = "Buy")


Query_no <- setEvidence (junctionTA, nodes = c("Geographic.Location", 
                                               "Occupation",
                                               "Social.Media.Activity",
                                               "Mailed"), 
                         states = c("Rural", 
                                    "Blue-Collar",
                                    "Low",
                                    "No"))
querygrain(Query_no, nodes = "Buy")


options(digits=2)
ELP = querygrain(Query_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain(Query_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# Since the ELP is positive, we may decide to mail to this population. 
# As a note, the ELP is much lower than for other populations tested,
# so once may can make the decision also based on some minimum ELP



# Married, Desktop, 45-54 years old, White-Collar, Suburban
# [Hypothesis: This segment, despite being in their prime earning years and 
# possibly having a stable financial situation, might exhibit a lower 
# propensity to engage with direct advertisements.] 


Query_yes <- setEvidence (junctionTA, nodes = c("Marital.Status", 
                                                "Device.Usage",
                                                "Age",
                                                "Occupation",
                                                "Mailed"), 
                          states = c("Married",
                                     "Desktop",
                                     "45-54",
                                     "White-Collar",
                                     "Yes"))
querygrain(Query_yes, nodes = "Buy")


Query_no <- setEvidence (junctionTA, nodes = c("Marital.Status", 
                                               "Device.Usage",
                                               "Age",
                                               "Occupation",
                                               "Mailed"), 
                         states = c("Married",
                                    "Desktop",
                                    "45-54",
                                    "White-Collar",
                                    "No"))
querygrain(Query_no, nodes = "Buy")


options(digits=2)
ELP = querygrain(Query_yes, nodes = "Buy")$Buy[[2]] * r_s -
  querygrain(Query_no, nodes = "Buy")$Buy[[2]] * r_u - c
ELP
# contradictory to our hypothesis, since the ELP is positive and high, 
# we mail to this population




