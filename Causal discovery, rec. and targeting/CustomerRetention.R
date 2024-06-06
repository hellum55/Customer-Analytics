# -----------------------------------------
# Bayesian Nets, Course Customer Analytics
# March 2024
# AAT 
# -----------------------------------------



#####################################
# Application 1: Customer Retention 
#####################################
# 1. Building the structure manually and introducing probabilities manually - 
# 2. Learning the structure and the probabilities from data - most common
# 3. Model evaluation
# 4. Making forward and backward inference 

##################################################
# 1. Building the structure and parameters manually 
##################################################
library(bnlearn)
install.packages("rtools")
# Create an empty graph 
dag <- empty.graph(nodes = c("Fuse","Plea","Atti","Comm"))
# Add the arcs that encode the direct dependencies between variables
dag <- set.arc (dag, from = "Fuse", to = "Atti")
dag <- set.arc (dag, from = "Plea", to = "Atti")
dag <- set.arc (dag, from = "Fuse", to = "Comm")
dag <- set.arc (dag, from = "Plea", to = "Comm")
dag <- set.arc (dag, from = "Atti", to = "Comm")
# Print 
dag
# Direct dependencies are listed for each variable
modelstring(dag)
# Explore the elements of the graphical net
nodes(dag)
arcs(dag)
plot(dag)
# Optional library Rgraphviz (see instructions on Blackboard on how to install it)
library(Rgraphviz) 
graphviz.plot(dag)

# Another way to build a large network from scratch is to define the nodes 
# and create a matrix to set the whole arc set at once:
dag2 <- empty.graph(nodes = c("Fuse","Plea","Atti","Comm"))
arcs(dag2) = matrix (c("Fuse", "Atti",
                       "Plea", "Atti",
                       "Fuse", "Comm",
                       "Plea", "Comm",
                       "Atti", "Comm"),
                     byrow  = TRUE, ncol = 2,
                     dimnames = list (NULL, c("from", "to")))
plot(dag2)

#  A easy way to build the DAG when we know the structure:
dag3 <- model2network("[Fuse][Plea][Atti|Fuse:Plea][Comm|Fuse:Plea:Atti]")
plot(dag3)
# graphviz.plot (dag3)


# Compare dags
all.equal(dag, dag2)
all.equal(dag, dag3)


# Introducing the parameters manually
Fuse.lv <- c ("Low", "Med", "High") 
Plea.lv <- c ("Low", "Med", "High")
Atti.lv <- c ("Low", "Med", "High")
Comm.lv <- c ("Low", "Med", "High")


Fuse.prob <- array (c(0.02, 0.26, 0.72), dim = 3, dimnames = list (Fuse = Fuse.lv))
Fuse.prob
Plea.prob <- array (c(0.01, 0.55, 0.44), dim = 3, dimnames= list (Plea = Plea.lv))
Plea.prob
Atti.prob <- array (c(0.99, 0.01, 0.00,
                          0.00, 0.67, 0.33,
                          0.01, 0.99, 0.00,
                          0.34, 0.33, 0.33, 
                          0.00, 0.79, 0.21,
                          0.00, 0.40, 0.60,
                          0.99, 0.01, 0.00,
                          0.00, 0.47, 0.53,
                          0.00, 0.09, 0.91), dim = c (3, 3, 3), dimnames= list (Atti = Atti.lv, Plea = Plea.lv, Fuse = Fuse.lv))
Atti.prob

Comm.prob <- array (c(0.00, 1.00, 0.00, 
                        0.34, 0.33, 0.33, 
                        0.34, 0.33, 0.33, 
                        0.34, 0.33, 0.33, 
                        0.00, 1.00, 0.00, 
                        1.00, 0.00, 0.00, 
                        0.34, 0.33, 0.33,
                        0.00, 1.00, 0.00, 
                        0.34, 0.33, 0.33,
                        0.34, 0.33, 0.33,
                        0.34, 0.33, 0.33,
                        0.34, 0.33, 0.33,
                        0.34, 0.33, 0.33,
                        0.00, 0.98, 0.02,
                        0.00, 0.83, 0.17,
                        0.34, 0.33, 0.33,
                        0.00, 0.33, 0.67,
                        0.00, 0.44, 0.56,
                        1.00, 0.00, 0.00,
                        0.34, 0.33, 0.33,
                        0.34, 0.33, 0.33,
                        0.34, 0.33, 0.33,
                        0.00, 0.84, 0.16,
                        0.00, 0.71, 0.29,
                        0.34, 0.33, 0.33,
                        0.00, 0.40, 0.60,
                        0.00, 0.10, 0.90), 
                 dim = c (3, 3, 3, 3), dimnames= list (Comm = Comm.lv,  Atti = Atti.lv, Plea = Plea.lv, Fuse = Fuse.lv))
Comm.prob

# Relate the CPT to the labels
cpt <- list(Fuse = Fuse.prob, 
            Plea = Plea.prob,
            Atti = Atti.prob, 
            Comm = Comm.prob)
#  Relate the DAG and CPT and define a fully-specified BN
bn <- custom.fit (dag, cpt)
bn




###########################################################################
# 2. Learning the structure and parameters from observational data
###########################################################################
library(rgraphviz)
retention <- read.csv("retention.csv", header = T, colClasses = "factor" )
retention_test <- read.csv("retention_test.csv", header = T, colClasses = "factor" )
 
head (retention)
str (retention)
# Reminder: bn are particularly designed for categorical variables
# continuous variable require to be discretized.
# However, if all variables are continuous, a Gaussian Bayes net can be built. 
# A Gaussian Bayes net is equivalent to a path analysis model or a sem model 
# with observable variables instead of latent constructs. 


    # 2.1 Learning a structure using a constrained-based algorithm 
    # "grow-shrink (gs)", with conditional independence test chi-squared
    # Constrained-based alg. do not work with missing data
    par(mfrow = c(2, 2))
    bn.gs <- gs (retention, alpha = 0.05, test ="x2") # alternative test ="mi"
    plot(bn.gs, main = "Grow shrink_X2")
    graphviz.plot (bn.gs, main = "Grow shrink_X2") 
    # notice that in the constrained-based alg some links are undirected.
    # this occurs because the algorithm cannot establish the direction of "causality". 
    
    # other constraint-based algorithms have been developed
    bn2 <- iamb (retention, alpha = 0.05, test ="mi")
    graphviz.plot (bn2, main = "Iamb1_mi" ) 
    bn3 <- fast.iamb (retention, alpha = 0.05, test ="mi")
    graphviz.plot (bn3, main = "FastIamb_mi") 
    bn4 <- inter.iamb (retention, alpha = 0.05, test ="mi" )
    graphviz.plot (bn4, main = "InterIamb_mi") 
    # in the optimal case, all will return the same graph
    
    # to easily identify undirected paths 
    undirected.arcs(bn.gs)
    # We need to set the direction of the undirected arcs to be able 
    # to learn the parameters from observational data 
    bn.gs1 <- set.arc (bn.gs, from = "Atti", to = "Comm")
    #plot(bn.gs1, main = "Grow Shrink_") 
    graphviz.plot(bn.gs1, main = "Grow Shrink") 
    

  
    # 2.2. Learning the structure using a score-based algorithm 
    # Hill-Climbing (hc) greedy search
    par(mfrow = c(1, 2))
    bn.hc <- hc (retention, score = "bic")
    #plot (bn.hc, main = "Hill Climbing_BIC") 
    graphviz.plot (bn.hc, main = "Hill Climbing_BIC") 

    
    # Learning the parameters 
    # we learn the parameters for bn.gs1 structure (the theoretical structure) 
    bn.mle <- bn.fit (bn.gs1, data = retention, method = "mle")
    bn.mle
    # print them
    bn.mle$Fuse
    bn.mle$Plea
    bn.mle$Atti
    bn.mle$Comm
    
    # Other useful functions 
    # drop.arc(net, from="A", to=""T)
    # e.g. newnet = drop.arc(net, from = "T", to = "A")
    # Test for the conditional independence between variables 
    # ci.test("T", "E", c("O", "R"), test = "x2", data = data)
    # mb(bn.gs1, "Atti")
    

###############################
# 3). Model evaluation 
###############################
    # I.Metrics of model complexity 
    nodes(bn.mle)
    arcs(bn.mle)
    bn.mle 
    
    
    # II. Metrics of model sensitivity
    # Test if any two nodes are d-separated 
    dsep(bn.mle, x = "Plea", y = "Fuse")
    dsep(bn.mle, x = "Plea", y = "Comm")

    
    # III. Evaluate the arc.strength()
    # a) with criterion ="x2" or "mi", the output reports the p-value for the test. 
    #    The lower the p-value, the stronger the relationship. 
    # b) with criterion ="bic" reports the change in the BIC score of the net caused 
    #    by an arc removal.The more negative the change, means the BIC score will go 
    #    worse if we delete that arc (i.e. the arc is important for the model).
    library(dplyr)
    options(scipen = 0)
    arc.strength (bn.gs1, retention, criterion = "x2") %>%.[order(.$strength),]
    arc.strength (bn.gs1, retention, criterion = "bic") %>%.[order(.$strength),]
    # The output reveals that, if we remove Plea -> Comm, BIC will decrease with -668.211, 
    # which in bnlearn means the model will get worse.
    # The output reveals that, if we remove Atti -> Comm, BIC will increase with 40.48, 
    # which in bnlearn package means the model may improve based on this index.
    
    # Repeating the analysis for the hill-climbing structure
    arc.strength (bn.hc, retention, criterion = "bic") %>%.[order(.$strength),]
    # As expected, all strengths are negative
    # this is expected as BIC was optimized when the hc algorithm has searched for this model 
    
    ############################################################################
    # III. Metrics of evaluation and selection among several dags: 
    # BIC, BDe, AIC scores are used to compare alternative structures and choose the best  
    # In bnlearn, AIC, BIC, BDE closer to zero means better model; often the three indexes
    # do not agree.
    bnlearn::score (bn.gs1, retention, type = "aic")
    bnlearn::score (bn.hc, retention, type = "aic")
    
    bnlearn::score (bn.gs1, retention, type = "bic")
    bnlearn::score (bn.hc, retention, type = "bic")
    
    
    ############################################################################
    # IV. Metrics of predictive accuracy (error rate, confusion matrix, AUC)  
    library(gRain)
    library(gRbase)
    library (caTools)
    
    
    # using k-fold cross validation 
    # This function requires as one of its parameters only structure, not the full model
    # Here I use classification error ("pred") for the node Comm (our target) as a loss function. 
    netcv = bn.cv (retention, bn.gs1, loss ="pred", k = 5, loss.args = list(target = "Comm"), debug = TRUE)
    netcv 
    # the prediction accuracy of Comm based on 5-fold cross validation is 1-0.18 = 0.82 
    # in a similar way one can assess each individual variable
    
    
    
    # using a testing sample to evaluate the model performance
    # we need to transform the full model into a gRain object 
    net1 =  as.grain(bn.mle)
    net1
    # assuming Comm is the target node, we predict the probability of Comm 
    # using net1 in the test sample
    predComm = predict (net1, response = c("Comm"), newdata = retention_test, 
                        predictors = names (retention_test)[-4], 
                        type = "distribution") 
    #target variable is the 4th column in the testing dataset 
    predComm = predComm$pred$Comm
    predComm

    # Instead of probabilities of 0 or 1, one can save the actual CLASS (0/1). 
    predComm_class = predict (net1, response = c("Comm"), 
                              newdata = retention_test, 
                              predictors = names (retention_test)[-4], 
                              type = "class")
    predCommclass = predComm_class$pred$Comm
    predCommclass

        ########################################################################
        # Another method if you cannot use package gRain 
          bn.mle1 = bn.fit(model2network("[Fuse][Plea][Atti|Fuse:Plea][Comm|Fuse:Plea:Atti]"),retention) 
          predComm1= predict(bn.mle1, node = "Comm",data = retention_test) 
          predComm1
          table(predCommclass, predComm1)
        ########################################################################
        
    
    # confusion matrix
    table(predComm_class$pred$Comm, retention_test$Comm)
 
    
    # ROC and AUC 
    library(caTools) 
    colAUC(predComm, retention_test[ ,4], plotROC = TRUE) 
    # requires the predicted probabilities, not the predicted class
    # we get an AUC for every column of the prediction matrix
    # our DV has 3 categories: Low, Med and High
    # we observe that the model has problems when distinguishing between high and medium
    # but performs pretty well when identifying the Low category (customers who are not committed to VC)
   
  
    
##############################################
# 4) Making queries (forward and backward)
##############################################
# How do we use the model in practice?
# Below we consider several hypothetical situations.   
  
      
  # Using BN, one can evaluate the expected changes in attitude, and 
  # respectively, commitment due to changes in functional usefulness and pleasure. 
  # We will set evidence in the network for Fuse and Plea and we´ll look at the 
  # cpt for Atti and Comm before and after setting the evidence. 
  # Setting (hard) evidence means setting one of the states of the variable
  # at probability 1 (100%).
    
    
    # Transform the bn into a junction tree 
    # options(digits=1)
    library (gRain)
    junction <- compile (as.grain(bn.mle))
    # "querygrain" function extracts the marginal distribution of the nodes
    querygrain(junction, nodes = "Atti")
    querygrain(junction, nodes = "Comm")

    
    # Imagine a new customer joins the VC reporting a Low (Medium or High) 
    # Functional Usefulness perception. 
    # This information can be fed to the network as evidence in order to predict 
    # the conditional probability of his/her attitude and commitment to VC. 
    
    # if Fuse = Low
    jLow <- setEvidence (junction, nodes = "Fuse", states = "Low")
    A1 = querygrain(jLow, nodes = "Atti")
    A1
    C1= querygrain(jLow, nodes = "Comm")
    C1
    
    # if Fuse = Med
    jMed <- setEvidence (junction, nodes = "Fuse", states = "Med")
    A2 = querygrain(jMed, nodes = "Atti")
    A2
    C2 = querygrain(jMed, nodes = "Comm")
    C2
    
    
    # if Fuse = High
    jHigh <- setEvidence (junction, nodes = "Fuse", states = "High")
    A3 = querygrain(jHigh, nodes = "Atti")
    A3
    C3 = querygrain(jHigh, nodes = "Comm")
    C3
    
    
    # Summary (only for Atti)
    AttiHigh <- c(A1$Atti[[1]], A2$Atti[[1]], A3$Atti[[1]])
    AttiLow <- c(A1$Atti[[2]], A2$Atti[[2]], A3$Atti[[2]])
    AttiMed <-c(A1$Atti[[3]], A2$Atti[[3]], A3$Atti[[3]])
    df1 <- data.frame(Fuse = c("Low", "Med", "High"), AttiLow, AttiMed, AttiHigh)
    df1
    matplot(rownames(df1), df1, type='l', xlab='Fuse', ylab='', ylim=c(0,1))
    legend('topright', inset=.01, legend=colnames(df1[,2:4]), 
           pch=1, horiz=T, col=2:4)
    
    # Discussion
    # As Fuse changes from Low to Medium to High, 
    #   - the high state of attitude shows an increasing trend, 
    #   - the medium state of attitude shows a decreasing trend,
    #   - the low state of attitude shows a constant trend. 
    # Notice in the figure that when functional usefulness is low (left-side), 
    # the probability of attitude medium is quite high (0.80); it may suggest that
    # functional usefulness does not radically affect the customer´s attitude.  
    
    
    # same analysis by setting evidence in Pleasure
    # if Plea = Low
    jLow <- setEvidence (junction, nodes = "Plea", states = "Low")
    A1 = querygrain(jLow, nodes = "Atti")
    C1= querygrain(jLow, nodes = "Comm")
    
    # if Plea = Med
    jMed <- setEvidence (junction, nodes = "Plea", states = "Med")
    A2 = querygrain(jMed, nodes = "Atti")
    C2 = querygrain(jMed, nodes = "Comm")
  
    # if Plea = High
    jHigh <- setEvidence (junction, nodes = "Plea", states = "High")
    A3 = querygrain(jHigh, nodes = "Atti")
    C3 = querygrain(jHigh, nodes = "Comm")
    
    # summary 
    AttiHigh <- c(A1$Atti[[1]], A2$Atti[[1]], A3$Atti[[1]])
    AttiLow <- c(A1$Atti[[2]], A2$Atti[[2]], A3$Atti[[2]])
    AttiMed <-c(A1$Atti[[3]], A2$Atti[[3]], A3$Atti[[3]])
    df2 <- data.frame(Plea = c("Low", "Med", "High"), AttiLow, AttiMed, AttiHigh)
    options(scipen = 999)
    df2
    matplot(rownames(df2), df2, type='l', xlab='Plea', ylab='', ylim=c(0,1))
    legend('topright', inset=.01, legend=colnames(df2[,2:4]), 
           pch=1, horiz=T, col=2:4)
    # Discussion
    # as we set evidence in Plea from low to medium to high, we notice
    #  - the high state of attitude shows an increasing trend
    #  - the medium state of attitude shows a mixed trend and 
    #  - the low state of attitude shows a decreasing trend 
    # Notice in the figure, when pleasure is low, the probability of attitude 
    # being low is high (0.8592). This means pleasure has a stronger 
    # relationship with attitude; 
    # hence, linked to the previous result, for positive attitudes,
    # it is more important to enhance perceived pleasure than to enhance 
    # functional usefulness. 
    
  
    
  
    # Diagnostic (backward inference)
    # Let us assume that the evidence given is that the customer’s attitude towards VC
    # is high. This information is fed to the network by setting the probability of 
    # attitude being high and observing the changes in the parent 
    # variables (Fuse and Plea)
    
    # Prior ctp
    querygrain(junction, nodes = "Fuse")
    querygrain(junction, nodes = "Plea")
    
    # New ctp
    jHigh <- setEvidence (junction, nodes = "Atti", states = "High")
    querygrain(jHigh, nodes = "Fuse")
    querygrain(jHigh, nodes = "Plea")
    # Discussion
    # After we set evidence for Atti as High, the probability of the high state 
    # of Plea and Fuse is increasing, while the probability of the low and 
    # medium states of Plea and Fuse is decreasing. 
    # This implies that positive attitude towards interaction in a VC is because of 
    # person’s increased perception of pleasure and functional usefulness in the VC.
    
    
    # Assume that the online vendor observes decreasing commitment towards participation
    # among its customers. He can set evidence to the network that the probability 
    # of commitment is low and see the effect of the parent variables (attitude,
    # functional usefulness and pleasure).
    # Prior ctp
    querygrain(junction, nodes = "Atti")
    querygrain(junction, nodes = "Fuse")
    querygrain(junction, nodes = "Plea")
    
    # New ctp
    CLow <- setEvidence (junction, nodes = "Comm", states = "Low")
    querygrain(CLow, nodes = "Atti")
    querygrain(CLow, nodes = "Fuse")
    querygrain(CLow, nodes = "Plea")
    # Discussion
    # After we set evidence for Comm as Low, the prob of Pleasure to be High is 
    # 0.00000. A vendor, therefore, needs to take corrective action to enhance 
    # customers’ pleasure aspect in the VC to improve customers’ commitment. 
    # The results infer that low commitment is mainly due to lack of enhancement 
    # of the pleasure aspect of the website. 
    # The vendors need to take corrective action to provide the customers 
    # with more fun and enjoyment. 
    
    
    
    # Modeling contradictory behavior
    # Assume some customers interact in the VC to seek information from the VC, 
    # but do not participate in VC activities. 
    # Such customers can be considered persons with positive attitudes but low 
    # commitment.
    # Can BN predict the reasons behind such contradictory behavior?
    # Prior ctp
    querygrain(junction, nodes = "Fuse")
    querygrain(junction, nodes = "Plea")
    
    # New ctp
    AHigh <- setEvidence (junction, nodes = "Att", states = "High")
    AHighCLow <- setEvidence (AHigh, nodes = "Comm", states = "Low")
    querygrain(AHighCLow, nodes = "Fuse")
    querygrain(AHighCLow, nodes = "Plea")
    # Discussion
    # After we set evidence for attiude and commitment we observe that 
    # FUSE is 0.30 likely to be Low, but a significant 
    # proportion is still likely to be High (0.62); instead,
    # PLEA is most likely to be Low (0.69) and unlikely to be High (0%) 
    # This reveals that customers interact primarily because of fun, 
    # and they do not perceive the VC to be sufficiently useful to them to 
    # commit to. 
    
    

 
 
   