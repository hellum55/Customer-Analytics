# ********************************************* #
#     Application with clickstreams data        #
#     Course: Customer Analytics                #
#     Lecturer: Ana Alina Tudoran               #
#     Update: Spring 2022                       #
# ********************************************* #

# INTRODUCTION
# For modeling sequences of clicks:

  # Virtually any model can be used to analyse clickstream data
  # given the data is pre-processed in the correct way
  # However, Neural Networks (NN) and Markov Chains (MC)
  # are two of the most common type of models used for modeling clickstreams 

  # NN are black-box models best for prediction. 
  # For example, predicting the conversion rate is a standard application. 
  # NN are also used for segmentation.
  # For example, Self-organizing maps clustering (SOM) is a type of neural network for
  # unsupervised learning that reduces the data through 
  # the use of self-organizing maps (Kohonen, 1998).
  # NN are not covered in this course. However, an example of using SOM for 
  # customer segmentation can be found in the supplementary reading
  # Tudoran, A.A. 2022 A Machine Learning Approach to Identifying Decision-Making
  # Styles for Managing Customer Relationships on Brightspace

  # Markov chains models are transparent models that 
  # are used for predicting, segmenting and understanding customer behavior. 
  # In this application, I focus on the "clickstream" library 
  # to model the clickstream data with MC models. 



# BACKGROUND
# What is a (discrete state) Markov Chain? 
   # [Note: one can have Markov chains with discrete and continous states 
   # in discrete and in continuous time. Here we focus on discrete state
   # in discrete time] 

  # A Markov chain is a stochastic process X that takes state "m" 
  # from a ﬁnite set of states "M" at each time "n". 
  # If the state at time "n" only depends on the recent k states, 
  # we call X a Markov chain "of order k". For example:
  
      # 0-order Markov chain = the probability to be in any of the "m" states in the next step is
      # independent of the present state
      # 1st-order Markov chain = the probability to be in any of the "m" states in the next step is
      # independent of the previous states given the present state (one-period memory)
      # 2nd-order Markov chain = two-periods memory
      # 3rd-order Markov chain = three-periods memory
      # ...so on
  
      # Markov chains can be described by transition probability matrices
      # Each value in these matrices is a parameter
      # Higher-order Markov chains have (m−1)m^k model parameters 
      # the number of lag parameters increases exponentially


# How we select the Markov chain order? 
  # Usually, a user considering a Product Page might either Add the product to the shopping cart, 
  # view Product Reviews, follow a Product Recommendation, or Search for another product. 
  # Moe (2003) proposes that the probability for a transition to either of the possible 
  # next states depends on the MODE (browsing, searching, or buying) the user is currently in. 
  # This MODE (latent state) can be identiﬁed when considering the recent k states.

  # Model order selection is usually based on a criteria like AIC or BIC. 
  # The function fitMarkovChain() estimates the parameters 
  # of a Markov chain model of order k.


# How does R interpret the clickstreams? 
  # Clickstreams = collection of data sequences - with different sizes!


# (1) A motivating example
  # Session 1: P1 P2 P1 P3 P4 Defer
  # Session 2: P3 P4 P1 P3 Defer
  # Session 3: P5 P1 P6 P7 P6 P7 P8 P7 Buy
  # Session 4: P9 P2 P11 P12 P11 P13 P11 Buy
  # Session 5: P4 P6 P11 P6 P1 P3 Defer
  # Session 6: P3 P13 P12 P4 P12 P1 P4 P1 P3 Defer
  # Session 7: P10 P5 P10 P8 P8 P5 P1 P7 Buy
  # Session 8: P9 P2 P1 P9 P3 P1 Defer
  # Session 9: P5 P8 P5 P7 P4 P1 P6 P4 Defer

# 13 possible product pages
# 2 absorbing states

# a clickstream =  a sequence of click events for exactly one session of an online store user
# a clickstream = a vector in R
# a collection of clickstreams = a list in R
# we read the list of cliskstreams from a file using "readClickstreams()" function
# it is a comma-separated file and each line is exactly one clicktream 

library(clickstream)
cls <- readClickstreams(file = "sample.csv", sep = ",", header = TRUE) 
cls
summary(cls)
# writeClickstreams(cls, "sample.csv", header = TRUE, sep = ",")

mc <- fitMarkovChain(clickstreamList = cls, order = 2,control = list(optimizer = "quadratic")) 
mc
# notice:
# - the two transition probabilities matrices for the two lags
# - start probabilities for the states the corresponding clickstreams started 
# - end probabilities for the states the corresponding clickstreams ended 

# fitMarkovChain() computes the log-likelihood of the model
# based on this, one can get AIC and BIC to compare two fitted Markov models
summary(mc) 
# model plot
plot(mc, order = 2)


# i)  Predict either the next click or the ﬁnal click (state) of a customer.
  # If a customer starts with the clickstream P9 P2, what will do next?
  pattern <- new("Pattern", sequence = c("P9", "P2"))
  resultPattern <- predict(mc, startPattern = pattern, dist = 1)
  resultPattern
  # the user will most likely click on P1 next


  # If a customer has recently viewed products P9 and P2, what is the prediction for the next two clicks?
  pattern <- new("Pattern", sequence = c("P9", "P2"), absorbingProbabilities = data.frame(Buy = 0.333, Defer = 0.667))
  resultPattern <- predict(mc, startPattern = pattern, dist = 2)
  resultPattern
  # He or she visit products P1 and P3, 
  # but the probability that she really continues visiting products P1 P3 is only 26.17%.
  # Purchasing probability is 5.83% after 2 further clicks,
  # and she most likely defers the purchase 

  
  # Online stores often have evidence on how many of the visitors convert 
  # to a buyer (this info is typically used to formulate initial absorbing probabilities
  # for all users). 
  # But, particularly for customers who log on to their account, the online stores can also 
  # know how many times a particular user has been 
  # only visiting the online store  and how often she has bought a product, 
  # This information can be used to formulate initial absorbing probabilities for a user. 
  # If for example a user has been logged in and ﬁnally bought a product in 50% of her log-ins,
  # we can compute absorbing probabilities (posterior) for a stream of clicks:
  
  absorbingProbabilities <- c(0.5, 0.5) 
  sequence <- c("P9", "P2")
  for (s in sequence) {
       absorbingProbabilities <- absorbingProbabilities * 
         data.matrix(subset(mc@absorbingProbabilities, state == s,
                            select = c("Buy", "Defer")))
       }
  absorbingProbabilities <- absorbingProbabilities /sum(absorbingProbabilities)
  absorbingProbabilities
  # 22.62% to ﬁnally buy a product after she has visited products P9 and P2.
  
  
  
  
## ii) An alternative before running the model is to identify segments of customers 
## by clustering clickstreams and afterwards building a model within each cluster
## (Reference: Huang, Ng, Ching, Ng, and Cheung, 2001, k-means alg. and Euclidean distance) 
  
  set.seed(12345)
  clusters <- clusterClickstreams(clickstreamList = cls, order = 1,centers = 3)
  clusters
  clusters$clusters[[1]]
  clusters$clusters[[2]]
  clusters$clusters[[3]]
  
  # Fit a ‘MarkovChain’ object for each ‘Clickstreams’ object 
  mc_clu1 <- fitMarkovChain(clickstreamList = clusters$clusters[[1]], order = 2,control = list(optimizer = "quadratic"))
  mc_clu1
  
  mc_clu2 <- fitMarkovChain(clickstreamList = clusters$clusters[[2]], order = 2,control = list(optimizer = "quadratic"))
  mc_clu2
  
  mc_clu3 <- fitMarkovChain(clickstreamList = clusters$clusters[[3]], order = 2,control = list(optimizer = "quadratic"))
  mc_clu3
  # or write these objects to ﬁle with writeClickstreams().
  
  
  
  
  
# (2) Full example with simulated data
  # - clickstreams for 100,000 user sessions 
  # - clicks are either one of 7 products or on one of the two ﬁnal states "Buy" and "Defer".
  
  # Prepare the data
  set.seed(123)
  cls <- randomClickstreams(states = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "Defer", "Buy"),
                            startProbabilities = c(0.2, 0.25, 0.1, 0.15, 0.1, 0.1, 0.1, 0, 0),
                            transitionMatrix = matrix(c(0.01, 0.09, 0.05, 0.21, 0.12, 0.17, 0.11, 0.2, 0.04,
                                                        0.1, 0, 0.29, 0.06, 0.11, 0.13, 0.21, 0.1, 0,
                                                        0.07, 0.16, 0.03, 0.25, 0.23, 0.08, 0.03, 0.12, 0.03,
                                                        0.16, 0.14, 0.07, 0, 0.05, 0.22, 0.19, 0.1, 0.07,
                                                        0.24, 0.27, 0.17, 0.13, 0, 0.03, 0.09, 0.06, 0.01,
                                                        0.11, 0.18, 0.04, 0.15, 0.26, 0, 0.1, 0.11, 0.05,
                                                        0.21, 0.07, 0.08, 0.2, 0.14, 0.18, 0.02, 0.08, 0.02,
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
                            meanLength = 50, n = 100000)
  
  summary(cls)
  
  
  # select the model (Markov chain order)
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = cls, order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result

  # fit the selected model
  mc <- fitMarkovChain(clickstreamList = cls, order = 2,control = list(optimizer = "quadratic")) 
  mc
  # predict the next click
  pattern <- new("Pattern", sequence = c("P3", "P4"), absorbingProbabilities = data.frame(Buy = 0.22, Defer = 0.78))
  resultPattern <- predict(mc, startPattern = pattern, dist = 1)
  resultPattern
  
  # or, cluster, fit a mc per cluster and predict; clustering clickstreams is useful 
  # in case of high clickstream heterogeneity
  clusters_sim <- clusterClickstreams(clickstreamList = cls, order = 1, centers = 3)  # takes 5-10 min. to converge
  summary(clusters_sim$clusters[[1]]) 
  summary(clusters_sim$clusters[[2]]) 
  summary(clusters_sim$clusters[[3]]) 
  
  # mc for clu 1 
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters_sim$clusters[[1]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  
  mc_clu1 <- fitMarkovChain(clickstreamList = clusters_sim$clusters[[1]], order = 1) 
  summary(mc_clu1)
  
  pattern <- new("Pattern", sequence = c("P1", "P4", "P6"), absorbingProbabilities = data.frame(Buy = 0.22, Defer = 0.78))
  resultPattern <- predict(mc_clu1, startPattern = pattern, dist = 1)
  resultPattern
  # interpret
  
  
  
  
# (3) Example with real data (A Danish company provided us with a file of clickstream data on their e-commerce customers).
  mydata  <- readClickstreams(file = "~/Documents/Alina Tudoran/TEACHING/Postgraduate/Customer Analytics/6. CLICKSTREAMS/Lecture 2 Application R/StepsDesktop.csv", sep=",",header = T)
  summary(mydata)
  
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = mydata, order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  
  # mc order k = 1
  mc <- fitMarkovChain(clickstreamList = mydata, order = 1,control = list(optimizer = "quadratic")) 
  mc
  
  # Predict
  pattern <- new("Pattern", sequence = c("DeliveryPage"))
  resultPattern <- predict(mc, startPattern = pattern, dist = 1)
  resultPattern
  pattern <- new("Pattern", sequence = c("AddCartPage"))
  resultPattern <- predict(mc, startPattern = pattern, dist = 1)
  resultPattern
  pattern <- new("Pattern", sequence = c("AddCartPage", "CartPage"))
  resultPattern <- predict(mc, startPattern = pattern, dist = 1)
  resultPattern
  pattern <- new("Pattern", sequence = c("DeliveryPage"))
  resultPattern <- predict(mc, startPattern = pattern, dist = 1)
  resultPattern
  
  
  # Clustering first 
  clusters_ex <- clusterClickstreams(clickstreamList = mydata, order = 1, centers = 3)  # takes 5-10 min. to converge
  summary(clusters_ex$clusters[[1]]) # clu 1
  # mc for clu 1 
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters_ex$clusters[[1]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  
  
  summary(clusters_ex$clusters[[2]]) # clu 2
  # mc for clu 2 
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters_ex$clusters[[2]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  
  
  summary(clusters_ex$clusters[[3]]) # clu 3
  # mc for clu 3
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters_ex$clusters[[3]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  
  
  mc_clu1 <- fitMarkovChain(clickstreamList = clusters_ex$clusters[[1]], order = 1) 
  summary(mc_clu1)
  mc_clu2 <- fitMarkovChain(clickstreamList = clusters_ex$clusters[[2]], order = 1) 
  summary(mc_clu2)
  mc_clu3 <- fitMarkovChain(clickstreamList = clusters_ex$clusters[[3]], order = 1) 
  summary(mc_clu3)
  
  # model graphical representation (i)
  plot(mc_clu1, order = 1)
  plot(mc_clu2, order = 1)
  plot(mc_clu3, order = 1)
  
  # model graphical representation (ii)
  par(mfrow=c(1,3))
  par(mar=c(1, 1, 4, 0))
  set.seed(11)
  plot(mc_clu1, order = 1, digits = 1, minProbability = 0.40,
       vertex.color=0,
       vertex.frame.color=0,
       vertex.shape="none",
       vertex.size=8, 
       vertex.size2=3,
       vertex.label.dist=0.4, 
       vertex.color="transparent",
       vertex.label.font=2,
       vertex.label.cex=0.95, 
       vertex.label.degree=1,
       vertex.label.color="black",
       edge.arrow.size=0.2,
       edge.label.cex = 0.9, 
       edge.curved=0,
       edge.label.font=4,
       margin=c(0,0,0,0.15),
       main ="Cluster 1")
 
  
  set.seed(11)
  plot(mc_clu2, order = 1, digits = 1, minProbability = 0.40,
       vertex.color=0,
       vertex.frame.color=0,
       vertex.shape="none",
       vertex.size=8, 
       vertex.size2=3,
       vertex.label.dist=0.4, 
       vertex.color="transparent",
       vertex.label.font=2,
       vertex.label.cex=0.95, 
       vertex.label.degree=1,
       vertex.label.color="black",
       edge.arrow.size=0.2,
       edge.label.cex = 0.9, 
       edge.curved=0,
       edge.label.font=4,
       margin=c(0,0,0,0.15),
       main ="Cluster 2")
  
  
  set.seed(11)
  plot(mc_clu3, order = 1, digits = 1, minProbability = 0.40,
       vertex.color=0,
       vertex.frame.color=0,
       vertex.shape="none",
       vertex.size=8, 
       vertex.size2=3,
       vertex.label.dist=0.4, 
       vertex.color="transparent",
       vertex.label.font=2,
       vertex.label.cex=0.95, 
       vertex.label.degree=1,
       vertex.label.color="black",
       edge.arrow.size=0.2,
       edge.label.cex = 0.9, 
       edge.curved=0,
       edge.label.font=4,
       margin=c(0,0,0,0.15),
       main ="Cluster 3")
  
  # Characterizing the clusters. Who are the customers underlying the three 
  # patterns of movement within the website?
  # cl.2 and cl.3 search a lot through Category page; 
  # cl.1 search more through Product page;
  # cl.1 and cl.3 search more varied (use more channels than cl.2)
  # cl.3 are more likely to enter through DirectAccess -> Front page pattern
  # than the other two clusters.
  # In this application the product page was generalized. 
  # However, if a more fine-grained clicktream data were available by product brand
  # it will allow the manager to take informed decisions about the preferences of each cluster. 
  
  # predicting the next steps
  pattern <- new("Pattern", sequence = c("AddCartPage", "CartPage"))
  resultPattern <- predict(mc_clu1, startPattern = pattern, dist = 1)
  resultPattern
  
  pattern <- new("Pattern", sequence = c("AddCartPage", "CartPage"))
  resultPattern <- predict(mc_clu2, startPattern = pattern, dist = 1)
  resultPattern
  
  pattern <- new("Pattern", sequence = c("AddCartPage", "CartPage"))
  resultPattern <- predict(mc_clu3, startPattern = pattern, dist = 1)
  resultPattern
  # interpret
  # Possible covariates: product category, device. In that case, the analysis 
  # should be done by category and device. 
  
  
  
# (4) A second alternative to modeling clickstreams with higher-order 
  # Markov chains is representing them as sequential patterns. Such an approach answers to
  # the following question: Which are the patterns sequences most supported in each group? 
  # using "arulesSequences" and "arulesViz" for vizualization
  # Extracting all click patterns with a particular minimum support (Apriori algorithm)
  library("arules")
  library("arulesSequences")
  
  # looking by clustered data
  trans_clu1<- as.transactions(clusters$clusters[[1]]) 
  sequences_clu1 <- as(apriori(trans_clu1, parameter = list(support = 0.50)), "data.frame") 
  sequences_clu1
  # subrules <- subset(sequences_clu1, support>0.05)
  
  trans_clu2<- as.transactions(clusters$clusters[[2]]) 
  sequences_clu2 <- as(apriori(trans_clu2, parameter = list(support = 0.50)), "data.frame")
  sequences_clu2
  
  trans_clu3<- as.transactions(clusters$clusters[[3]]) 
  sequences_clu3 <- as(apriori(trans_clu3, parameter = list(support = 0.50)), "data.frame")
  sequences_clu3
  
  # The corresponding output shows that pattern sequences are supported by at least 50% of 
  # the clickstreams in each cluster. cluster 1 of clickstream is the most heterogeneeous. 
  # The most common pattern {ChannelAd} => {ProductPage} has support in 41% of the clicks
  # in clu1. This pattern reflects that these customers are most likely acquired through 
  # re-targeting. 
  
  # vizualization
  library("arulesViz")
  sequences_clu1 <- apriori(trans_clu1, parameter = list(support = 0.40))
  ruleExplorer(sequences_clu1)
  sequences_clu2 <- apriori(trans_clu2, parameter = list(support = 0.50))
  ruleExplorer(sequences_clu2)
  sequences_clu3 <- apriori(trans_clu3, parameter = list(support = 0.50))
  ruleExplorer(sequences_clu3)
  # this approach is also useful for discovering product/brands that are seen together

  
# (5) A third approach to work with clickstream data is to use frequencies of events instead of sequences. 
  # Disadv: the sequential structure is lost.
  # Adv: a variety of prediction methods seen in the Machine Learning can be applied using these frequency data
  frequencyDF <- frequencies(mydata)
  frequencyDF

# (6) A fourth option is to use Neural Networks (recurrent or not) with sequences 
  # of clickstream data purely for prediction purposes. NN are not covered in this course.  


# (7) Optional exercise with solution
  # Consider a real-life data set from Cadez, I., Heckerman, D., Meek, C., 
  # Smyth, P., White, S. (2003) Model-based clustering and visualization 
  # of navigation patterns on a web site, Data Mining and Knowledge Discovery, 399-424.
  # The dataset msnbc323 (Melnykov 2016a) is available 
  # in data("msnbc323", package = "ClickClust"). 
  # There are 323 clickstream sequences that involve 17 different states:
  
  # (1) frontpage, (2) news, 
  # (3) tech, (4) local, 
  # (5) opinion, (6) on-air,
  # (7) miscellaneous, (8) weather, 
  # (9) msn-news, (10) health-on-air, 
  # (11) living, (12) business, 
  # (13) msn-sports, (14) sports, 
  # (15) summary-news, (16) bbs, 
  # and (17) travel. 
  
  # The length of sequences varies from 35 to 362. 
  # There are 289 possible transitions among the 17 states. 
  
  library(ClickClust)
  data("msnbc323", package = "ClickClust")
  summary(msnbc323)
  clusters <- clusterClickstreams(clickstreamList = msnbc323, order = 1, centers = 3)  # takes 5-10 min. to converge
 
  
  # mc for clu 1 
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters$clusters[[1]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  #1
  
  # mc for clu 2 
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters$clusters[[2]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  #1
  
  
  # mc for clu 3
  maxOrder <- 5 
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = clusters$clusters[[3]], order = k)
    result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  result
  #1
  
  mc_clu1 <- fitMarkovChain(clickstreamList = clusters$clusters[[1]], order = 1) 
  summary(mc_clu1)
  mc_clu2 <- fitMarkovChain(clickstreamList = clusters$clusters[[2]], order = 1) 
  summary(mc_clu2)
  mc_clu3 <- fitMarkovChain(clickstreamList = clusters$clusters[[3]], order = 1) 
  summary(mc_clu3)
  
  
  # graphically
  par(mfrow=c(1,3))
  par(mar=c(1, 1, 4, 0))
  set.seed(11)
  plot(mc_clu1, order = 1, digits = 1, minProbability = 0.40,
       vertex.color=0,
       vertex.frame.color=0,
       vertex.shape="none",
       vertex.size=8, 
       vertex.size2=3,
       vertex.label.dist=0.4, 
       vertex.color="transparent",
       vertex.label.font=2,
       vertex.label.cex=0.95, 
       vertex.label.degree=1,
       vertex.label.color="black",
       edge.arrow.size=0.2,
       edge.label.cex = 0.9, 
       edge.curved=0,
       edge.label.font=4,
       margin=c(0,0,0,0.15),
       main ="Cluster 1")
  
  
  set.seed(11)
  plot(mc_clu2, order = 1, digits = 1, minProbability = 0.40,
       vertex.color=0,
       vertex.frame.color=0,
       vertex.shape="none",
       vertex.size=8, 
       vertex.size2=3,
       vertex.label.dist=0.4, 
       vertex.color="transparent",
       vertex.label.font=2,
       vertex.label.cex=0.95, 
       vertex.label.degree=1,
       vertex.label.color="black",
       edge.arrow.size=0.2,
       edge.label.cex = 0.9, 
       edge.curved=0,
       edge.label.font=4,
       margin=c(0,0,0,0.15),
       main ="Cluster 2")
  
  
  set.seed(11)
  plot(mc_clu3, order = 1, digits = 1, minProbability = 0.40,
       vertex.color=0,
       vertex.frame.color=0,
       vertex.shape="none",
       vertex.size=8, 
       vertex.size2=3,
       vertex.label.dist=0.4, 
       vertex.color="transparent",
       vertex.label.font=2,
       vertex.label.cex=0.95, 
       vertex.label.degree=1,
       vertex.label.color="black",
       edge.arrow.size=0.2,
       edge.label.cex = 0.9, 
       edge.curved=0,
       edge.label.font=4,
       margin=c(0,0,0,0.15),
       main ="Cluster 3")
  
  # interpret: 
  
  # Cluster 2 is entirely driven by transitions within the same categories
  # Thus, this group represents people who make the majority of transitions within their 
  # preferred category and do not change categories frequently.
  
  # Cluster 1 The second cluster is characterized by higher probabilities of transitions 
  # front page–news, news–news, msn-news, and summary–news. 
  # The transition front page–news reﬂects the common pattern for the users starting with 
  # the category front page to proceed directly to the category news. 
  # Once the reader gets to the category news, he or she typically stays within it or proceeds 
  # to summary.Thus, the second cluster consists of people mostly concerned with news. 
  
  # Cluster 3 is characterized by transitions travel-health-on-air, travel-frontpage, 
  # consisting of the people concerned about travelling issues.
  
  # The analysis of this dataset illustrates how click-plots can 
  # be used and interpreted to discover interesting navigation patterns common for 
  # observations within detected clusters.
  
  
  
  
  
  

  