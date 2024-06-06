#______________________________________________________________________________
# Application 3: Product recommendations with probabilistic models
# Bayesian Nets, Course Customer Analytics
# March 2024
# AAT 
#______________________________________________________________________________

# In this analysis, we implement a latent class analysis with BN. 
# This classification is not directly observable; it is inferred from the data. 
# The key premise is that membership in a particular latent class can accurately
# predict a user's ratings across a range of items. 
# This method offers a comprehensive to product recommendation 
# by considering several factors:

    # User Similarities: By analyzing patterns across users, the model 
    # identifies underlying groups of users who exhibit similar behavior 
    # in their ratings. This helps in understanding user preferences and 
    # can be used to predict how a user might rate items that they have not 
    # yet encountered, based on the behavior of similar users within the 
    # same latent class

    # Item Similarities: Similarly, the model accounts for the relationships 
    # between items. It recognizes that certain items are consistently rated 
    # similarly by users, suggesting that these items share appealing 
    # characteristics or cater to specific interests

    
    # User-Item Interactions: Beyond examining users and items in isolation, 
    # this approach delves into the interactions between users and items. 
    # It explores how particular types of users tend to rate certain kinds of
    # items, capturing the dynamic interplay between user preferences and item 
    # characteristics



library(poLCA)      # for latent classification 
library(bnlearn)    # for building BN
library (gRain)     # for querying BN
library(Rgraphviz)  # for visualizing BN


# Data on user preferences
data = read.csv("CollFilforR.csv", 
                header = T, 
                colClasses = "factor", 
                sep = ";")
View(data)




# First step, test several models with number of classes ranging from 2 to 6
# Display BIC. Choose the model with the lowest BIC (AIC, G^2, X^2)
# At this stage we only decide the number of latent classes in the data

set.seed(234)
# Defining the variables used in the model 
f <- cbind(V1, V2, V3, V4)~1 #(~1 means without covariates)
# V1...V4 are the observed categorical variables from which the latent classes are derived 
# f <- cbind(V1, V2, V3, V4) ~ Cov1 + Cov2 (if covariates, e.g. individual characteristics)

bic_values <- numeric(length = 5) #we are testing 5 models (from 2 to 6 classes)
min_bic <- 100000
for(i in 2:6){
  lc <- poLCA(f, data, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=1, verbose=TRUE, calc.se=TRUE)
  
  bic_values[i-1] <- lc$bic  # Store the BIC value for each model
  
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	

# bic
print(bic_values)
# plot bic
classes <- 2:6
plot(classes,
     bic_values, 
     type = "b", 
     pch = 19, xlab = "Number of Classes", ylab = "BIC",
     main = "BIC Values by Number of Classes")

# best selected model - full output
LCA_best_model 
# specific output
LCA_best_model$posterior # matrix of posterior class membership probabilities
LCA_best_model$predclass # class membership
LCA_best_model$probs # estimated class-conditional response probabilities

# save the class
data_copy <- data
data_copy$class <- factor(LCA_best_model$predclass)
View (data_copy)





# Second step, for making product recommendations, 
# set up a BN classifier considering the learned class as the 'root' node 
# and preferences as ´children' nodes
data_wNA = na.omit(data_copy) # remove missing data from the dataset
# learn the structure (e.g., TAN structure)
dag = tree.bayes(data_wNA, "class") 
plot(dag)
# learn the parameters
bn.mle <- bn.fit (dag, data = data_wNA, method = "mle")
bn.fit.barchart(bn.mle$V1)


# Now we can use BN to make product recommendations 
# e.g., given a new customer has reported the following preferences 
# for V1=5, V2=1, V3=1, what is the estimated preference for V4?
library (gRain)
library (graph)
junction <- compile (as.grain(bn.mle))

      # New: Warning due to 0 probabilities tables, we use Bayesian parameter estimation
      graph.model <- as.graphNEL(dag)
      gr.model <- grain(graph.model, data =  data_wNA, smooth = 1)


# now we can use the net for inference
V1V2V3 <- setEvidence (gr.model, nodes = c("V1", "V2", "V3"), states = c("5", "1", "1"))
prediction = querygrain(V1V2V3, nodes = "V4")
str(prediction)
prediction$V4

# $V4
# V4

#.   1          2          4          5 
#0.32669680 0.52234158 0.06583710 0.08512452
# this user most likely he will score 1 or 2 => do not recommend the product V4
# his expected preference for V4 is 
preference = prediction$V4[[1]]*1 + prediction$V4[[2]]*2 + prediction$V4[[3]]*4 + prediction$V4[[4]]*5  
preference

querygrain(V1V2V3, nodes = "class") # to get the expected class 




# To make recommendation in a dataset and save them
# Loop through each row in dataset (in this example we use our data)
data=data_copy
# Initialize predictions_df outside the loop
predictions_df <- data.frame(RowIndex = integer(), Node = character(), stringsAsFactors = FALSE)

# Loop through each row in your dataset to predict and save the most probable product to recommend
for (i in 1:nrow(data)) {
  knownPreferences <- !is.na(data[i, ])
  unknownPreferences <- is.na(data[i, ])
  
  # Ensure there is at least one known preference to set as evidence
  if(any(knownPreferences)) {
    nodes <- names(data)[knownPreferences]
    states <- as.character(data[i, knownPreferences])
    
    # Only proceed if there are nodes and states to set as evidence
    if(length(nodes) > 0 && length(states) > 0) {
      junctionWithEvidence <- setEvidence(junction, nodes = nodes, states = states)
      
      
      # Now, proceed to query the model for unknown preferences
      for (nodeToPredict in names(data)[unknownPreferences]) {
        prediction <- querygrain(junctionWithEvidence, nodes = nodeToPredict)
        predictions_df <- rbind(predictions_df, data.frame(RowIndex = i, Node = nodeToPredict, stringsAsFactors = FALSE))
      }
      }
    }
  }

View(predictions_df)
write.csv(predictions_df, "predictions_product.csv", row.names = FALSE)


# To save the probabilities instead of the most probable state
predictions_df <- data.frame(RowIndex = integer(),
                             Node = character(),
                             State = character(),
                             Probability = numeric(),
                             stringsAsFactors = FALSE)

for (i in 1:nrow(data)) {
  knownPreferences <- !is.na(data[i, ])
  unknownPreferences <- is.na(data[i, ])
  
  if(any(knownPreferences)) {
    nodes <- names(data)[knownPreferences]
    states <- as.character(data[i, knownPreferences])
    
    if(length(nodes) > 0 && length(states) > 0) {
      junctionWithEvidence <- setEvidence(junction, nodes = nodes, states = states)
      
      for (nodeToPredict in names(data)[unknownPreferences]) {
        prediction <- querygrain(junctionWithEvidence, nodes = nodeToPredict)
        
       
          # Extract probabilities and their corresponding state names
          probs <- prediction[[nodeToPredict]]  # Accessing the numeric vector of probabilities
          stateNames <- attr(probs, "dimnames")[[1]]  # Extracting state names from dimnames
          
          # Iterate through each state and its probability
          for (j in seq_along(probs)) {
            new_row <- data.frame(RowIndex = i,
                                  Node = nodeToPredict,
                                  State = stateNames[j],
                                  Probability = probs[j],
                                  stringsAsFactors = FALSE)
            predictions_df <- rbind(predictions_df, new_row)
          }
        }
        
      }
    }
  }

View(predictions_df)
write.csv(predictions_df, "prediction_probabilities.csv", row.names = FALSE)


