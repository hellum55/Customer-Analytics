library(recommenderlab)
library(tidyverse)


### Step 1 - storage
data(MovieLense)
help(MovieLense)
class(MovieLense)
dim(MovieLense)
# Data is given in realRatingMatrix format  ; Optimized to store sparse matrices
str(MovieLense,vec.len=2) #not as we normally reference list elements by \\$ but \\@
methods(class=class(MovieLense)) # methods applicable to this class


### Step 2 - explore data
## Loading the metadata that gets loaded with main dataset
moviemeta <- MovieLenseMeta
class(moviemeta)
colnames(moviemeta)

## What do we know about the films?
library(pander)
pander(head(moviemeta,2),caption = "First few Rows within Movie Meta Data ")
# Look at the first few ratings of the first user
head(as(MovieLense[1,], "list")[[1]])
# Number of ratings per user
hist(rowCounts(MovieLense))
# Number of ratings per movie
hist(colCounts(MovieLense))
# Top 10 movies
movie_watched <- data.frame(
  movie_name = names(colCounts(MovieLense)),
  watched_times = colCounts(MovieLense)
)
top_ten_movies <- movie_watched[order(movie_watched$watched_times, decreasing = TRUE), ][1:10, ] 
# Plot top 10
ggplot(top_ten_movies) + aes(x=movie_name, y=watched_times) + 
  geom_bar(stat = "identity",fill = "firebrick4", color = "dodgerblue2") + xlab("Movie Tile") + ylab("Count") +
  theme(axis.text = element_text(angle = 40, hjust = 1)) 

## What do we know about the ratings
summary(getRatings(MovieLense))
# Plot the ratings
data.frame(ratings=getRatings(MovieLense)) %>%
  ggplot(aes(ratings)) + geom_bar(width=0.75)+
  labs(title='MovieLense Ratings Distribution')


### Step 3 - split in training and test
# Training and test set: At least 30 items evaluated or at least 100 users for each item
rates <- MovieLense[rowCounts(MovieLense) > 30, colCounts(MovieLense) > 100]
rates1 <- rates[rowCounts(rates) > 30,]
# We randomly define the which_train vector that is True for users in the training set and FALSE for the others.
# We will set the probability in the training set as 80%
set.seed(1234)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(rates1), replace = TRUE, prob = c(0.8, 0.2))
# Define the training and the test sets
recc_data_train <- rates1[which_train, ]
recc_data_test <- rates1[!which_train, ]


### step 4 - recommendations
## Get an overview of different recommender models
recommenderRegistry$get_entries(dataType="realRatingMatrix")
recommender_models <- recommenderRegistry$get_entries(dataType="realRatingMatrix")
names(recommender_models)
lapply(recommender_models,"[[","description")
recommender_models$IBCF_realRatingMatrix$parameters

## Item-based CF
# IBCF: Item-based collaborative filtering
# Let's build the recommender IBCF - cosine:
recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30)) 
# We have now created a IBCF Recommender Model
# We will define n_recommended that defines the number of items to recommend to 
# each user and with the predict function, create prediction(recommendations) for the test set.
n_recommended <- 5
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
# This is the recommendation for the first user
recc_predicted@items[[1]]
# Now let's define a list with the recommendations for each user
recc_matrix <- lapply(recc_predicted@items, function(x){
  colnames(rates)[x]
})
# Let's take a look the recommendations for the first four users:
recc_matrix[1:4]

## User-based CF
# UBCF = User-based collaborative filtering
# The method computes the similarity between users with cosine
# Let's build a recommender model leaving the parameters to their defaults. 
recc_model <- Recommender(data = recc_data_train, method = "UBCF")
# A UBCF recommender has now been created
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
# Let's define a list with the recommendations to the test set users.
recc_matrix <- sapply(recc_predicted@items, function(x) {
  colnames(rates)[x]
})
# Again, let's look at the first four users
recc_matrix[,1:4]


### step 5 - evaluation
## Evaluation of IBCF ratings
# Cross validation
# We can split the data into some chunks, take a chunk out as the test set, and evaluate the accuracy. Then we can 
# do the same with each other chunk and compute the average accuracy. Here we construct the evaluation model
n_fold <- 4 
rating_threshold <- 4 # threshold at which we consider the item to be good
items_to_keep <- 20 # given=20 means that while testing the model use only 20 randomly picked ratings from every 
# user to predict the unknown ratings in the test set the known data set has the ratings specified by given and the 
# unknown data set the remaining ratings used for validation
eval_sets <- evaluationScheme(data = rates1, method = "cross-validation", k = n_fold, 
                              given = items_to_keep, goodRating = rating_threshold)
size_sets <-sapply(eval_sets@runsTrain, length)
size_sets
#IBCF
model_to_evaluate <- "IBCF"
model_parameters <- NULL  #   we use the standard settings
eval_recommender <-Recommender(data = getData(eval_sets, "train"), method = model_to_evaluate, parameter = model_parameters)
# The IBCF can recommend new items and predict their ratings. In order to build 
# the model, we need to specify how many items we want to recommend, for example, 5.
items_to_recommend <- 5
# We can build the matrix with the predicted ratings using the predict function:
eval_prediction <- predict(object = eval_recommender, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")
# By using the calcPredictionAccuracy, we can calculate the Root mean square 
# error (RMSE), Mean squared error (MSE), and the Mean absolute error (MAE).
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = TRUE
  )
# This is a small sample of the results for the Prediction and Accuracy
head(eval_accuracy)
# Now, let's take a look at the RMSE by each user
ggplot(data=as.data.frame(eval_accuracy),aes(x=RMSE)) + geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")
# However, we need to evaluate the model as a whole, so we will set the byUser to False
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = FALSE
)
eval_accuracy #for IBCF

## Evaluation of IBCF top-N
# Confusion matrix good threshold =4
results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10, 100, 10)) #n number top-n recommendations
# results object is an evaluationResults object containing the results of the evaluation.
# Each element of the list corresponds to a different split of the k-fold.
# Let's look at the first element
head(getConfusionMatrix(results)[[1]])
# In this case, look at the first four columns
# True Positives (TP): These are recommended items that have been purchased.
# False Positives (FP): These are recommended items that haven't been purchased
# False Negatives (FN): These are not recommended items that have been purchased.
# True Negatives (TN): These are not recommended items that haven't been purchased.
# If we want to take account of all the splits at the same time, we can just sum up the indices:
columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)

## Building an ROC curve. Will need these factors
# 1. True Positive Rate (TPR): Percentage of purchased items that have been recommended. TP/(TP + FN)
# 2. False Positive Rate (FPR): Percentage of not purchased items that have been recommended. FP/(FP + TN)
plot(results, annotate = TRUE, main = "ROC curve")

## We can also look at the accuracy metrics as well
# precision: Percentage of recommended items that have been purchased. FP/(TP + FP)
# recall: Percentage of purchased items that have been recommended. TP/(TP + FN) = True Positive Rate
plot(results, "prec/rec", annotate = TRUE, main = "Precision-Recall")

## Comparing models
models_to_evaluate <- list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")), 
                           IBCF_cor = list(name = "IBCF", param = list(method = "pearson")), 
                           UBCF_cos = list(name = "UBCF", param = list(method = "cosine")), 
                           UBCF_cor = list(name = "UBCF", param = list(method = "pearson")), 
                           random = list(name = "RANDOM", param = NULL))
# In order to evaluate the models, we need to test them, varying the number of items.
n_recommendations <- c(1,5,seq(10,100,10))
# Now let's run and evaluate the models
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)
# Plot the ROC curve
plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")
# Plot precision-recall
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright", ylim = c(0,0.4))
title("Precision-recall")

