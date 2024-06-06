library(recommenderlab)
library(tidyverse)


data(MovieLense)
class(MovieLense)
help(MovieLense)
dim(MovieLense)



#select only the users who have rated at least 50 movies or movies that had been rated more than 100 times
(ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                              colCounts(MovieLense) > 100])




# use the minimum number of items purchased by any user to decide item number to keep
(min(rowCounts(ratings_movies)))

n_fold <- 4
items_to_keep <- 15
rating_threshold <- 3


# Use k-fold to validate models
set.seed(1234)
eval_sets <- evaluationScheme(data = ratings_movies, method = "cross-validation",k = n_fold, given = items_to_keep, 
                              goodRating = rating_threshold)


models  <- list(
 
  IBCF=list(name="IBCF",param=list(method = "cosine")),
  UBCF=list(name="UBCF", param=list(method = "pearson")),
  SVD = list(name="SVD", param=list(k = 50)),
  SVDF=list(name="SVDF", param=list(k=50))
)

# varying the number of items we want to recommend to users
n_rec <- c(1, 5, seq(10, 100, 10))

# evaluating the recommendations
results <- evaluate(x = eval_sets, method = models, n= n_rec)

# extract the related average confusion matrices
(avg_matrices <- lapply(results, avg))

plot(results, annotate=TRUE)
plot(results, "prec/rec", annotate = TRUE, main = "Precision-Recall")

recommender_ibcf <- Recommender(data = getData(eval_sets, "train"),
                                method = "IBCF",parameter = list(method = "cosine"))

recommender_ubcf <- Recommender(data = getData(eval_sets, "train"),
                                method = "UBCF",parameter = list(method = "pearson"))

recommender_svd <- Recommender(data = getData(eval_sets, "train"),
                                method = "SVD",parameter = list(k=50))

recommender_svdf <- Recommender(data = getData(eval_sets, "train"),
                               method = "SVDF",parameter = list(k=50))

items_to_recommend <- 10

eval_prediction_ibcf <- predict(object = recommender_ibcf, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_prediction_ubcf <- predict(object = recommender_ubcf, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_prediction_svd <- predict(object = recommender_svd, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_prediction_svdf <- predict(object = recommender_svdf, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")
# compare RMSE for different models
######################RANDOM######################

#UBCF

eval_accuracy_ubcf <- calcPredictionAccuracy(
  x = eval_prediction_ubcf, data = getData(eval_sets, "unknown"), byUser = F)

eval_accuracy_ubcf_user <- calcPredictionAccuracy(
  x = eval_prediction_ubcf, data = getData(eval_sets, "unknown"), byUser = TRUE)


head(eval_accuracy_ubcf_user)



#IBCF
eval_accuracy_ibcf <- calcPredictionAccuracy(
  x = eval_prediction_ibcf, data = getData(eval_sets, "unknown"), byUser = F)

eval_accuracy_ibcf_user <- calcPredictionAccuracy(
  x = eval_prediction_ibcf, data = getData(eval_sets, "unknown"), byUser = TRUE)


head(eval_accuracy_ibcf_user)

#SVD
eval_accuracy_svd <- calcPredictionAccuracy(
  x = eval_prediction_svd, data = getData(eval_sets, "unknown"), byUser = F)

eval_accuracy_svd_user <- calcPredictionAccuracy(
  x = eval_prediction_svd, data = getData(eval_sets, "unknown"), byUser = TRUE)


head(eval_accuracy_svd_user)

#SVDF
eval_accuracy_svdf <- calcPredictionAccuracy(
  x = eval_prediction_svdf, data = getData(eval_sets, "unknown"), byUser = F)

eval_accuracy_svdf_user <- calcPredictionAccuracy(
  x = eval_prediction_svdf, data = getData(eval_sets, "unknown"), byUser = TRUE)


head(eval_accuracy_svdf_user)


eval_accuracy_ubcf
eval_accuracy_ibcf
eval_accuracy_svd
eval_accuracy_svdf
