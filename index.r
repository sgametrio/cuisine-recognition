# Configuration variables
k_fold <- FALSE

# Loading libs
library(e1071) # svm
library(caret) # train and trainControl
library(doParallel) # parallelizing train function
library(pROC) # ROC curve

#dataset = read.csv2("dataset/matrix_train.csv")
dataset = read.csv2("dataset/max2k_recipes.csv")
#plot(dataset$cuisine) # See distribution of cuisines TODO: too much biases in dataset (see italian and mexican)
cuisines = dataset$cuisine
barplot(prop.table(table(cuisines)), las=2, cex.names=.9)

# Don't know if this frees memories
#dataset <- NULL

n <- nrow(dataset)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train <- dataset[tindex,]   # Create training set
test <- dataset[-tindex,]   # Create test set

if (k_fold) {
  # 10-fold cross-validation
  train_ctrl <- trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  # use 10-fold and extract correct information
  svm.model <- train(cuisine ~ ., data=dataset, method = "svmLinear2", trControl = train_ctrl)
  stopCluster(cl)
  #svm.model$resample # accuracy - kappa - n° fold
  matrix <- confusionMatrix(data = svm.model$pred$pred, reference = svm.model$pred$obs)
} else {
  svm.model <- svm(cuisine ~ ., data=train, method="C-classification", kernel="linear")
  prediction <- predict(svm.model, test)
  matrix <- confusionMatrix(test$cuisine, prediction)
}


