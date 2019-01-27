# Configuration variables
source("config.r")

# Packages manager
source("packages.r")

# Loading libs
# library(e1071) # svm
# library(caret) # train and trainControl
# library(doParallel) # parallelizing train function
# library(pROC) # ROC curve
# library(mlbench) # for now nothing
# library(tictoc) # timings

using("e1071", "caret", "doParallel", "pROC", "mlbench", "tictoc", "rpart")

set.seed(314)    # Set seed for reproducible results
###### Redirect output to file
sink(paste("statistics/", filename, sep = ""))

###### READING INPUT
#dataset = read.csv2("dataset/matrix_train.csv")
#dataset = read.csv2("dataset/max_recipes.csv")
dataset = read.csv2(toString(use_dataset))

###### FEATURE SELECTION
if (feature_selection) {
  tic("feature selection")
  # remove cuisine column for correlation
  correlationMatrix = cor(subset(dataset, select = -c(cuisine)))
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated = findCorrelation(correlationMatrix, cutoff=0.75)
  # Remove highly correlated features
  for (variable in highlyCorrelated) {
    dataset = dataset[,-variable]
  }
  toc()
}

###### PLOTS SECTION
#plot(dataset$cuisine) # See distribution of cuisines TODO: too much biases in dataset (see italian and mexican)
#cuisines = dataset$cuisine
#barplot(prop.table(table(cuisines)), las=2, cex.names=.9)

###### TRAINING AND TESTING
if (k_fold) {
  train_ctrl = trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
  cl = makePSOCKcluster(2)
  registerDoParallel(cl, cores = 2)
  if (model == "svm") {
    tic("10-fold cross validation SVM")
    # 10-fold cross-validation
    # train_ctrl = trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
    # cl = makePSOCKcluster(2)
    # registerDoParallel(cl, cores = 2)
    # use 10-fold and extract correct information
    svm.model = train(cuisine ~ ., data=dataset, method = "svmLinear2", trControl = train_ctrl)
    stopCluster(cl)
    toc()
    #svm.model$resample # accuracy - kappa - n° fold
    matrix = confusionMatrix(data = svm.model$pred$pred, reference = svm.model$pred$obs)
  } else if (model == "dec-tree") {
    tic("10-fold cross validation decision tree")
    # 10-fold cross-validation
    # train_ctrl = trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
    # cl = makePSOCKcluster(2)
    # registerDoParallel(cl, cores = 2)
    # use 10-fold and extract correct information
    decisiontree.model = train(cuisine ~ ., data=dataset, method = "rpart", trControl = train_ctrl)
    stopCluster(cl)
    toc()
    #svm.model$resample # accuracy - kappa - n° fold
    matrix = confusionMatrix(data = decisiontree.model$pred$pred, reference = decisiontree.model$pred$obs)
  }
  tic("10-fold cross validation training and testing")
  # 10-fold cross-validation
  train_ctrl = trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
  tic("10-fold cross validation training and testing")
  # 10-fold cross-validation
  train_ctrl = trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
  #cl = makePSOCKcluster(2)
  #registerDoParallel(cl, cores = 2)
  # use 10-fold and extract correct information
  svm.model = train(cuisine ~ ., data=dataset, method = "svmLinear2", trControl = train_ctrl)
  #stopCluster(cl)
  toc()
  #svm.model$resample # accuracy - kappa - n° fold
  matrix = confusionMatrix(data = svm.model$pred$pred, reference = svm.model$pred$obs)
} else {
  n = nrow(dataset)  # Number of observations
  ntrain = round(n*0.75)  # 75% for training set
  tindex = sample(n, ntrain)   # Create a random index
  train = dataset[tindex,]   # Create training set
  test = dataset[-tindex,]   # Create test set
  if (model == "svm") {
    tic("normal training SVM")
    svm.model = svm(cuisine ~ ., data=train, method="C-classification", kernel="linear")
    prediction = predict(svm.model, test)
    toc()
  } else if (model == "dec-tree") {
    tic("normal training decision tree")
    decisiontree.model = rpart(cuisine ~ ., data=train, method="class")
    prediction = predict(decisiontree.model, test, type="class")
    toc()
  }
  matrix = confusionMatrix(test$cuisine, prediction)
}

###### Analyzing results
# TODO: plot svm parameters
print(matrix)
sink()
