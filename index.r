# Configuration variables
source("config.r")

# Packages manager
source("packages.r")

using("e1071", "caret", "doParallel", "pROC", "mlbench", "tictoc", "rpart")

set.seed(314)    # Set seed for reproducible results
###### Redirect output to file
sink(paste("statistics/", filename, sep = ""))

###### READING DATASET
dataset = read.csv2(dataset_file)

###### FEATURE SELECTION
if (feature_selection) {
  tic("feature selection")
  # remove cuisine column for correlation
  correlationMatrix = cor(subset(dataset, select = -c(cuisine)))
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated = findCorrelation(correlationMatrix, cutoff=0.75)
  # Remove highly correlated features
  dataset = dataset[,-highlyCorrelated]
  toc()
}

###### PLOTS SECTION
#plot(dataset$cuisine) # See distribution of cuisines TODO: too much biases in dataset (see italian and mexican)
#cuisines = dataset$cuisine
#barplot(prop.table(table(cuisines)), las=2, cex.names=.9)

###### TRAINING AND TESTING
if (k_fold) {
  train_ctrl = trainControl(method = "cv", savePredictions = TRUE, classProbs = TRUE)
  if (model == "svm") {
    tic("10-fold cross validation SVM")
    # use 10-fold and extract correct information
    svm.model = train(cuisine ~ ., data=dataset, method = "svmLinear2", trControl = train_ctrl)
    toc()
    #svm.model$resample # accuracy - kappa - n° fold
    matrix = confusionMatrix(data = svm.model$pred$pred, reference = svm.model$pred$obs)
  } else if (model == "dec-tree") {
    tic("10-fold cross validation decision tree")
    # use 10-fold and extract correct information
    decisiontree.model = train(cuisine ~ ., data=dataset, method = "rpart", trControl = train_ctrl)
    toc()
    #svm.model$resample # accuracy - kappa - n° fold
    matrix = confusionMatrix(data = decisiontree.model$pred$pred, reference = decisiontree.model$pred$obs)
  }
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
    svm.model = svm(cuisine ~ ., data=train, method="C-classification", kernel="linear", probability = TRUE)
    prediction = predict(svm.model, test)
    toc()
  } else if (model == "dec-tree") {
    tic("normal training decision tree")
    decisiontree.model = rpart(cuisine ~ ., data=train, method="class")
    prediction = predict(decisiontree.model, test, type="class")
    toc()
  }
  matrix = confusionMatrix(test$cuisine, prediction)
  # Calculate AUC and ROC for every cuisine
  cuisine_auc = list()
  i = 1
  print("cuisine   -   AUC")
  for (cuisine in levels(prediction)) {
    copy_prediction = prediction
    copy_target = test$cuisine
    levels(copy_prediction)[levels(copy_prediction) != cuisine] = 0
    levels(copy_prediction)[levels(copy_prediction) == cuisine] = 1
    levels(copy_target)[levels(copy_target) != cuisine] = 0
    levels(copy_target)[levels(copy_target) == cuisine] = 1
    cuisine_auc[[cuisine]] = roc(as.numeric(copy_target), as.numeric(copy_prediction))
    i = i+1
  }
  colors = rainbow(length(levels(prediction)))
  plot(cuisine_auc[[levels(prediction)[1]]], col = colors[1], lty=1, lwd=1)
  for (i in 2:length(levels(prediction))) {
    plot(cuisine_auc[[levels(prediction)[i]]], col = colors[i], add = TRUE, lty=1, lwd=1)
  }
  legend(-0.02, 1, legend = levels(prediction), col = colors, lty=1, lwd=1)
}

###### Analyzing results
# TODO: plot svm parameters
# TODO: manual cross-validation 
print(matrix)
sink()
