# Configuration variables
source("config.r")

# Packages manager
source("packages.r")

using("e1071", "caret", "doParallel", "pROC", "mlbench", "tictoc", "rpart", "ROCR")

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


# TODO: da commentare
if (model == modelsvm) {
  n = nrow(dataset)  # Number of observations
  ntrain = round(n*0.75)  # 75% for training set
  tindex = sample(n, ntrain)   # Create a random index
  train = dataset[tindex,]   # Create training set
  test = dataset[-tindex,]   # Create test set
  i = 1
  # Compute N binary SVMs and respective ROCs
  colors = rainbow(length(levels(dataset$cuisine)))
  for (cuisine in levels(dataset$cuisine)) {
    binary_test = test
    binary_train = train
    levels(binary_test$cuisine)[levels(binary_test$cuisine) != cuisine] = "0"
    levels(binary_test$cuisine)[levels(binary_test$cuisine) == cuisine] = "1"
    levels(binary_train$cuisine)[levels(binary_train$cuisine) != cuisine] = "0"
    levels(binary_train$cuisine)[levels(binary_train$cuisine) == cuisine] = "1"
    svm.rocr.model = svm(cuisine ~ ., data = binary_train, method = "C-classification", kernel = "linear", probability = TRUE)
    rocr_pred = predict(svm.rocr.model, binary_test, probability = TRUE)
    rocr_pred.prob = attr(rocr_pred, "probabilities")
    rocr_pred.to.roc = rocr_pred.prob[, "1"]
    rocr_pred.rocr = prediction(rocr_pred.to.roc, binary_test$cuisine)
    perf.rocr = performance(rocr_pred.rocr, measure = "auc", x.measure = "cutoff")
    perf.tpr.rocr = performance(rocr_pred.rocr, "tpr","fpr")
    add = TRUE
    if (i == 1) {
      add = FALSE
    }
    plot(perf.tpr.rocr, main=paste(paste(cuisine, ": ", sep=""),(perf.rocr@y.values)), add = add, col = colors[i])
    i = i+1
  }
  legend(0.85, 0.92, 1, legend = levels(dataset$cuisine), col = colors, lty=1, lwd=1)
}
###### Analyzing results
# TODO: plot svm parameters
# TODO: manual cross-validation 
print(matrix)
sink()


