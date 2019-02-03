# Do a little preprocess
source("preprocess/preprocess.r")

# Configuration variables
# source("config.r")

# Packages manager
source("packages.r")

using("plyr", "tm", "caret", "pROC", "mlbench", "tictoc", "rpart", "ROCR")

set.seed(314)    # Set seed for reproducible results

###### Redirect output to file
sink(paste("statistics/", filename, sep = ""))

###### BUILDING DOCUMENT TERM MATRIX DATASET
# dataset = read.csv2(dataset_file)
corpus = VCorpus(VectorSource(preproc_dataset$ingredients))
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
# Create a Document Term Matrix, in which each row is a recipe and each column is a term appearing in some recipe
# i,j = how many times term j appear in recipe i
dtm = DocumentTermMatrix(corpus)
# Adding as first column the recipe column
dataset = cbind(cuisine=preproc_dataset$cuisine, as.data.frame(as.matrix(dtm)))

##### FREE MEMORY
remove(ingredients, lastLength, regexes, toMatch, preproc_dataset, corpus, dtm)
gc()

###### ZERO MEAN SCALE
if (zero_mean) {
  tic("zero mean scaling")
  dataset[, 2:ncol(dataset)] = scale(dataset[, 2:ncol(dataset)])
  toc()
}

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
  ##### FREE MEMORY
  remove(correlationMatrix, highlyCorrelated)
  gc()
}

###### BALANCE DATASET
# Due to class unbalance of italian, southern_us and others cuisine
# one may want to reduce unbalance selecting at most 1.5K random recipe per cuisine 
if (do_balance) {
  dataset = ddply(dataset, "cuisine", function(x) x[sample(nrow(x), min(nrow(x), max_recipes)),])
}

###### PLOTS SECTION
#plot(dataset$cuisine) # See distribution of cuisines TODO: too much biases in dataset (see italian and mexican)
#cuisines = dataset$cuisine
#barplot(prop.table(table(cuisines)), las=2, cex.names=.9)


# TODO: da commentare
if (roc) {
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
}

if (k_fold) {
  tic("K-FOLD")
  # Randomly shuffle the dataset
  dataset = dataset[sample(nrow(dataset)),]
  #Create num_fold equally size folds
  folds = cut(seq(1, nrow(dataset)), breaks = num_fold, labels=FALSE)
  measures = data.frame(precision = numeric(0), recall = numeric(0), fmeasure = numeric(0))
  #Perform num_fold cross validation
  for(i in 1:1){
    #Segement data by fold 
    testIndexes = which(folds == i, arr.ind=TRUE)
    test = dataset[testIndexes, ]
    train = dataset[-testIndexes, ]
    fit = train(cuisine ~., train, method = toString(model))
    predictions = predict(fit, test)
    # correct_count = sum(predictions == dataset[ind == i,]$cuisine)
    # accuracies = append(correct_count / nrow(dataset[ind ==i,]), accuracies)
    result = confusionMatrix(data = predictions, reference = test$cuisine, mode="prec_recall")
    gc()
  }
}

###### Analyzing results
# TODO: plot svm parameters
# TODO: manual cross-validation 
# print(matrix)
sink(NULL)


