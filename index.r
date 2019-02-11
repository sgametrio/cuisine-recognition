# Do a little preprocess
#source("preprocess/preprocess.r")

# Helpers function
source("helpers.r")

# Configuration variables
source("config.r")

# Packages manager
source("packages.r")

using("plyr", "BBmisc", "HandTill2001", "e1071", "stringr", "tm", "caret", "pROC", "ROCR", "mlbench", "tictoc", "jsonlite")

set.seed(314)    # Set seed for reproducible results

###### Redirect output to file
sink(paste("statistics/", filename, ep = ""))

###### BUILDING DOCUMENT TERM MATRIX DATASET
# dataset = read.csv2(dataset_file)
recipes = fromJSON("dataset/integrated-dataset.json")
dataset = fromCleanedRecipes(recipes)
#corpus = VCorpus(VectorSource(recipes$ingredients_ids))
#corpus = tm_map(corpus, removeWords, stopwords())
#corpus = tm_map(corpus, removePunctuation)
#corpus = tm_map(corpus, stripWhitespace)
# Create a Document Term Matrix, in which each row is a recipe and each column is a term appearing in some recipe
# i,j = how many times term j appear in recipe i
#dtm = DocumentTermMatrix(corpus)
#dataset = as.data.frame(as.matrix(dtm))
#dataset = cbind(kcal = recipes$kcal, dataset)
#dataset = cbind(proteins = recipes$proteins, dataset)
#dataset = cbind(fat = recipes$fat, dataset)
#dataset = cbind(carbs = recipes$carbs, dataset)
# Adding as first column the recipe column
#dataset = cbind(cuisine = recipes$cuisine, dataset)

##### FREE MEMORY
remove(ingredients, lastLength, recipes, regexes, toMatch, preproc_dataset, corpus, dtm)
gc()

###### ZERO MEAN SCALE
if (zero_mean) {
  tic("zero mean scaling")
  dataset[, 6:ncol(dataset)] = scale(dataset[, 6:ncol(dataset)])
  toc()
}

###### FEATURE SELECTION
if (feature_selection) {
  cuisine_tmp = dataset$cuisine
  tic("feature selection")
  # remove cuisine column for correlation
  correlationMatrix = cor(subset(dataset, select = -c(cuisine, carbs, fats, kcal, proteins)))
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated = findCorrelation(correlationMatrix, cutoff=0.5)
  # Remove highly correlated features
  dataset = dataset[,-highlyCorrelated]
  toc()
  dataset = cbind(cuisine=cuisine_tmp, dataset[, !(names(dataset) == "cuisine")])
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
# plot(dataset$cuisine) # See distribution of cuisines TODO: too much biases in dataset (see italian and mexican)
# cuisines = dataset$cuisine
# barplot(prop.table(table(cuisines)), las=2, cex.names=.9)
# freq = sort(colSums(as.matrix(dtm)), decreasing = T) # Words frequencies
# wf = data.frame(word=names(freq), freq=freq) # Data frame of frequencies
# Plot words frequenices greater than 2500
# hist = ggplot(subset(wf, freq > 2500), aes(x = reorder(word, -freq), y = freq)) +
#        geom_bar(stat = "identity") +
#        theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print wordcloud
# wc = wordcloud(names(freq), freq, min.freq=500)


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
    aucs = matrix(nrow = 20, ncol = 1, dimnames = list(levels(dataset$cuisine), c("roc")))
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
      aucs[i, 1] = unlist(performance(rocr_pred.rocr, measure = "auc", x.measure = "cutoff")@y.values)
      # perf.tpr.rocr = performance(rocr_pred.rocr, "tpr","fpr")
      # add = TRUE
      # if (i == 1) {
      #   add = FALSE
      # }
      # plot(perf.tpr.rocr, main=paste(paste(cuisine, ": ", sep=""),(perf.rocr@y.values)), add = add, col = colors[i])
      i = i+1
      gc()
    }
    # legend(0.85, 0.92, 1, legend = levels(dataset$cuisine), col = colors, lty=1, lwd=1)
  }
}

if (k_fold) {
  tic("K-FOLD")
  # bestFit = NULL
  # Colors for plots
  colors = rainbow(length(levels(dataset$cuisine)))
  # Randomly shuffle the dataset
  dataset  = dataset[sample(nrow(dataset)),]
  #Create num_fold equally size folds
  folds = cut(seq(1, nrow(train)), breaks = num_fold, labels=FALSE)
  results = vector(mode = "list", length = num_fold)
  #Perform num_fold cross validation
  for(fold in 1:num_fold){
    print(paste("Fold: ", toString(fold)))
    #Segement data by fold 
    testIndexes = which(folds == fold, arr.ind=TRUE)
    test = dataset[testIndexes, ]
    train = dataset[-testIndexes, ]
    fit = svm(cuisine ~ ., train, method = "C-classification", kernel = "linear", probability = TRUE, scale = FALSE)
    predictions = predict(fit, test, probability = TRUE)
    # correct_count = sum(predictions == dataset[ind == i,]$cuisine)
    # accuracies = append(correct_count / nrow(dataset[ind ==i,]), accuracies)
    confMat = confusionMatrix(data = predictions, reference = test$cuisine, mode="everything")
    # Saving statistics to a list (we can iterate through this using lapply)
    rocPerClass = matrix(nrow = 20, ncol = 1, dimnames = list(levels(dataset$cuisine), c("roc")))
    i = 1
    for (cuisine in levels(dataset$cuisine)) {
      add = TRUE
      main = ""
      pred = prediction(attr(predictions, "probabilities")[, cuisine], test$cuisine == cuisine)
      rocPerClass[i, 1] = as.numeric(performance(pred, "auc")@y.values)
      # Plot ROC
      if (i == 1) {
        add = FALSE
        main = paste("ROCs in fold ", fold)
      }
      plot(performance(pred, "tpr", "fpr"), add = add, col = colors[i], main = main)
      rocPerClass[i, 1] = as.numeric(multiclass.roc(test$cuisine == cuisine, attr(predictions, "probabilities")[, cuisine])$auc)
      i = i + 1
    }
    legend("bottomright", 1, legend = levels(test$cuisine), col = colors, lty=1, lwd=1, cex = 0.75, bty = "n")
    results[[fold]] = list(confMat=confMat$table,
                           overall=as.data.frame(as.matrix(confMat, what = "overall")), 
                           classes=as.data.frame(as.matrix(confMat, what = "classes")),
                           # Overall roc in i-th fold
                           roc=as.numeric(multiclass.auc(attr(predictions, "probabilities"), test$cuisine)),
                           rocPerClass=as.data.frame(rocPerClass)
                      )
    # Choose bet fit 
    # if (fold == 1) {
    #   bestFit = fit
    # } else {
    #   maxRoc = max(unlist(lapply(results[1:fold-1], function(x) as.numeric(x$roc))))
    #   if (results[[fold]]$roc > maxRoc) {
    #     bestFit = fit
    #   }
    # }
    # Print to file
    print("Confusion matrix")
    print(results[[fold]]$confMat)
    print("Overall stats")
    print(results[[fold]]$overall)
    print("Class stats")
    print(results[[fold]]$classes)
    print(paste("Overall ROC: ", results[[fold]]$roc))
    print(paste("ROC per class: "))
    print(results[[fold]]$rocPerClass)
    gc()
  }
} else {
  # Number of observations
  n = nrow(dataset)  
  # 75% for training set
  ntrain = round(n * 0.75)  
  # Create a random index
  tindex = sample(n, ntrain) 
  # Create training set
  train = dataset[tindex, ] 
  # Create test set
  test = dataset[-tindex, ]   
  fit = svm(cuisine ~ ., data = train, method = "C-classification", kernel = "linear", probability = TRUE, scale = FALSE)
  predictions = predict(fit, test, probability = TRUE)
  confMat = confusionMatrix(data = predictions, reference = test$cuisine, mode="everything")
  # Saving statistics to a list (we can iterate through this using lapply)
  rocPerClass = matrix(nrow = 20, ncol = 1, dimnames = list(levels(dataset$cuisine), c("roc")))
  i = 1
  for (cuisine in levels(dataset$cuisine)) {
    add = TRUE
    main = ""
    pred = prediction(attr(predictions, "probabilities")[, cuisine], test$cuisine == cuisine)
    rocPerClass[i, 1] = as.numeric(performance(pred, "auc")@y.values)
    # Plot ROC
    if (i == 1) {
      add = FALSE
      main = "ROCs"
    }
    plot(performance(pred, "tpr", "fpr"), add = add, col = colors[i], main = main)
    # rocPerClass[i, 1] = as.numeric(multiclass.roc(test$cuisine == cuisine, attr(predictions, "probabilities")[, cuisine])$auc)
    i = i + 1
  }
  legend("bottomright", 1, legend = levels(dataset$cuisine), col = colors, lty=1, lwd=1, cex = 0.75, bty = "n")
  results = list(confMat=confMat$table,
                 overall=as.data.frame(as.matrix(confMat, what = "overall")), 
                 classes=as.data.frame(as.matrix(confMat, what = "classes")),
                 # Overall roc
                 # roc=as.numeric(multiclass.roc(test$cuisine, attr(predictions, "probabilities")[, 2])$auc),
                 roc=as.numeric(multiclass.auc(attr(predictions, "probabilities"), test$cuisine)),
                 rocPerClass=as.data.frame(rocPerClass)
  ) 
  # Print to file
  print("Confusion matrix")
  print(results$confMat)
  print("Overall stats")
  print(results$overall)
  print("Class stats")
  print(results$classes)
  print(paste("Overall ROC: ", results$roc))
  print(paste("ROC per class: "))
  print(results$rocPerClass)
}

if (!k_fold) {
  overallPerClass = matrix(nrow = 20, ncol = 3, dimnames = list(levels(dataset$cuisine), c("Precision", "Recall", "F1")))
  for (i in 1:20) {
    for (j in 5:7) {
      overallPerClass[i, j-4] = unlist(as.numeric(results$classes[j, i]) )
    }
  }
  print("Avg accuracy")
  print(results$overall[1,])
} else {
  overallPerClass = matrix(nrow = 20, ncol = 3, dimnames = list(levels(dataset$cuisine), c("Precision", "Recall", "F1")))
  for (i in 1:20) {
    for (j in 5:7) {
      overallPerClass[i, j-4] = mean(unlist(lapply(results, function(x) { as.numeric(x$classes[j, i]) })))
    }
  }
  print("Avg accuracy")
  print(mean(sapply(results, function(x) x$overall[1,])))
}

print("Overall measure per class")
print(overallPerClass)

print("Avg precision")
print(mean(overallPerClass[, 1]))

print("Avg recall")
print(mean(overallPerClass[, 2]))

print("Avg F1")
print(mean(overallPerClass[, 3]))

###### Analyzing results
# TODO: plot svm parameters
# TODO: manual cross-validation 
# print(matrix)
sink(NULL)


