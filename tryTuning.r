pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 2)

# Do a little preprocess
#source("preprocess/preprocess.r")

# Helpers function
source("helpers.r")

# Configuration variables
source("config.r")

# Packages manager
source("packages.r")

using("plyr", "stringr", "tm", "e1071", "caret", "pROC", "mlbench", "tictoc", "ROCR", "jsonlite")

set.seed(314)    # Set seed for reproducible results

###### BUILDING DOCUMENT TERM MATRIX DATASET
# dataset = read.csv2(dataset_file)
recipes = fromJSON("dataset/integrated-dataset.json")
dataset = fromCleanedRecipes(recipes)
dataset = ddply(dataset, "cuisine", function(x) x[sample(nrow(x), min(nrow(x), max_recipes)),])
### SPLIT DATA INTO K FOLDS ###
dataset = dataset[sample(nrow(dataset)),]
#Create num_fold equally size folds
folds = cut(seq(1, nrow(dataset)), breaks = num_fold, labels=FALSE)
results = vector(mode = "list", length = num_fold)
### PARAMETER LIST ###
cost <- 10^(2:4)
#gamma <- 10^(-2:2)
parms <- expand.grid(cost = cost)
### LOOP THROUGH PARAMETER VALUES ###
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  #g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(folds), .combine = rbind, .inorder = FALSE) %dopar% {
    testIndexes = which(folds == i, arr.ind=TRUE)
    test = dataset[testIndexes, ]
    train = dataset[-testIndexes, ]
    mdl = svm(cuisine ~ ., data = train, method = "C-classification", kernel = "linear", cost = c, probability = TRUE, scale = FALSE)
    pred = predict(mdl, test, decision.values = TRUE, probability = TRUE)
    data.frame(y = test$cuisine, prob = attributes(pred)$probabilities[, 2])
    gc()
  }
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob) 
  data.frame(parms[i, ], roc = roc$auc[1])
  gc()
}
