# Extract functions to helpers.r
source("helpers.r")
# Read configuration variables
source("config.r")
# Packages manager
source("packages.r")
using("plyr", "jsonlite", "e1071", "pROC", "mlbench", "tictoc", "ROCR", "caret", "DataExplorer", "corrplot")

set.seed(314)    # Set seed for reproducible results

# Redirect output to file
#sink(paste("statistics/", filename, sep = ""))

### Building training set from json
recipes = fromJSON("dataset/integrated-dataset.json")

### Plot esplorativi
#summary(recipes)

dataset = fromCleanedRecipes(recipes)
#dataset = fromRawRecipes(recipes)

# Continent column creation
asian = c("japanese", "vietnamese", "korean", "thai", "chinese", "filipino", "indian")
north_american = c("jamaican", "cajun_creole", "mexican")
south_american = c("southern_us", "brazilian")
mediterranean = c("greek", "italian", "moroccan", "spanish")
european = c("british", "french", "irish", "russian")
continent = dataset$cuisine
levels(continent)[levels(continent) %in% asian] = "asian"
levels(continent)[levels(continent) %in% north_american] = "north_american"
levels(continent)[levels(continent) %in% south_american] = "south_american"
levels(continent)[levels(continent) %in% mediterranean] = "mediterranean"
levels(continent)[levels(continent) %in% european] = "european"
if (target == "continent") {
  cuisine = dataset$cuisine
  dataset$cuisine = continent
}

### Plots on unscaled dataset
# Unscale data
unscaled = dataset
nutrients = c("kcal", "proteins", "fats", "carbs")
unscaled[, nutrients] = recipes[, nutrients]

palette = c("#fd4445", "#ffaf00", "#0091cf", "#01395e", "#05a58d")
plot(continent, col = palette, main = "Cuisine distribution by continent")
barplot(table(cuisine), las=2, cex.names=.9, main = "Cuisine distribution", ylim = c(0,8000))
sub = unscaled[, c("cuisine", "carbs", "fats", "kcal", "proteins")]
per_cuisine_recipe_nutrients = aggregate(. ~ cuisine, data = sub, mean)
plot_correlation(per_cuisine_recipe_nutrients[, nutrients]) # Too much correlation on average nutrients per cuisine
plot_correlation(unscaled[, nutrients]) # Too much correlation on nutrients
plot_correlation(sub) # correlation between cuisines and nutrients
corrMatrix = cor(per_cuisine_recipe_nutrients[, nutrients])
corrplot(corrMatrix, type = "upper", tl.col = "black", tl.srt = 0, title = "Correlation between nutrients",addCoef.col = "white")
# TODO: analyze ingredients counts

### Reduce instances to reduce bias
# Due to class unbalance of italian, southern_us and others cuisine
# one may want to reduce unbalance selecting at most 1.5K random recipe per cuisine 
if (do_balance && target == "cuisine") {
  dataset = ddply(dataset, "cuisine", function(x) x[sample(nrow(x), min(nrow(x), max_recipes)),])
}

### Dataset shuffling and training-test creation
dataset = dataset[sample(nrow(dataset)),]
folds = cut(seq(1, nrow(dataset)), breaks = num_fold, labels=FALSE)

### Feature Selection
if (feature_selection) {
  dataset = featureSelection(dataset, cut_off)
}

### Folds creation
classes = levels(dataset$cuisine)
testIndexes = which(folds == 1, arr.ind=TRUE)
test = dataset[testIndexes, ]
train = dataset[-testIndexes, ]

### Multiclass SVM
num_fold = 1
multiclass_perf = vector("list", num_fold)
rocs = vector("list", length(classes))
mean_aucs = vector(length = length(classes), mode = "numeric")
for (fold in 1:num_fold) {
  testIndexes = which(folds == fold, arr.ind = TRUE)
  train = dataset[-testIndexes, ]
  test = dataset[testIndexes, ]
  fit = svm(cuisine ~ ., data = train, kernel = "linear", method = "C-classification", probability = TRUE, cost = 2)
  predictions = predict(fit, test, probability = TRUE)
  multiclass_perf[[fold]] = confusionMatrix(data = predictions, reference = test$cuisine, mode="prec_recall")
  
  # To avoid memory consumption
  multiclass_perf[[fold]] = multiclass_perf[[fold]]$overall["Accuracy"]
  # Plot ROCs
  j = 1
  for (cuisine in classes) {
    roc = multiclass.roc(test$cuisine, attr(predictions, "probabilities")[, cuisine])
    #rocs[[j]] = roc
    mean_aucs[[j]] = mean_aucs[[j]] + roc$auc
    j = j+1
  }
  print(fold)
}

summary(fit)



# TODO: compute mean aucs and performances
for (i in length(classes)) {
  mean_aucs[[i]] = mean_aucs[[i]] / num_fold
}
print(mean_aucs)

mean_acc = 0
for (i in 1:num_fold) {
  mean_acc = mean_acc + multiclass_perf[[i]]
}
print(mean_acc / num_fold)


### Binary SVMs
performance = vector("list", length(classes))
i = 1
colors = rainbow(length(classes))
for (cuisine in classes) {
  binary_test = test
  binary_train = train
  # Transform multiclass problem to binary
  levels(binary_test$cuisine)[levels(binary_test$cuisine) != cuisine] = "0"
  levels(binary_test$cuisine)[levels(binary_test$cuisine) == cuisine] = "1"
  levels(binary_train$cuisine)[levels(binary_train$cuisine) != cuisine] = "0"
  levels(binary_train$cuisine)[levels(binary_train$cuisine) == cuisine] = "1"
  svm.rocr.model = svm(cuisine ~ ., data = binary_train, method = "C-classification", kernel = "linear", probability = TRUE)
  rocr_pred = predict(svm.rocr.model, binary_test, probability = TRUE)
  rocr_pred.prob = attr(rocr_pred, "probabilities")
  rocr_pred.to.roc = rocr_pred.prob[, "1"]
  rocr_pred.rocr = prediction(rocr_pred.to.roc, binary_test$cuisine)
  performance[[i]] = rocr_pred.rocr
  perf.rocr = performance(rocr_pred.rocr, measure = "auc", x.measure = "cutoff")
  perf.tpr.rocr = performance(rocr_pred.rocr, "tpr","fpr")
  add = TRUE
  if (i == 1) {
    add = FALSE
  }
  plot(perf.tpr.rocr, add = add, col = colors[i])
  print(i)
  i = i+1
}
legend(0.85, 0.92, 1, legend = levels(dataset$cuisine), col = colors, lty=1, lwd=1)
# In performance ho le metriche per tipo di cucina
print(performance)