# Packages manager
source("packages.r")
using("tm", "stringr")

# Build dataset in tabular form from list of recipes
fromCleanedRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$mapped_ingredients))
  dataset = fromCorpus(corpus)
  # colnames(dataset)[colnames(dataset) == 'fats'] = 'fatss' # to avoid duplicate name fats
  dataset = addFeatures(dataset, recipes)
  dataset = cbind(cuisine = recipes$cuisine, dataset)
  colnames(dataset) = makeReadable(colnames(dataset))
  dataset
}

fromRawRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$ingredients))
  dataset = fromCorpus(corpus)
  colnames(dataset)[colnames(dataset) == 'fats'] = 'fatss' # to avoid duplicate name fats
  dataset = addFeatures(dataset, recipes)
  dataset = cbind(cuisine = recipes$cuisine, dataset)
  colnames(dataset) = makeReadable(colnames(dataset))
}

fromCorpus = function (corpus) {
  # corpus = tm_map(corpus, removeWords, stopwords())
  # corpus = tm_map(corpus, removePunctuation)
  # corpus = tm_map(corpus, stripWhitespace)
  dtm = DocumentTermMatrix(corpus)
  dataset = as.data.frame(as.matrix(dtm))
}

addFeatures = function(dataset, recipes) {
  cols = c("kcal", "proteins", "fats", "carbs")
  dataset = cbind(kcal = recipes$kcal, dataset)
  dataset = cbind(proteins = recipes$proteins, dataset)
  dataset = cbind(fats = recipes$fats, dataset)
  dataset = cbind(carbs = recipes$carbs, dataset)
  # Scale attributes
  dataset[, cols] = scale(dataset[, cols])
  dataset
}

# Change ingredients ids with their original mapping name for readability purpose
makeReadable = function(ids) {
  mappings = fromJSON("dataset/ingredients-linkage.json")
  unlist(lapply(ids, function(x) {
    mapped = mappings$original[which(mappings$mapped == x)];
    if (length(mapped) > 0) {
      mapped[which.min(lapply(str_split(mapped, " "), length))]
    } else {
      x
    }
  }))
}

featureSelection = function (dataset, cutoff) {
  subset = subset(dataset, select = -c(cuisine))
  correlation_matrix = cor(subset)
  highly_correlated_features = findCorrelation(correlation_matrix, cutoff=cutoff)
  # Remove highly_correlated_features because gives no information
  dataset[, -highly_correlated_features]
}