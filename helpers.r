# Packages manager
source("packages.r")
using("tm")

# Build dataset in tabular form from list of recipes
fromCleanedRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$mapped_ingredients))
  dataset = fromCorpus(corpus)
  dataset = addFeatures(dataset, recipes)
  cbind(cuisine = recipes$cuisine, dataset)
}

fromRawRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$ingredients))
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  dataset = fromCorpus(corpus)
  colnames(dataset)[colnames(dataset) == 'fats'] = 'fatss' # to avoid duplicate name fats
  dataset = addFeatures(dataset, recipes)
  cbind(cuisine = recipes$cuisine, dataset)
}

fromCorpus = function (corpus) {
  dtm = DocumentTermMatrix(corpus)
  dataset = as.data.frame(as.matrix(dtm))
  dataset
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