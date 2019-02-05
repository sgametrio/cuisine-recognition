# Build dataset in tabular form from list of recipes
fromCleanedRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$ingredients_ids))
  fromCorpus(corpus, recipes)
}

fromRawRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$ingredients))
  fromCorpus(corpus, recipes)
}

fromCorpus = function (corpus, recipes) {
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  dtm = DocumentTermMatrix(corpus)
  dataset = as.data.frame(as.matrix(dtm))
  dataset = cbind(kcal = recipes$kcal, dataset)
  dataset = cbind(proteins = recipes$proteins, dataset)
  dataset = cbind(fat = recipes$fat, dataset)
  dataset = cbind(carbs = recipes$carbs, dataset)
  # Adding as first column the cuisine (target) column
  dataset = cbind(cuisine = recipes$cuisine, dataset)
  dataset
}