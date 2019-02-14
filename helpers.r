# Packages manager
source("packages.r")
using("tm", "stringr")

# Build dataset in tabular form from list of recipes
fromCleanedRecipes = function (recipes) {
  corpus = VCorpus(VectorSource(recipes$ingredients))
  dataset = fromCorpus(corpus)
  # colnames(dataset)[colnames(dataset) == 'fats'] = 'fatss' # to avoid duplicate name fats
  # dataset = addFeatures(dataset, recipes)
  dataset = cbind(cuisine = recipes$cuisine, dataset)
  # colnames(dataset) = makeReadable(colnames(dataset))
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
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
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
  dataset = normalize(dataset, method = "range")
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

compute.A.conditional <- function(pred.matrix, i, j, ref.outcome) {
  # computes A(i|j), the probability that a randomly 
  # chosen member of class j has a lower estimated probability (or score) 
  # of belonging to class i than a randomly chosen member of class i
  
  # select predictions of class members
  i.idx <- which(ref.outcome == i)
  j.idx <- which(ref.outcome == j)
  pred.i <- pred.matrix[i.idx, i] # p(G = i) assigned to class i observations
  pred.j <- pred.matrix[j.idx, i] # p(G = i) assigned to class j observations
  all.preds <- c(pred.i, pred.j)
  classes <- c(rep(i, length(pred.i)), rep(j, length(pred.j)))
  o <- order(all.preds)
  classes.o <- classes[o]
  # Si: sum of ranks from class i observations
  Si <- sum(which(classes.o == i))
  ni <- length(i.idx)
  nj <- length(j.idx)
  # calculate A(i|j)
  A <- (Si - ((ni * (ni + 1))/2)) / (ni * nj)
  return(A)
}

multiclass.auc <- function(pred.matrix, ref.outcome) {
  labels <- colnames(pred.matrix)
  A.ij.cond <- utils::combn(labels, 2, function(x, pred.matrix, ref.outcome) {x
    i <- x[1]
    j <- x[2]
    A.ij <- compute.A.conditional(pred.matrix, i, j, ref.outcome)
    A.ji <- compute.A.conditional(pred.matrix, j, i, ref.outcome)
    pair <- paste0(i, "/", j)
    return(c(A.ij, A.ji))
  }, simplify = FALSE, pred.matrix = pred.matrix, ref.outcome = ref.outcome)
  c <- length(labels)
  pairs <- unlist(lapply(combn(labels, 2, simplify = FALSE), function(x) paste(x, collapse = "/")))
  A.mean <- unlist(lapply(A.ij.cond, mean))
  names(A.mean) <- pairs
  A.ij.joint <- sum(unlist(A.mean))
  M <- 2 / (c * (c-1)) * A.ij.joint 
  attr(M, "pair_AUCs") <- A.mean
  return(M)
}

