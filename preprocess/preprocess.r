source("config.r")
source("packages.r")
using("jsonlite", "stringr", "tictoc")
# sink(paste("statistics/", filename, sep = ""))
print("STARTING PREPROCESS")
print("LOADING DATASET")
recipes = fromJSON("dataset/recipes-dataset.json")
# Apply to the ingredients list the tolower function
# lapply is the same as in LISP
# recipes$ingredients <- lapply(recipes$ingredients, function(x) {tolower(x)})
# Perform an union of all the ingredients sublists
# Reduce is the same as in LISP
ingredients <- Reduce(union, recipes$ingredients)
# ingredients <- sort(tolower(ingredients))
# Read all the regex placed in regexes.txt file
regexes <- readLines(file("preprocess/regexes_3.txt"))
# Collapse them all in one big regex
toMatch <- paste(regexes, collapse="|")
lastLength = length(ingredients)
# Apply the regex to all the ingredients sublist, replace the matched text with an empty string
# Perform an union over the sublists and check if the length of ther obtained list is greater than the previous one
# If so, perform another round
# Otherwise stop and exit
print("CLEANING DATASET")
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl) 
repeat {
  recipes$ingredients <- lapply(recipes$ingredients, function(x) {gsub(toMatch, replacement = "", x)})
  ingredients <- Reduce(union, recipes$ingredients);
  print(lastLength)
  # print(length(ingredients))
  if(length(ingredients) >= lastLength){
    break
  } else {
    lastLength = length(ingredients)
  }
}
# recipes$ingredients <- lapply(recipes$ingredients, function(x) {paste(x, collapse = " ")})
# stopCluster(cl)
print("DONE")
# write_json(dataset, "dataset/regex-cleaned-dataset.json")

