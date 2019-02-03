print("STARTING PREPROCESS")
source("config.r")
source("packages.r")
using("jsonlite", "stringr", "tictoc")
print("LOADING DATASET")
preproc_dataset = fromJSON(toString(dataset_file))
# Apply to the ingredients list the tolower function
# lapply is the same as in LISP
preproc_dataset$ingredients <- lapply(preproc_dataset$ingredients, function(x) {tolower(x)})
# Perform an union of all the ingredients sublists
# Reduce is the same as in LISP
ingredients <- Reduce(union, preproc_dataset$ingredients)
# ingredients <- sort(tolower(ingredients))
# Read all the regex placed in regexes.txt file
regexes <- readLines(file("preprocess/regexes_2.txt"))
# Collapse them all in one big regex
toMatch <- paste(regexes, collapse="|")
lastLength = length(ingredients)
# Apply the regex to all the ingredients sublist, replace the matched text with an empty string
# Perform an union over the sublists and check if the length of ther obtained list is greater than the previous one
# If so, perform another round
# Otherwise stop and exit
print("CLEANING DATASET")
repeat {
  preproc_dataset$ingredients <- lapply(preproc_dataset$ingredients, function(x) {gsub(toMatch, replacement = "", x)})
  ingredients <- Reduce(union, preproc_dataset$ingredients);
  # print(lastLength)
  # print(length(ingredients))
  if(length(ingredients) >= lastLength){
    break
  } else {
    lastLength = length(ingredients)
  }
}
print("DONE")
# write_json(dataset, "dataset/regex-cleaned-dataset.json")

