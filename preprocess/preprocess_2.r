# library(jsonlite)
# library(stringr)
source("packages.r")
using("jsonlite", "stringr", "spacyr", "tictoc")
tic("Preprocess spaCy")
trainset = fromJSON("dataset/original-dataset.json")
# Apply to the ingredients list the tolower function
# lapply is the same as in LISP
# trainset$ingredients <- lapply(trainset$ingredients, function(x) {tolower(x)})
# Perform an union of all the ingredients sublists
# Reduce is the same as in LISP
# ingredients <- Reduce(union, trainset$ingredients)
# ingredients <- sort(tolower(ingredients))
# Read all the regex placed in regexes.txt file
regexes <- readLines(file("preprocess/regexes_2.txt"))
# Collapse them all in one big regex
toMatch <- paste(regexes, collapse="|")
trainset$ingredients <- lapply(trainset$ingredients, function(x) {gsub(toMatch, replacement = "", tolower(x))})
# unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")
spacy_initialize()
u <- unlist(trainset$ingredients)
u <- sapply(u, 
            function(x) {
              # tags = spacy_parse(x, pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE, dependency = FALSE, nounphrase = FALSE, multithreading = TRUE)
              # ifelse(length(tags$token) > 1, paste(tags$lemma[which(tags$pos == "NOUN")], collapse = " "), tags$lemma)
              tags = spacy_parse(x, pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE, dependency = FALSE, nounphrase = FALSE, multithreading = TRUE); 
              ifelse(length(tags$token) > 1, paste(tags$lemma[!(tags$pos %in% c("ADJ", "VERB", "PUNCT"))], collapse = " "), tags$lemma)
            })
trainset$ingredients <- relist(u, trainset$ingredients)
# trainset$ingredients <- lapply(trainset$ingredients, function(x) {gsub(toMatch, replacement = "", x)})
ingredients <- Reduce(union, trainset$ingredients);
# lastLength = length(ingredients)
# Apply the regex to all the ingredients sublist, replace the matched text with an empty string
# Perform an union over the sublists and check if the length of ther obtained list is greater than the previous one
# If so, perform another round
# Otherwise stop and exit
# repeat {
#   trainset$ingredients <- lapply(trainset$ingredients, function(x) {gsub(toMatch, replacement = "", x)})
#   ingredients <- Reduce(union, trainset$ingredients);
#   print(lastLength)
#   print(length(ingredients))
#   if(length(ingredients) >= lastLength){
#     break
#   } else {
#     lastLength = length(ingredients)
#   }
# }
toc()
write_json(trainset, "dataset/regex-spacy-cleaned-dataset.json")
# At the end of the script we have:
# The "ingredients" var, containing all the possible ingredients
# Every ingredients sublist in "trainset$ingredients" cleaned due to the regex removal
# We've maintained the corrispondence between trainset$ingredients and ingredients
# The next step is to create a data.frame or a table in which we'll have:
# A cuisine per row (maybe downsampled due to unbalance of italian, mexican and souther_us cuisines wrt the others)
# An ingredient per column
# t(i,j) = 1, if cuisine i contains ingredient j, 0 otherwise (where t is the data.frame/table)
# Maybe it would be claver take in account also how many time an ingredient is used in a particular cuisine (percentage)


