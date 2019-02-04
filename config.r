# Do not comment variables, instead change their values
zero_mean = FALSE
feature_selection = FALSE
do_balance = FALSE
roc = FALSE
k_fold = FALSE
max_recipes = 1500
num_fold = 10
dataset_file = "dataset/cleaned-dataset.json"
# Available models
# nb = naive bayes
# rf = random forest
model = "svmLinear3"


# Output filename based on config variables. Leave this at the end of config file
filename = toString(model)
if (zero_mean) {
  filename = paste(filename, "zero-mean", sep = "_")
}
if (feature_selection) {
  filename = paste(filename, "feat-sel", sep = "_")
}
if (do_balance) {
  str = paste(toString(max_recipes), "balanced", sep = "")
  filename = paste(filename, str, sep = "_")
}
if (k_fold) {
  str = paste(toString(num_fold), "-fold", sep = "")
  filename = paste(filename, toString(str), sep = "_")
}
filename = paste(filename, ".txt", sep="")
