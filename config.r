# Do not comment variables, instead change their values
k_fold = FALSE
feature_selection = FALSE
dataset_file = "dataset/max_recipes.csv"
modelsvm = "svm"
modeldtree = "dec-tree"
modelnbayes = "n-bayes"
model = modelsvm


# Output filename based on config variables. Leave this at the end of config file
filename = model
if (k_fold) {
  filename = paste(filename, "k-fold", sep = "_")
}
if (feature_selection) {
  filename = paste(filename, "feat-sel", sep = "_")
}
filename = paste(filename, ".txt", sep="")
