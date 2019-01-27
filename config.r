# Do not comment variables, instead change their values
k_fold = TRUE
feature_selection = TRUE




# Output filename based on config variables. Leave this at the end of config file
filename = "stats"
if (k_fold) {
  filename = paste(filename, "k-fold", sep = "_")
}
if (feature_selection) {
  filename = paste(filename, "feat-sel", sep = "_")
}
filename = paste(filename, ".txt", sep = "")
