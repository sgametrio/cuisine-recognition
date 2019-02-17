# Compute combined distance
require("stringr")
require("stringdist")
strs = c()
str = readline("Inserire una stringa (per terminare inserire la stringa vuota): ")
while (str != "") {
  strs = append(strs, str)
  str = readline("Inserire una stringa (per terminare inserire la stringa vuota): ")
}
max_dist = as.integer(readline("Inserire la distanza di edit massima secondo cui due stringhe sono uguali: "))
strs = c("chicken consomme",
         "low sodium chicken",
         "cut up chicken",
         "minced chicken",
         "roast breast of chicken",
         "fryers",
         "bone in chicken thighs",
         "sliced chicken",
         "chicken pan drippings",
         "bone-in chicken",
         "chicken pieces",
         "chicken schmaltz",
         "broiler chicken",
         "diced chicken",
         "boneless skin on chicken thighs",
         "2 1/2 to 3 lb. chicken, cut into serving pieces",
         "Knorr Chicken Flavor Bouillon",
         "knorr chicken flavor bouillon cube",
         "boneless chicken skinless thigh",
         "skinless chicken fillets",
         "black chicken",
         "free-range chickens",
         "bone in skinless chicken thigh",
         "boneless skinless chicken",
         "boneless chicken",
         "chicken in water",
         "broiler",
         "fryer chickens",
         "broiler-fryer chicken",
         "broiler-fryers",
         "organic chicken",
         "bone in skin on chicken thigh",
         "knorr chicken flavor bouillon",
         "chicken fillets",
         "chicken",
         "skinless chicken pieces",
         "chicken-flavored soup powder",
         "whole chicken",
         "chicken and rice soup")
# In this simple version we only apply the tolower transformation
# We might want to remove punctuation, numbers, strange symbols and so on
strs = tolower(strs)
distances = matrix(nrow = length(strs), ncol = length(strs))
diag(distances) = 0
colnames(distances) = rownames(distances) = strs
# We compute a triangular distance matrix
for (i in seq_len(length(strs)-1)) {
  for (j in (i+1):length(strs)) {
    # Create a set over the splitted tokens applying an union on them
    tks1 = Reduce(union, unlist(str_split(string = strs[i], pattern = " ")))
    tks2 = Reduce(union, unlist(str_split(string = strs[j], pattern = " ")))
    # Get the real intersection
    intersection = intersect(tks1, tks2)
    # Get the differences sets
    diff1 = setdiff(tks1, tks2)
    diff2 = setdiff(tks2, tks1)
    # Compute the edit distance between every couple of diff sets
    # If the computed dist is lower than max_dist, then they are the same token
    # Add it to the intersection
    for (d1 in seq_len(length(diff1))) {
      for (d2 in seq_len(length(diff2))) {
        if (stringdist(diff1[d1], diff2[d2], method = "lv") <= max_dist) {
          intersection = append(intersection, diff1[d1])
        }
      }
    }
    # Compute combined Jaccard distance
    distances[i, j] = 1 - (length(intersection) / (length(tks1) + length(tks2) - length(intersection)))
  }
}
View(distances)
# Pick element with the least average distance from all others elements
selected_elem = 0
min_avg = Inf
sum = 0
for (i in seq_len(length(strs))) {
  sum = 0
  for (j in seq_len(length(strs))) {
    # Since we've first built a tringular matrix, we've to take the lowerside matrix element in the upperside
    if (j < i) {
      sum = sum + distances[j, i]
    } else if (i != j) {
      sum = sum + distances[i, j]
    }
  }
  avg = sum / (length(strs)-1)
  if (avg < min_avg) {
    min_avg = avg
    selected_elem = i
  }
  print(paste("Element", i, ":", strs[i], "; AVG: ", avg))
}
print(paste("Selected element: ", strs[selected_elem], "; AVG: ", min_avg))