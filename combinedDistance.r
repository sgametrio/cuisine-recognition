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
# strs = c("AT&T", "ATT Corporation", "AT&T Corporation", "IBM Corporation")
# In this simple version we only apply the tolower transformation
# We might want to remove punctuation, numbers, strange symbols and so on
strs = tolower(strs)
distances = matrix(nrow = length(strs), ncol = length(strs))
diag(distances) = 0
colnames(distances) = rownames(distances) = strs
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
    # If the computed dist is lower from max_dist, then they are the same token
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