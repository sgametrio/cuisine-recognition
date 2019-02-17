# Compute combined distance
# Our definition of combined distance is as follows:
# Given two strings s1 and s2, then
# combinedDist(s1, s2) = 1 - (|combinedIntersection(s1, s2)| / |combinedUnion(s1, s2)|), where
# combinedIntersection(s1, s2) = Intersection(s1, s2) U {s | editDistance(s, s1) <= max_dist, }
require("stringr")
require("stringdist")
# strs = c()
# str = readline("Inserire una stringa (per terminare inserire la stringa vuota): ")
# while (str != "") {
#   strs = append(strs, str)
#   str = readline("Inserire una stringa (per terminare inserire la stringa vuota): ")
# }
max_dist = as.integer(readline("Inserire la distanza di edit massima secondo cui due stringhe sono uguali: "))
strs = c("AT&T Corporation", "AT&T", "ATT Corporation", "IBM Corporation")
# In this simple version we only apply the tolower transformation
# We might want to remove punctuation, numbers, strange symbols and so on
strs = tolower(strs)
# Compute the max string length to normalize the edit distance
max_str_length = max(sapply(strs, str_length))
combined_distances = edit_distances = jaccard_distances = matrix(nrow = length(strs), ncol = length(strs))
diag(edit_distances) = diag(jaccard_distances) = diag(combined_distances) = 0
colnames(combined_distances) = colnames(jaccard_distances) = colnames(edit_distances) = strs
rownames(combined_distances) = rownames(jaccard_distances) = rownames(edit_distances) = strs
# We compute a triangular distance matrix
for (i in seq_len(length(strs)-1)) {
  for (j in (i+1):length(strs)) {
    # Create a set over the splitted string tokens, applying an union on them
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
    # Edit distance
    edit_distances[i, j] = stringdist(strs[i], strs[j], method = "lv")
    # Jaccard distance
    # jaccard_distances[i, j] = stringdist(strs[i], strs[j], method = "jaccard")
    jaccard_distances[i, j] = 1 - (length(intersect(tks1, tks2)) / length(union(tks1, tks2)))
    # Combined distance
    combined_distances[i, j] = 1 - (length(intersection) / (length(tks1) + length(tks2) - length(intersection)))
  }
}
edit_distances = edit_distances / max_str_length
View(edit_distances)
View(jaccard_distances)
View(combined_distances)
# Pick the element with the least average distance from all others elements, for three distance measure used
selected_elem_edit = selected_elem_jaccard = selected_elem_combined = 0
min_avg_edit = min_avg_jaccard = min_avg_combined = Inf
sum_edit = sum_jaccard = sum_combined = 0
for (i in seq_len(length(strs))) {
  sum_edit = sum_jaccard = sum_combined = 0
  for (j in seq_len(length(strs))) {
    # Since we've first built a tringular matrix, we've to take the lowerside matrix element in the upperside
    if (j < i) {
      sum_edit = sum_edit + edit_distances[j, i]
      sum_jaccard = sum_jaccard + jaccard_distances[j, i]
      sum_combined = sum_combined + combined_distances[j, i]
    } else if (i != j) {
      sum_edit = sum_edit + edit_distances[i, j]
      sum_jaccard = sum_jaccard + jaccard_distances[i, j]
      sum_combined = sum_combined + combined_distances[i, j]
    }
  }
  avg_edit = sum_edit / (length(strs)-1)
  avg_jaccard = sum_jaccard / (length(strs)-1)
  avg_combined = sum_combined / (length(strs)-1)
  if (avg_edit < min_avg_edit) {
    min_avg_edit = avg_edit
    selected_elem_edit = i
  }
  if (avg_jaccard < min_avg_jaccard) {
    min_avg_jaccard = avg_jaccard
    selected_elem_jaccard = i
  }
  if (avg_combined < min_avg_combined) {
    min_avg_combined = avg_combined
    selected_elem_combined = i
  }
}
print(paste("Selected element using normalized edit distance: ", strs[selected_elem_edit], "; AVG: ", min_avg_edit))
print(paste("Selected element using jaccard distance: ", strs[selected_elem_jaccard], "; AVG: ", min_avg_jaccard))
print(paste("Selected element using combined distance: ", strs[selected_elem_combined], "; AVG: ", min_avg_combined))
