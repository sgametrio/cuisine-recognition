jaccard = fromJSON("dataset/jaccard-similarity.json")
recipes = fromJSON("dataset/integrated-dataset.json")
linkage = fromJSON("dataset/ingredients-linkage.json")
mapped_unlist = unlist(recipes$mapped_ingredients)
mappings = sapply(mapped_unlist, function(x) { linkage$original[which(x == linkage$mapped)] })
for (i in seq_along(mappings)) {
  #print(mappings[[i]])
  if (length(mappings[[i]]) > 0) {
    cols = which(colnames(jaccard) %in% mappings[[i]])
    maxCol = 0
    selectedCol = 0
    for (col in cols) {
      tmp_max = max(jaccard[, col], na.rm = TRUE)
      if (tmp_max > maxCol) {
        maxCol = tmp_max
        selectedCol = col
      }
      if (maxCol == 1) {
        break
      }
    }
    mapped_unlist[i] = colnames(jaccard)[selectedCol]
  }
}
recipes$mapped_ingredients = relist(mapped_unlist, recipes$mapped_ingredients)
write_json(recipes, "dataset/integrated-jaccard-dataset.json")
