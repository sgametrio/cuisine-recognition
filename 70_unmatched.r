library(jsonlite)
expl_data = fromJSON("dataset/integrated-dataset.json")
not_start_food = lapply(expl_data$mapped_ingredients, function(x) startsWith(x, "food_"))
not_mapped = c()
ingrs = c()
for (i in seq_along(not_start_food)) {
  not_start_food_elems = which(not_start_food[i][[1]] == FALSE)
  if (length(not_start_food_elems) > 0) {
    for (j in seq_along(not_start_food_elems)) {
      not_mapped_label = expl_data$mapped_ingredients[i][[1]][not_start_food_elems[j]]
      if (!(not_mapped_label %in% ingrs)) {
        # print(not_mapped_label)
        ingrs = append(ingrs, not_mapped_label)
        not_mapped = append(not_mapped, i)
      }
    }
  }
}

### Plot distribution per continent
distr = as.data.frame(table(expl_data$cuisine[not_mapped]))
labels = distr$Var1
mp = barplot(distr$Freq, axes = FALSE, axisnames = FALSE, ylim=c(0,20), main = "Unmatched ingredients per recipes")
text(mp, par("usr")[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)

### Plot distribution per cuisine
asian = c("japanese", "vietnamese", "korean", "thai", "chinese", "filipino", "indian")
north_american = c("jamaican", "cajun_creole", "mexican")
south_american = c("southern_us", "brazilian")
mediterranean = c("greek", "italian", "moroccan", "spanish")
european = c("british", "french", "irish", "russian")
expl_data$cuisine[which(expl_data$cuisine %in% asian)] = "asian"
expl_data$cuisine[which(expl_data$cuisine %in% north_american)] = "north_american"
expl_data$cuisine[which(expl_data$cuisine %in% south_american)] = "south_american"
expl_data$cuisine[which(expl_data$cuisine %in% mediterranean)] = "mediterranean"
expl_data$cuisine[which(expl_data$cuisine %in% european)] = "european"
distr = as.data.frame(table(expl_data$cuisine[not_mapped]))
labels = distr$Var1
mp = barplot(distr$Freq, axes = FALSE, axisnames = FALSE, ylim=c(0,40), main = "Unmatched ingredients per continent")
text(mp, par("usr")[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)

