library(jsonlite)
# recipes = fromJSON("dataset/integrated-jaccard-dataset.json")
cuisines = sort(Reduce(union, recipes$cuisine))
ingrs_per_recipe = vector("list", 20)
avg_ingrs_per_recipe = vector("list", 20)
sd_ingrs_per_recipe = vector("list", 20)
names(ingrs_per_recipe) = names(avg_ingrs_per_recipe) = names(sd_ingrs_per_recipe) = cuisines
for (cuisine in cuisines) {
  print(cuisine)
  ingrs = recipes$ingredients[recipes$cuisine == cuisine]
  ingrs_per_recipe[[cuisine]] = Reduce(union, unlist(ingrs))
  avg_ingrs_per_recipe[[cuisine]] = round(mean(sapply(ingrs, length)))
  sd_ingrs_per_recipe[[cuisine]] = round(sd(sapply(ingrs, length)))
}
freq = table(recipes$cuisine)
max_recipes = 5000
not_to_add = c()
new_random_recipes = vector("list", 2)
names(new_random_recipes) = c("cuisine", "ingredients")
for (cuisine in cuisines) {
  print(cuisine)
  if (max_recipes-freq[cuisine] > 0) {
    for (i in seq_len(max_recipes-freq[cuisine])) {
      min = avg_ingrs_per_recipe[[cuisine]]-sd_ingrs_per_recipe[[cuisine]]
      min = ifelse(min > 0, min, 0)
      max = avg_ingrs_per_recipe[[cuisine]]+sd_ingrs_per_recipe[[cuisine]]
      how_many_ingrs = sample(min:max, 1)
      new_random_recipes$cuisine = append(new_random_recipes$cuisine, cuisine)
      new_random_recipes$ingredients = append(new_random_recipes$ingredients, list(sample(ingrs_per_recipe[[cuisine]], how_many_ingrs)))
    } 
  } else {
    not_to_add = append(cuisine, not_to_add)
    new_random_recipes$cuisine = append(new_random_recipes$cuisine, rep(cuisine, max_recipes))
    new_random_recipes$ingredients = append(new_random_recipes$ingredients, recipes$ingredients[sample(which(recipes$cuisine == cuisine), max_recipes)])
  }
}
index_not_to_add = which(!recipes$cuisine %in% not_to_add)
new_random_recipes$cuisine = append(new_random_recipes$cuisine, recipes$cuisine[index_not_to_add])
new_random_recipes$ingredients = append(new_random_recipes$ingredients, recipes$ingredients[index_not_to_add])
recipes = new_random_recipes
remove(ingrs, avg_ingrs_per_recipe, sd_ingrs_per_recipe, ingrs_per_recipe, cuisines, cuisine, freq, max_recipes, index_not_to_add, new_random_recipes)
gc()