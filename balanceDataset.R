library(jsonlite)
recipes = fromJSON("dataset/recipes-dataset.json")
cuisines = sort(Reduce(union, recipes$cuisine))
ingrs_per_recipe = vector("list", 20)
avg_ingrs_per_recipe = vector("list", 20)
sd_ingrs_per_recipe = vector("list", 20)
names(ingrs_per_recipe) = names(avg_ingrs_per_recipe) = names(sd_ingrs_per_recipe) = cuisines
for (cuisine in cuisines) {
  print(cuisine)
  # Get all recipes ingredients per cuisine
  ingrs = recipes$ingredients[recipes$cuisine == cuisine]
  # Take only unique element
  ingrs_per_recipe[[cuisine]] = Reduce(union, unlist(ingrs))
  # Compute how many ingredients there are, on average, in a recipe for a particular cuisine
  avg_ingrs_per_recipe[[cuisine]] = round(mean(sapply(ingrs, length)))
  # Compute the standard deviation
  sd_ingrs_per_recipe[[cuisine]] = round(sd(sapply(ingrs, length)))
}
# We now want to balance our dataset, so we create random recipes in this way:
# 1) We compute the frequencies of all recipes, and if the max number of random recipes to create is greater
#    than the frequency of a particular cuisine, than we create only max_recipes-freq[[cuisine]] random recipes
#    and we concatenate them to the others
# 2) Otherwise we create max_recipes new random recipes and we do that assuring that all unique ingredients would be used
# 3) The number of ingredients per recipes is choosen randomly between avg[[cuisine]]-sd[[cuisine]] and avg[[cuisine]]+sd[[cuisine]]
freq = table(recipes$cuisine)
# max_recipes has to be at least 489 recipes in order to be sure to take randomly all the unique 6714 ingredients
# 489 is computed from: 
# 1) Take the cuisine that has more ingredients than the others --> italian, with 2929 unique ingredients
# 2) At the worst case, since the average length of italian recipes is 10 and it's standard deviation is 4, we suppose
#    our algorithm takes 10-4=6 ingredients for every new random recipes
# 3) So to take all the unique italian ingredients at least one time we need at least 2929/6=488.17 recipes
max_recipes = 489
not_to_add = c()
new_random_recipes = vector("list", 2)
names(new_random_recipes) = c("cuisine", "ingredients")
for (cuisine in cuisines) {
  print(cuisine)
  min = avg_ingrs_per_recipe[[cuisine]]-sd_ingrs_per_recipe[[cuisine]]
  min = ifelse(min > 0, min, 0)
  max = avg_ingrs_per_recipe[[cuisine]]+sd_ingrs_per_recipe[[cuisine]]
  if (max_recipes-freq[cuisine] > 0) {
    for (i in seq_len(max_recipes-freq[cuisine])) {
      how_many_ingrs = sample(min:max, 1)
      new_random_recipes$cuisine = append(new_random_recipes$cuisine, cuisine)
      new_random_recipes$ingredients = append(new_random_recipes$ingredients, list(sample(ingrs_per_recipe[[cuisine]], how_many_ingrs)))
    } 
  } else {
    saved = ingrs_per_recipe[[cuisine]]
    # We save the cuisine that we must not integrate with the already presents recipes, because we create them all now
    not_to_add = append(not_to_add, cuisine)
    for (i in seq_len(max_recipes)) {
      how_many_ingrs = sample(min:max, 1)
      if (how_many_ingrs > length(saved)) {
        # Add last ingredients
        ingrs_to_add = saved
        # Take back the original ones
        saved = ingrs_per_recipe[[cuisine]]
        # Remove from them the saved ones
        saved = saved[!saved %in% ingrs_to_add]
        # How many random ingredients we have to pick?
        remains = sample(saved, how_many_ingrs-length(ingrs_to_add))
        # Final list of ingredients for this recipe
        ingrs_to_add = append(ingrs_to_add, remains)
        # Remove remains ingredients from the total
        saved = saved[!saved %in% remains]
      } else {
        ingrs_to_add = sample(saved, how_many_ingrs)
        saved = saved[!saved %in% ingrs_to_add]
      }
      new_random_recipes$cuisine = append(new_random_recipes$cuisine, cuisine)
      new_random_recipes$ingredients = append(new_random_recipes$ingredients, list(ingrs_to_add))
    }
    # new_random_recipes$cuisine = append(new_random_recipes$cuisine, rep(cuisine, max_recipes))
    # new_random_recipes$ingredients = append(new_random_recipes$ingredients, recipes$ingredients[sample(which(recipes$cuisine == cuisine), max_recipes)])
  }
}
index_not_to_add = which(!recipes$cuisine %in% not_to_add)
new_random_recipes$cuisine = append(new_random_recipes$cuisine, recipes$cuisine[index_not_to_add])
new_random_recipes$ingredients = append(new_random_recipes$ingredients, recipes$ingredients[index_not_to_add])
recipes = new_random_recipes
remove(ingrs, avg_ingrs_per_recipe, sd_ingrs_per_recipe, ingrs_per_recipe, cuisines, cuisine, freq, max_recipes, index_not_to_add, new_random_recipes)
gc()
