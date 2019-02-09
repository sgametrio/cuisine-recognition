# Get 70 unmatched ingredients stats
source("70_unmatched.r")
# Extract functions to helpers.r
source("helpers.r")
# Read configuration variables
source("config.r")
# Packages manager
source("packages.r")
using("plyr", "stringr","jsonlite", "e1071", "pROC", "mlbench", "tictoc", "ROCR", "caret", "DataExplorer", "corrplot")

### Building training set from json
recipes = fromJSON("dataset/integrated-dataset.json")

dataset = fromCleanedRecipes(recipes)

# Continent column creation
asian = c("japanese", "vietnamese", "korean", "thai", "chinese", "filipino", "indian")
north_american = c("jamaican", "cajun_creole", "mexican")
south_american = c("southern_us", "brazilian")
mediterranean = c("greek", "italian", "moroccan", "spanish")
european = c("british", "french", "irish", "russian")
continent = dataset$cuisine
levels(continent)[levels(continent) %in% asian] = "asian"
levels(continent)[levels(continent) %in% north_american] = "north_american"
levels(continent)[levels(continent) %in% south_american] = "south_american"
levels(continent)[levels(continent) %in% mediterranean] = "mediterranean"
levels(continent)[levels(continent) %in% european] = "european"
if (target == "continent") {
  cuisine = dataset$cuisine
  dataset$cuisine = continent
}

### Plots on unscaled dataset
# Unscale data
unscaled = dataset
nutrients = c("kcal", "proteins", "fats", "carbs")
unscaled[, nutrients] = recipes[, nutrients]

palette = c("#fd4445", "#ffaf00", "#0091cf", "#01395e", "#05a58d")
plot(continent, col = palette, main = "Cuisine distribution by continent")
barplot(table(cuisine), las=2, cex.names=.9, main = "Cuisine distribution", ylim = c(0,8000))
sub = unscaled[, c("cuisine", "carbs", "fats", "kcal", "proteins")]
per_cuisine_recipe_nutrients = aggregate(. ~ cuisine, data = sub, mean)
dataset_without_nutrients = subset(dataset, select = -c(carbs, proteins, fats, kcal))
per_cuisine_recipe_ingredients = aggregate(. ~ cuisine, data = dataset_without_nutrients, sum)
plot_correlation(per_cuisine_recipe_nutrients[, nutrients]) # Too much correlation on AVERAGE nutrients per cuisine
plot_correlation(unscaled[, nutrients]) # Too much correlation on nutrients
plot_correlation(sub) # correlation between cuisines and nutrients

# Splitted unmatched ingredients
splitted_ingrs = unlist(Reduce(append, strsplit(ingrs, " ")))
# Plot correaltion between them
plot_correlation(dataset_without_nutrients[, colnames(dataset_without_nutrients) %in% splitted_ingrs])
# Plot correaltion between them and cuisines continent
plot_correlation(dataset_without_nutrients[, colnames(dataset_without_nutrients) %in% c("cuisine", splitted_ingrs)])

corrMatrix = cor(per_cuisine_recipe_nutrients[, nutrients])
corrplot(corrMatrix, type = "upper", tl.col = "black", tl.srt = 30,addCoef.col = "white")
barplot(per_cuisine_recipe_nutrients$kcal, names.arg = per_cuisine_recipe_nutrients$cuisine, col = palette, main="100g Recipe calories by cuisine")
barplot(per_cuisine_recipe_nutrients$proteins, names.arg = per_cuisine_recipe_nutrients$cuisine, col = palette, main="100g Recipe proteins by cuisine")
# TODO: analyze ingredients counts
barplot(per_cuisine_recipe_ingredients$tomatoes, names.arg = levels(continent), main = "Tomatoes distribution by cuisine", col = palette)
which(dataset_without_nutrients == max(dataset_without_nutrients, na.rm = TRUE), arr.ind = TRUE)
