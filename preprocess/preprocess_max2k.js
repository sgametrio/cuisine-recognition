const fs = require("fs")
let dataset = JSON.parse(fs.readFileSync(__dirname + "/../dataset/regex-spacy-cleaned-dataset.json"))

// Output as csv dataset in the following form (sparse matrix):
// recipe_id(optional);cuisine(target);ingredient_1;ingredient_2;....;ingredient_n;
// 12345;italian;1;0;1;0;....;0;

/*** Preprocessing steps
 * - Transform recipes JSON to a CSV matrix (recipe x ingredient)
 * - Max MAX_RECIPES recipes for cuisine (to remove most of bias)
 * - Max 5 classes to predict (to better handle multiclass analysis problems)
 */

const MAX_RECIPES = 2000
const MAX_CUISINES = 5

let cuisines_count = {}
let ingredients = new Map() // ingredient => { quantity }
let recipes = []
for (let recipe of dataset) {
   let new_recipe = {}

   new_recipe.cuisine = recipe.cuisine
   if (!(recipe.cuisine in cuisines_count)) {
      if (cuisines_count.length >= MAX_CUISINES) {
        continue
      }
      cuisines_count[recipe.cuisine] = 1
   } else {
      // use max 2k recipes for each cuisine
      if (cuisines_count[recipe.cuisine] >= MAX_RECIPES) {
         continue
      }
      cuisines_count[recipe.cuisine] += 1
   }

   for (let ingredient of recipe.ingredients) {
      // count ingredients in recipes
      if(!ingredients.has(ingredient)) {
         ingredients.set(ingredient, { quantity: 1 })
      } else {
         quantity = ingredients.get(ingredient).quantity
         ingredients.set(ingredient, { quantity: quantity + 1 })
      }
   }
   
   new_recipe.ingredients = recipe.ingredients
   recipes.push(new_recipe)
}

let csv_output = ""
let stream = fs.createWriteStream(__dirname + "/../dataset/max_5_cuisines_2k_recipes.csv", { flags: "w" })

// write header line
csv_output += "cuisine"
for (let ingredient of ingredients.keys()) {
   csv_output += ";" + ingredient
}
csv_output += '\n'
stream.write(csv_output)

// write entries
for (let recipe of recipes) {
   csv_output = ""
   csv_output += recipe.cuisine
   for (let ingredient of ingredients.keys()) {
      csv_output += (recipe.ingredients.includes(ingredient)) ? ";1" : ";0"
   }
   csv_output += '\n'
   stream.write(csv_output)
}
stream.end()
