const fs = require("fs")
let dataset = JSON.parse(fs.readFileSync(__dirname + "/dataset/reduced-train.json"))

// Output as csv dataset in the following form (sparse matrix):
// recipe_id(optional);cuisine(target);ingredient_1;ingredient_2;....;ingredient_n;
// 12345;italian;1;0;1;0;....;0;
let cuisines_count = {}
let ingredients = []
let recipes = []
for (let recipe of dataset) {
   let new_recipe = {}

   new_recipe.cuisine = recipe.cuisine
   if (!(recipe.cuisine in cuisines_count)) {
      cuisines_count[recipe.cuisine] = 1
   } else {
      // use max 2k recipes for each cuisine
      if (cuisines_count[recipe.cuisine] >= 2000) {
         continue
      }
      cuisines_count[recipe.cuisine] += 1
   }

   let recipe_ingredients = []
   for (let ingredient of recipe.ingredients) {
      if (!ingredients.includes(ingredient)) {
         ingredients.push(ingredient)
      }
      recipe_ingredients.push(ingredient)
   }
   
   new_recipe.ingredients = recipe_ingredients
   recipes.push(new_recipe)
}

let csv_output = ""
let stream = fs.createWriteStream(__dirname + "/dataset/max2k_recipes.csv", { flags: "w" })

// write header line
csv_output += "cuisine"
for (let ingredient of ingredients) {
   csv_output += ";" + ingredient
}
csv_output += '\n'
stream.write(csv_output)

// write entries
for (let recipe of recipes) {
   csv_output = ""
   csv_output += recipe.cuisine
   for (let ingredient of ingredients) {
      csv_output += (recipe.ingredients.includes(ingredient)) ? ";1" : ";0"
   }
   csv_output += '\n'
   stream.write(csv_output)
}
stream.end()
