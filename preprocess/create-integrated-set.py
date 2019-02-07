import json

def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

def findMappedIngredient(mappings, original):
   for mapping in mappings:
      if mapping["original"] == original:
         return mapping

def findRichIngredient(ingredients, original):
   for ingredient in ingredients:
      if ingredient["foodId"] == original:
         return ingredient

def main():
   mappings = readJSON("../dataset/ingredients-linkage.json")
   recipes = readJSON("../dataset/recipes-dataset.json")
   ingredients = readJSON("../dataset/ingredients-dataset.json")

   integrated_recipes = []
   i = 0
   for recipe in recipes:
      integrated_recipe = {}
      integrated_recipe["id"] = recipe["id"]
      integrated_recipe["cuisine"] = recipe["cuisine"]
      mapped_ingredients = list()
      for ingredient in recipe["ingredients"]:
         mapped_ingredient = findMappedIngredient(mappings, ingredient)
         mapped_ingredients.append(mapped_ingredient)

      # Compute aggregated values
      kcal, carbs, fats, proteins = 0, 0, 0, 0
      recipe_ingredients_number = 0
      true_ingredients = set()
      for mapped_ingredient in mapped_ingredients:
         if mapped_ingredient["mapped"] is None:
            true_ingredients.add(mapped_ingredient["original"])
         else:
            if mapped_ingredient["mapped"] in true_ingredients:
               continue

            recipe_ingredients_number += 1
            true_ingredients.add(mapped_ingredient["mapped"])
            rich_ingredient = findRichIngredient(ingredients, mapped_ingredient["mapped"])
            nutrients = rich_ingredient["nutrients"]
            if "ENERC_KCAL" in nutrients:
               kcal += nutrients["ENERC_KCAL"]
            if "FAT" in nutrients:
               fats += nutrients["FAT"]
            if "PROCNT" in nutrients:
               proteins += nutrients["PROCNT"]
            if "CHOCDF" in nutrients:
               carbs += nutrients["CHOCDF"]
      
      integrated_recipe["mapped_ingredients"] = list(true_ingredients)
      integrated_recipe["kcal"] = kcal / recipe_ingredients_number
      integrated_recipe["carbs"] = carbs / recipe_ingredients_number
      integrated_recipe["fats"] = fats / recipe_ingredients_number
      integrated_recipe["proteins"] = kcal / recipe_ingredients_number
      integrated_recipes.append(integrated_recipe)
      i += 1
      print(i)

   writeJSON(integrated_recipes, "../dataset/integrated-dataset.json")

# execute only if run as a script
if __name__ == "__main__":
   main()