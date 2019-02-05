import json

def mergeRecipesIngredients(recipes, ingredients):
   print(recipes[0])
   for recipe in recipes:
      kcal, carbs, fats, proteins = 0, 0, 0, 0
      missing_kcal, missing_carbs, missing_fats, missing_proteins = 0, 0, 0, 0
      mapped_ingredients = []
      for ingredient in recipe["ingredients"]:
         # Push name if ingredient is not mapped on the API (i.e. is boolean)
         if isinstance(ingredients[ingredient], bool) and not(ingredients[ingredient]):
            mapped_ingredients.append(ingredient)
         # Push id otherwise
         else:
            mapped_ingredients.append(ingredients[ingredient]["foodId"])
            nutrients = ingredients[ingredient]["nutrients"]
            if "ENERC_KCAL" in nutrients:
               kcal += nutrients["ENERC_KCAL"]
            else:
               missing_kcal += 1
            if "FAT" in nutrients:
               fats += nutrients["FAT"]
            else:
               missing_fats += 1
            if "PROCNT" in nutrients:
               proteins += nutrients["PROCNT"]
            else:
               missing_proteins += 1
            if "CHOCDF" in nutrients:
               carbs += nutrients["CHOCDF"]
            else:
               missing_carbs += 1
      recipe["kcal"], recipe["carbs"], recipe["fats"], recipe["proteins"] = kcal, carbs, fats, proteins
      recipe["missing_kcal"], recipe["missing_carbs"], recipe["missing_fats"], recipe["missing_proteins"] = missing_kcal, missing_carbs, missing_fats, missing_proteins
      recipe["mapped_ingredients"] = mapped_ingredients
   print(recipes[0])
   return recipes


def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

def main():
   recipes = readJSON("../dataset/recipes-dataset.json")
   ingredients = readJSON("../dataset/ingredients-dataset.json")
   new_recipes = mergeRecipesIngredients(recipes, ingredients)
   writeJSON(new_recipes, "../dataset/merged-dataset.json")

if __name__ == "__main__":
   # execute only if run as a script
   main()