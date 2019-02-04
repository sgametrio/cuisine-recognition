import requests
import json
from ratelimit import limits, sleep_and_retry

ONE_MINUTE = 60

#@sleep_and_retry
#@limits(calls = 20, period = ONE_MINUTE)
def getIngredientsInfo(ingredient):
   params = {
      "nutrition-type": "logging",
      "ingr": ingredient,
      "app_id": "72a9739f",
      "app_key": "82fe3d90684ec3aecc4b29846cf445d9"
   }
   headers = {
      "Accept": "application/json",
      "Content-type": "application/json"
   }
   response = requests.get("https://api.edamam.com/api/food-database/parser", params = params, headers = headers, timeout = 15)
   return response.json()

def readRecipeDataset(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeEnrichedRecipes(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

def enrichRecipes(recipes):
   ingredients = {}
   count_incomplete_recipes = 0
   i = 0
   for recipe in recipes:
      not_found = False
      kcal, fat, proteins, carbs = 0, 0, 0, 0
      recipe["mapped_ingredients"] = []
      for ingredient in recipe["ingredients"]:
         new_ingredient = { "old": ingredient }
         exception_raised = False
         try:
            if not(ingredient in ingredients):
               response = getIngredientsInfo(ingredient)
               attribute = "parsed"
               if len(response[attribute]) == 0:
                  attribute = "hints"
               if len(response[attribute]) == 0:
                  not_found = True
               if not_found:
                  count_incomplete_recipes += 1
                  break
               else:
                  ingredients[ingredient] = response[attribute][0]["food"]
            
            new_ingredient["id"] = ingredients[ingredient]["foodId"]
            nutrients = ingredients[ingredient]["nutrients"]
            if "ENERC_KCAL" in nutrients:
               kcal += nutrients["ENERC_KCAL"]

            if "FAT" in nutrients:
               fat += nutrients["FAT"]
            
            if "PROCNT" in nutrients:
               proteins += nutrients["PROCNT"]
            
            if "CHOCDF" in nutrients:
               carbs += nutrients["CHOCDF"]

            recipe["mapped_ingredients"].append(new_ingredient)
         except Exception as e:
            print("Exception raised at recipe with id", str(recipe["id"]))
            exception_raised = True
            print(e)
         

      # Mark recipe as incomplete if at least one ingredient has not a correspective
      recipe["missing_ingredients"] = not_found
      recipe["exception_raised"] = exception_raised
      # enrich recipe with custom attributes
      recipe["kcal"] = kcal
      recipe["fat"] = fat
      recipe["proteins"] = proteins
      recipe["carbs"] = carbs
      i += 1
      print("Recipe #" + str(i) + " completed", sep="")
   return recipes

def mergeRecipes(recipes, enriched):
   for recipe in enriched:
      recipes = mergeRecipeWithSameId(recipes, recipe)
   return recipes

def mergeRecipeWithSameId(recipes, to_merge):
   for recipe in recipes:
      if recipe["id"] == to_merge["id"]:
         recipe = to_merge
         return recipes

def cleanRecipes(recipes):
   cleaned = []
   for recipe in recipes:
      clean = {
         "id": recipe["id"],
         "kcal": recipe["kcal"],
         "proteins": recipe["proteins"],
         "fat": recipe["fat"],
         "carbs": recipe["carbs"],
         "ingredients": recipe["ingredients"],
         "cuisine": recipe["cuisine"],
         "no_of_not_found": recipe["missing_ingredients"]
      }
      clean["ingredients_ids"] = []
      for ingredient in recipe["mapped_ingredients"]:
         clean["ingredients_ids"].append(str(ingredient["id"]))
      # Remove duplicates
      clean["ingredients_ids"] = list(set(clean["ingredients_ids"]))
      cleaned.append(clean)
   
   return cleaned

def main():
   recipes = readRecipeDataset("../dataset/original-dataset.json")
   recipes = enrichRecipes(recipes)
   # Retry recipes with exceptions (maximum 3 times to avoid infinite loop)
   exceptions = [recipe for recipe in recipes if recipe["exception_raised"]]
   i = 0
   while len(exceptions) > 0 and i < 3:
      retry = enrichRecipes(exceptions)
      recipes = mergeRecipes(recipes, retry)
      exceptions = [recipe for recipe in recipes if recipe["exception_raised"]]
      i += 1
   writeEnrichedRecipes(recipes, "../dataset/enriched-dataset.json")

   recipes = cleanRecipes(recipes)
   writeEnrichedRecipes(recipes, "../dataset/cleaned-dataset.json")

   # What about missing ingredients: maybe preprocessing before calling the API?

if __name__ == "__main__":
   # execute only if run as a script
   main()