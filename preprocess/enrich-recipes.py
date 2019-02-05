import requests
import json
from ratelimit import limits, sleep_and_retry
import timeout_decorator

ONE_MINUTE = 60

@sleep_and_retry
@limits(calls = 20, period = ONE_MINUTE)
@timeout_decorator.timeout(12, exception_message="Timeout of 12 seconds. Continue...")
def getIngredientsInfo(ingredient):
   params = {
      "nutrition-type": "logging",
      "ingr": ingredient,
      "app_id": "72a9739f",
      "app_key": "dd10989037b18ac8fdf2cb825c672ce9"
   }
   headers = {
      "Accept": "application/json",
      "Content-type": "application/json"
   }
   response = requests.get("https://api.edamam.com/api/food-database/parser", params = params, headers = headers, timeout = 10)
   return response.json()

def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
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
   recipes = readJSON("../dataset/original-dataset.json")
   recipes = enrichRecipes(recipes)
   # Retry recipes with exceptions (maximum 3 times to avoid infinite loop)
   exceptions = [recipe for recipe in recipes if recipe["exception_raised"]]
   i = 0
   while len(exceptions) > 0 and i < 3:
      retry = enrichRecipes(exceptions)
      recipes = mergeRecipes(recipes, retry)
      exceptions = [recipe for recipe in recipes if recipe["exception_raised"]]
      i += 1
   writeJSON(recipes, "../dataset/enriched-dataset.json")

   recipes = cleanRecipes(recipes)
   writeJSON(recipes, "../dataset/cleaned-dataset.json")

   # What about missing ingredients: maybe preprocessing before calling the API?

if __name__ == "__main__":
   # execute only if run as a script
   main()