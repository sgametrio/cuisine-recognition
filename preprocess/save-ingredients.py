from functools import reduce
import json
from ratelimit import limits, sleep_and_retry
import timeout_decorator
import requests

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

def mergeIngredients(recipe1, recipe2):
   merged = set(recipe1)
   for element in recipe2:
      merged.add(element)
   return list(merged)

def ingredientsList(recipes):
   ingredients = []
   for recipe in recipes:
      ingredients.append(recipe["ingredients"])
   print("Recipes #", len(ingredients), sep="")
   return reduce(mergeIngredients, ingredients)
   
def getIngredientsNutrients(ingredients):
   enriched = {}
   kcal, fat, proteins, carbs = 0, 0, 0, 0
   i = 0
   for ingredient in ingredients:
      not_found = False
      try:
         if not(ingredient in enriched):
            response = getIngredientsInfo(ingredient)
            attribute = "parsed"
            if len(response[attribute]) == 0:
               attribute = "hints"
            if len(response[attribute]) == 0:
               not_found = True
            if not_found:
               enriched[ingredient] = False
            else:
               enriched[ingredient] = response
      except Exception as e:
         print("Exception raised at ingredient ", ingredient)
         enriched[ingredient] = False
         print(e)
      i += 1
      print("Ingredient #" + str(i) + " completed of " + str(len(ingredients)), sep="")
   return enriched

def retryFalseIngredients(ingredients):
   false_ingredients = []
   for ingredient in ingredients.keys():
      if not(ingredients[ingredient]):
         false_ingredients.append(ingredient)

   false_ingredients = getIngredientsNutrients(false_ingredients)
   for ingredient in false_ingredients.keys():
      ingredients[ingredient] = false_ingredients[ingredient]
   return ingredients


def main():
   recipes = readJSON("../dataset/original-dataset.json")
   ingredients = ingredientsList(recipes)
   ingredients = getIngredientsNutrients(ingredients)
   ingredients = retryFalseIngredients(ingredients)
   writeJSON(ingredients, "../dataset/ingredients-dataset.json")

if __name__ == "__main__":
   # execute only if run as a script
   main()