import json

def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

def computeIngredientsCompleteness(ingredients):
   completeness = {
      "tuples": { "%": 0, "n_tot": 0, "n_null": 0 },
      "kcal": { "%": 0, "n_tot": 0, "n_null": 0 },
      "proteins": { "%": 0, "n_tot": 0, "n_null": 0 },
      "carbs": { "%": 0, "n_tot": 0, "n_null": 0 },
      "fats": { "%": 0, "n_tot": 0, "n_null": 0 },
      "fibers": { "%": 0, "n_tot": 0, "n_null": 0 },
      "foodContentsLabel": { "%": 0, "n_tot": 0, "n_null": 0 },
      "category": { "%": 0, "n_tot": 0, "n_null": 0 },
      "categoryLabel": { "%": 0, "n_tot": 0, "n_null": 0 }
   }
   for ingredient in ingredients:
      tuple_with_null = False
      if not("ENERC_KCAL" in ingredient["nutrients"]):
         completeness["kcal"]["n_null"] += 1
         tuple_with_null = True
      if not("FAT" in ingredient["nutrients"]):
         completeness["fats"]["n_null"] += 1
         tuple_with_null = True
      if not("PROCNT" in ingredient["nutrients"]):
         completeness["proteins"]["n_null"] += 1
         tuple_with_null = True
      if not("CHOCDF" in ingredient["nutrients"]):
         completeness["carbs"]["n_null"] += 1
         tuple_with_null = True
      if not("FIBTG" in ingredient["nutrients"]):
         completeness["fibers"]["n_null"] += 1
         tuple_with_null = True
      if not("foodContentsLabel" in ingredient):
         completeness["foodContentsLabel"]["n_null"] += 1
         tuple_with_null = True
      if not("category" in ingredient):
         completeness["category"]["n_null"] += 1
         tuple_with_null = True
      if not("categoryLabel" in ingredient):
         completeness["categoryLabel"]["n_null"] += 1
         tuple_with_null = True
      if tuple_with_null:
         completeness["tuples"]["n_null"] += 1

      for attr in completeness:
         completeness[attr]["n_tot"] += 1

   for attr in completeness:
      completeness[attr]["%"] = 100 - (completeness[attr]["n_null"] / completeness[attr]["n_tot"] * 100)

   return completeness 

def main():
   ingredients = readJSON("../dataset/ingredients-dataset.json")
   completeness = computeIngredientsCompleteness(ingredients)
   writeJSON(completeness, "../dataset/completeness-stats.json")

# execute only if run as a script
if __name__ == "__main__":
   main()

