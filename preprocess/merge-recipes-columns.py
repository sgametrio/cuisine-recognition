import json

def mergeRecipesColumns(recipes, enriched):
   for i in range(len(recipes)):
      recipes[i]["carbs"] = enriched[i]["carbs"]
      recipes[i]["fats"] = enriched[i]["fats"]
      recipes[i]["proteins"] = enriched[i]["proteins"]
      recipes[i]["kcal"] = enriched[i]["kcal"]
   return recipes


def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

def main():
   recipes = readJSON("../dataset/regex-cleaned-dataset.json")
   enriched = readJSON("../dataset/merged-dataset.json")
   new_recipes = mergeRecipesColumns(recipes, enriched)
   writeJSON(new_recipes, "../dataset/regex-cleaned-merged-dataset.json")

if __name__ == "__main__":
   # execute only if run as a script
   main()