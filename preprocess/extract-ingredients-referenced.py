import json

def createIdsReferences(mappings):
   ids = {}
   for mapping in mappings:
      id = mapping["mapped"]
      if not(id is None):
         if not(id in ids):
            ids[id] = []
         ids[id].append(mapping["original"])
   return ids


def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

def main():
   mappings = readJSON("../dataset/ingredients-linkage.json")
   ids = createIdsReferences(mappings)
   writeJSON(ids, "../dataset/ids-references.json")

if __name__ == "__main__":
   # execute only if run as a script
   main()