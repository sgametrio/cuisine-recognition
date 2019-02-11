import json
import string
import operator
import editdistance

def readJSON(filename):
   with open(filename, "r") as f:
      return json.load(f)

def writeJSON(recipes, filename):
   with open(filename, "w") as outfile:
      json.dump(recipes, outfile)

"""
Extract terms from compound ingredient, lower them and remove punctuation
"""
def cleanString(ingredient):
   terms = ingredient.split()
   terms = [term.lower() for term in terms]
   table = str.maketrans('', '', string.punctuation)
   stripped = [t.translate(table) for t in terms]
   return stripped

"""
Perform Jaccard similarity on ingredient's term list to choose the "right ingredient representation"
Then compute Jaccard similarity between ingredients and the "right one"
"""
def buildJaccardSimilarityObject(ingredients):
   similarity = [[] for _ in ingredients]
   ing_terms = []
   for ingredient in ingredients:
      ing_terms.append(cleanString(ingredient))
   
   for i in range(len(ing_terms)):
      for j in range(len(ing_terms)):
         if i != j:
            similarity[i].append(computeJaccardSimilarityOnLists(ing_terms[i], ing_terms[j]))
   
   similarity_means = {}
   i = 0
   for ingredient in ingredients:
      if len(similarity[i]) > 0:
         similarity_means[ingredient] = sum(similarity[i]) / float(len(similarity[i]))
      i += 1
   
   # Choose the right ingredient as the maximum jaccard similarity mean
   right_ingredients = {}
   if len(similarity_means.keys()) == 0:
      # similarity on single element is 1.0
      right_ingredients[ingredients[0]] = 1.0
   else:
      key_max = max(similarity_means.items(), key=operator.itemgetter(1))[0]
      right_ingredients[key_max] = 1.0
      for key in similarity_means:
         if key != key_max:
            right_ingredients[key] = computeJaccardSimilarityOnLists(cleanString(key), cleanString(key_max)) 

   return right_ingredients

def computeJaccardSimilarityOnLists(list1, list2):
   l1 = set(list1)
   l2 = set(list2)
   intersection = l1.intersection(l2)
   # If edit distance is less than 2 (avoid s of plurals) then they are the same token
   diff1 = l1.difference(l2)
   diff2 = l2.difference(l1)
   for d1 in diff1:
      for d2 in diff2: 
         if editdistance.eval(d1, d2) < 2:
            intersection.add(d1)
   
   similarity = float(len(intersection) / (len(l1) + len(l2) - len(intersection)))
   return similarity

def main():
   ingredients_ref = readJSON("../dataset/ids-references.json")
   jaccard = []
   for id in ingredients_ref:
      jaccard.append(buildJaccardSimilarityObject(ingredients_ref[id]))
   accuracy = 0.0
   tot_ingr = 0
   for ingr in jaccard:
      for key in ingr:
         accuracy += ingr[key]
         tot_ingr += 1

   accuracy = accuracy / tot_ingr
   print("Average accuracy on dataset: ", str(accuracy))
   writeJSON(jaccard, "../dataset/jaccard-similarity.json")

if __name__ == "__main__":
   main()