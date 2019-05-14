### HW2_Key, Machine Learning 2018
### Code Adopted from Frances Zlotnik
import os, re, nltk, urllib2, csv

##make nested dictionary of press releases and metadata
wd = "/HW2/GrimmerSenatePressReleases-master/raw/"


os.chdir(wd)

folders = [wd + "Sessions", wd + "Shelby"]

pressReleases = {}

for folder in folders:
    files = os.listdir(folder)
    for pr in files:
        pressReleases[pr] = {}
        pressReleases[pr]["day"] = pr[:2]
        pressReleases[pr]["month"] = pr[2:5]
        pressReleases[pr]["year"] = pr[5:9]
        pressReleases[pr]["author"] = re.sub("[0-9]+.txt", "", pr[9:])
        with open(folder + "/" + pr, "r") as fileIn:
            pressReleases[pr]["text"] = fileIn.read()

#get stopwords, store as set for quick lookup. apply porter stemmer
porter = nltk.stem.PorterStemmer()

stop_words = urlopen('http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop').read()
stop_words = stop_words.decode('utf-8')
stop_words = stop_words.split("\n")
stopwords = {x.strip("\n") for x in stopwords}
toAdd = {"shelby", "sessions", "richard", "jeff", "email", "press", "room", "member", "senate"}
stopwords.update(toAdd) 
stopwords = {porter.stem(w) for w in stopwords}




#to count unigrams and trigrams, create dictionaries linking the -grams to their counts.
#For unigram and each trigram in each doc, check whether it already exists in the dictionary.
#If not, add it and give it a value of 1. If it does, increment the value by 1


unigrams = {}
trigrams = {}

for pr in pressReleases:
    txt = pressReleases[pr]["text"]
    txt = re.sub("\W", " ", txt)
    txt = txt.lower()
    tokens = nltk.word_tokenize(txt)
    tokens = [porter.stem(w) for w in tokens]
    tokens = [x for x in tokens if x not in stopwords]
    pressReleases[pr]["numUnigrams"] = len(tokens)
    #add another layer of dictionary to record the frequencies of unigrams in each document
    pressReleases[pr]["doc_unigrams"] = {}
    for i in set(tokens):
        count = tokens.count(i)
        #for each unigram, add a count to the new inner dictionary
        pressReleases[pr]["doc_unigrams"][i] = count
        #add to overall unigrams dictionary, with count
        if i in unigrams:
            unigrams[i] += count
        else:
            unigrams[i] = count
    #now to deal with trigrams
    trigList = list(nltk.trigrams(tokens))
    pressReleases[pr]["numTrigrams"] = len(trigList)
    pressReleases[pr]["doc_trigrams"] = {}
    for j in set(trigList):
        count = trigList.count(j)
        pressReleases[pr]["doc_trigrams"][j] = count
        if j in trigrams:
            trigrams[j] += count
        else:
            trigrams[j] = count 

#To sort by values:
#sorted(a, key=a.get, reverse=True)
#for i in topUnigrams:
#   print i, unigrams[i]


topUnigrams = sorted(unigrams, key=unigrams.get, reverse=True)[:1000]
topTrigrams = sorted(trigrams, key=trigrams.get, reverse=True)[:500]


#write unigrams file
#if you want to calculate your rates out of total words, add a column for this
#headerUni = ["file"] + ["id"] + topUnigrams + ["allUnigrams"] 
headerUni =  ["file"]+ ["id"] + topUnigrams

#os.chdir("/Users/Frannie/Dropbox/TextAsData/ProblemSets/PS3/")
os.chdir("/HW2/Key/")

with open("unigrams.csv", "w") as csvfile:
    writer = csv.writer(csvfile, delimiter= ",")
    writer.writerow(headerUni)
    for i in pressReleases:
        toWrite = []
        toWrite.append(i)
        toWrite.append(pressReleases[i]["author"])
        for j in topUnigrams:
            if j in pressReleases[i]["doc_unigrams"]:
                toWrite.append(str(pressReleases[i]["doc_unigrams"][j]))
            else:
                toWrite.append(str(0))
        #if you want to calculate your rates out of total words, you can add this to your tdm
        #toWrite.append(str(pressReleases[i]["numUnigrams"]))
        writer.writerow(toWrite)


#write trigrams file

#define funciton that takes in tuple, converts to dot delimited string
def tupToString(tup):
    return ".".join(tup)


stringifiedTris = [tupToString(x) for x in topTrigrams]

#if you want to calculate your rates out of total , add a column for this
#headerTri = ["file"] + ["id"] + stringifiedTris + ["allTrigrams"]
headerTri = ["file"] + ["id"] + stringifiedTris

os.chdir("/HW2/Key/")

with open("trigrams.csv", "w") as csvfile:
    writer = csv.writer(csvfile, delimiter= ",")
    writer.writerow(headerTri)
    for i in pressReleases:
        toWrite = []
        toWrite.append(i)
        toWrite.append(pressReleases[i]["author"])
        for j in topTrigrams:
            if j in pressReleases[i]["doc_trigrams"]:
                toWrite.append(str(pressReleases[i]["doc_trigrams"][j]))
            else:
                toWrite.append(str(0))
        #if you want to calculate your rates out of total words, you can add this to your tdm
        #toWrite.append(str(pressReleases[i]["numTrigrams"]))
        writer.writerow(toWrite)


#############start the R code

unis<- read.delim('unis.csv', sep=',')
tris<- read.delim('tris.csv', sep=',')


unis<- unis[,-c(1,2)]
tris<- unis[,-c(1,2)]

dtm<- cbind(unis, tris)

store_sum<- c()
set.seed(8675309)
for(z in 2:(nrow(dtm)-1)){
    clust<- kmeans(dtm, centers = z)
    store_sum[z]<- clust$tot.withinss
    }

set.seed(121507)
kmeans_6<- kmeans(dtm, centers = 6)

store_words<- matrix(NA, nrow = 10, ncol = 6)

for(z in 1:10){
    ert<- kmeans_6$centers[z,] - kmeans_6$centers[-z,]
    store_words[z,]<- colnames(dtm)[order(ert, decreasing=T)[1:10]]
}

source('MixMultinom.R')

mix_6<- mix_mult(dtm, 6, tol = 1e-5, 022808)
mix_assign<- apply(mix_6$rs, 1, which.max)


confuse<- table(mix_assign, kmeans_6$cluster)

