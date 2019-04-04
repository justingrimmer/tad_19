##########################
###
###
###  Lecture 2, Macine Learning
###
###
###
##########################
from bs4 import BeautifulSoup
from urllib import request 
import re, os

url  = request.urlopen('http://avalon.law.yale.edu/19th_century/gettyb.asp').read()


soup = BeautifulSoup(url)


text = soup.p.contents[0]


text_1 = text.lower()


text_2 = re.sub('\W', ' ', text_1)



from nltk import word_tokenize
from nltk import bigrams
from nltk import trigrams
from nltk import ngrams


text_3 = word_tokenize(text_2)

text_3_bi = bigrams(text_3)
text_3_tri = trigrams(text_3)
text_3_n = ngrams(text_3, 4)



stop_words = str(request.urlopen('http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop').read())
stop_words.split('\\n')
##we can then identify the stop words and then eliminate them from the list

##this is code that executes a very simple for loop to check the list
text_4 = [x for x in text_3 if x not in stop_words]

##you can check what was removed with:

text_rem = [x for x in text_3 if x not in text_4]

##we're going to use a similar format to apply various stemming/lemmatizing/synonyms algorithms


from nltk.stem.lancaster import LancasterStemmer

st = LancasterStemmer()



from nltk.stem import PorterStemmer

pt = PorterStemmer()


from nltk.stem.snowball import EnglishStemmer

sb = EnglishStemmer()


from nltk.stem.wordnet import WordNetLemmatizer

wn = WordNetLemmatizer()


##let's examine the word ``better"
st.stem('better')
pt.stem('better')
sb.stem('better')
wn.lemmatize('better', 'a')

wn.lemmatize('families', 'n')

##
##applying the porter stemmer to the gettysburg address


text_5 = list(map(pt.stem, text_4))

##now creating a dictionary that will count the occurrence of the words

getty = {}
used = []
for word in text_5:
	if word in getty:
		getty[word] += 1
	if word not in getty and word not in used:
		getty[word] = 1
		used.append(word)

getty_count = list(getty.values())
getty_keys = list(getty.keys())


rfile = open('/users/justingrimmer/dropbox/teaching/text/mac18/class2/GettysburgFinal.txt', 'w')
rfile.write('stem, count')
rfile.write('\n')

for j in range(len(getty_keys)):
	rfile.write('%s,%s' %(getty_keys[j], getty_count[j]))
	rfile.write('\n')


rfile.close()


##position it so that it creates a document
dtm = open('/users/justingrimmer/dropbox/teaching/text/mac18/class2/GettysburgFinalDTM.txt', 'w')

getty_words = 'Document'
getty_numbers = 'Address'
for m in range(len(getty_keys)):
	getty_words += ','
	getty_words += getty_keys[m]
	getty_numbers += ','
	getty_numbers += str(getty_count[m])



dtm.write(getty_words)
dtm.write('\n')
dtm.write(getty_numbers)


for m in range(len(getty_keys)):
	dtm.write(getty_keys[m])
	dtm.write(',')

dtm.write('\n')
dtm.write('address')

for m in range(len(getty_count)):
	dtm.write(str(getty_count[m]))
	dtm.write(',')

dtm.write('\n')

dtm.close()



############
##Regular Expression

create_data.write('Number,Earmark, Wasteful_Spending,Gov_Waste,Tax_Payer,Nat_Debt,Debt,Deficit, Budget, Bud_Def, Big_Gov, Tax_Inc')
create_data.write('\n')

for z in range(1, len(text)):
    ff = open(text[z].strip('\n'), 'r')
    gg = ff.read()
    ff.close()
    ears =  re.findall('earmark|Earmark', gg[1:len(gg)])
    waste = re.findall('wasteful spending|Wasterful spending', gg[1:len(gg)])
    gov_waste=  re.findall('wasteful', gg[1:len(gg)])
    tax_payer = re.findall('taxpayer', gg[1:len(gg)])
    nat_debt = re.findall('national debt|National debt', gg[1:len(gg)])
    debt = re.findall('debt|Debt', gg[1:len(gg)])
    deficit = re.findall('deficit|Deficit', gg[1:len(gg)])
    budget = re.findall('budget|Budget', gg[1:len(gg)])
    bud_def = re.findall('Budget deficit|budget deficit', gg[1:len(gg)])
    big_gov = re.findall('Big government|big government', gg[1:len(gg)])
    tax_inc = re.findall('high tax|High tax|higher tax|Higher tax|tax increase|Tax increase|more tax|More tax', gg[1:len(gg)])
    create_data.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s' %(str(z), str(len(ears)), str(len(waste)), str(len(gov_waste)), str(len(tax_payer)), str(len(nat_debt)), str(len(debt)), str(len(deficit)), str(len(budget)), str(len(bud_def)), str(len(big_gov)) , str(len(tax_inc))))
    create_data.write('\n')
    if z%1000==0:
        print(z)



