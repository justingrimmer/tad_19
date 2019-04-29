#######################
##
##
##
##
##   HW 1 Key, Machine Learning
##
##
##
########################


from bs4 import BeautifulSoup
import re, os
from urllib.request import urlopen

urls = open('/Users/justingrimmer/dropbox/teaching/text/mac18/HW1/Debate1.html', 'r').read()
soup = BeautifulSoup(urls)

prev = ''
statements = []
ps = soup.find_all('p')
text_list = []


for tag in ps:
	text = tag.text 
	test = re.findall('^[A-Z]+:', text)
	if len(test)>0:
		if prev == test[0]:
			clean = re.sub('^[A-Z]+:', '', text)
			text_list.append(clean)
		if prev != test[0]:
			join_text = ' '.join(text_list)
			instance = []
			instance.append(prev)
			instance.append(join_text)
			statements.append(instance)
			prev = test[0]
			clean = re.sub('^[A-Z]+:', '', text)
			text_list = []
			text_list.append(clean)
	if len(test) ==0:
		clean = re.sub('\(.+\)', '', text)
		text_list.append(clean)



uni_dict = {}
tri_dict = {}


stop_words = urlopen('http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop').read()
stop_words = stop_words.decode('utf-8')
stop_words = stop_words.split("\n")


from nltk.stem import PorterStemmer

pt = PorterStemmer()

from nltk import trigrams
from nltk import word_tokenize
import re, os

stemmed_stop = list(map(pt.stem , stop_words))
stemmed_stop = list(map(lambda x: re.sub('\W', '', x), stop_words))


for  z in statements:
	texts = word_tokenize(re.sub('\W', ' ', z[1].lower()))
	texts2 = map(pt.stem, texts)
	texts3 = [x for x in texts2 if x not in stemmed_stop]
	temp_tri = trigrams(texts3)
	for l in texts3:
		if l in uni_dict:
			uni_dict[l]+=1
		if l not in uni_dict:
			uni_dict[l] = 1
	for n in temp_tri:
		if n in tri_dict:
			tri_dict[n]+=1 
		if n not in tri_dict:
			tri_dict[n] = 1

###now finding the biggest terms

un_ranks = sorted(uni_dict, key = uni_dict.get)
tri_ranks = sorted(tri_dict, key = tri_dict.get)


##this is fairly arbitrary, you can select more/fewer features
part1 = un_ranks[-1000:]
part3 = tri_ranks[-500:]


##putting together the document term matrix.  

use_keys = []

for z in range(len(part1)):
    use_keys.append(part1[z])

for z in range(len(part3)):
    use_keys.append(part3[z])


##alright, let's clean the trigrams and then go from there

def cleaner_tri(tuple):
    out = tuple[0] + '_' + tuple[1] + '_' + tuple[2]
    return(out)


out = open('/Users/justingrimmer/dropbox/teaching/text/mac18/HW1/HW1.csv', 'w')
out.write('Number,Speaker')
for z in part1:
	out.write(',%s' %(z))

for z in part3:
	out.write(',%s' %(cleaner_tri(z)))

out.write("\n")

trip_cat = list(map(cleaner_tri, part3))

a = 0 
for z in statements[2:]:
	speaker = re.sub(':', '', z[0])
	texts = word_tokenize(re.sub('\W', ' ', z[1].lower()))
	texts2 = list(map(pt.stem, texts))
	texts3 = [x for x in texts2 if x not in stemmed_stop]
	temp_tri = trigrams(texts3)
	tri_list = list(map(cleaner_tri, temp_tri))
	out.write('%s,%s' %(str(a), speaker))
	for m in part1:
		out.write(',%s' %(str(texts3.count(m))))
	for m in trip_cat:
		out.write(',%s' %(str(tri_list.count(m))))
	out.write('\n')
	a += 1

out.close()




################################
################################
###Switching over to R

dtm<- read.delim('/Users/justingrimmer/dropbox/teaching/text/mac18/HW1/HW1.csv', sep=',')
##just focusing on words
words<- dtm[,3:1502]
words[words>0]<- 1

dtm_counts<- as.matrix(dtm[,3:1502])


number<- apply(words, 2, sum)

idf<- log(nrow(dtm)/number)

remove<- which(idf ==Inf)

dtm_counts<- dtm_counts[,-remove]
idf<- idf[-remove]
##quickly looking
plot(idf)

dtm_idf<- dtm_counts
for(z in 1:ncol(dtm_idf)){
	dtm_idf[,z]<- dtm_counts[,z]*idf[z]
}

base_euc<- as.matrix(dist(dtm_counts))
idf_euc<- as.matrix(dist(dtm_idf))

cosine<- function(x, y){
	x.norm<- x/sqrt(x%*%x)
	y.norm<- y/sqrt(y%*%y)
	out<- x.norm%*%y.norm
	return(out)

}

cos_mat<- function(dtm){
	output<- matrix(NA, nrow = nrow(dtm), ncol = nrow(dtm))
	for(z in 1:nrow(output)){
	for(y in 1:z){
	output[z,y]<- output[y,z]<- cosine(dtm[z,], dtm[y,] )
	}}
	return(output)
}


base_cos<- cos_mat(dtm_counts)
idf_cos<- cos_mat(dtm_idf)

gaus_kern<- function(x, y, sigma){
	out<- exp(-((x-y)%*%(x-y))/sigma)
	return(out)
}

gaus_func<- function(dtm, sigma){
	output<- matrix(NA, nrow = nrow(dtm), ncol = nrow(dtm))
	for(z in 1:nrow(output)){
	for(y in 1:z){
			output[z,y]<- output[y,z]<- gaus_kern(dtm[z,], dtm[y,] , sigma)
				}}
		return(output)}

norm_dtm<- dtm_counts
sum_dtm<- apply(norm_dtm, 1, sum)
for(z in 1:nrow(norm_dtm)){
	norm_dtm[z,]<- dtm_counts[z,]/sum_dtm[z]
}

norm_idf_dtm<- dtm_idf
sum_idf_dtm<- apply(norm_idf_dtm, 1, sum)
for(z in 1:nrow(norm_idf_dtm)){
	norm_idf_dtm[z,]<- dtm_idf[z,]/sum_idf_dtm[z]
}


norms_gaus<- gaus_func(norm_dtm, 1)
norms_idf_gaus<- gaus_func(norm_idf_dtm, 2)

##finding closest
close_dist<- function(mat){
	out<- c()
	for(z in 1:nrow(mat)){
	out[z]<- order(mat[z,])[2]
	}
	return(out)}

close_sim<- function(mat){
	out<- c()
	for(z in 1:nrow(mat)){
	out[z]<- order(mat[z,], decreasing=T)[2]
	}
	return(out)
}

part1<- close_dist(base_euc)
part2<- close_dist(idf_euc)

part3<- close_sim(base_cos)
part4<- close_sim(idf_cos)


part5<- close_sim(norms_gaus)
part6<- close_sim(norms_idf_gaus)

##read and compare statements

##finally calculate between and across
speaker<- as.character(dtm[,2])
obama<- base_cos[which(speaker=='OBAMA'), which(speaker=='OBAMA')]
romney<- base_cos[which(speaker=='ROMNEY'), which(speaker=='ROMNEY')]

within_o<- mean(obama[lower.tri(obama)], na.rm=T)
within_r<- mean(romney[lower.tri(romney)], na.rm=T)

##and now across
store<- c()
a<- 0 
roms<- which(speaker=='ROMNEY')
for(z in which(speaker=='OBAMA')){
	a<- a + 1
	store[a]<- mean(base_cos[z,roms], na.rm=T)

}
across<- mean(store, na.rm=T)

###Bonus Question
log.post<- function(pars, mu, sig.2, Y, X){
  beta<- pars[1:2]
  part1<- dnorm(beta, mean = mu, sd = sqrt(sig.2), log = T)
  part2<- X%*%beta
  probs<- pnorm(part2)
  combine<- log(probs)*Y + log(1- probs)*(1-Y)
  out<- sum(part1) + sum(combine)
  return(out)
}



data_set<- read.delim('DataSet.csv', sep=',')

data_set<- as.matrix(data_set)
Y<- data_set[,1]
X<- data_set[,-1]
##now writing a loop that stores as sigma^2 changes


max.output<- optim(c(1, 1), log.post, method = 'BFGS', control = list(trace =100, fnscale = -1), hessian = T,  X = X, Y = Y , mu = 0, sig.2 = 1000)





sigma.2.seq<- seq(0.01, 10, len = 1000)
store.est<- c()
for(z in 1:len(sigma.2.seq)){
  temp<- optim(c(1, 1), log.post, method = 'BFGS', control = list(trace =100, fnscale = -1), hessian = T,  X = X, Y = Y , mu = 1, sig.2 = sigma.2.seq[z])
  store.est[z]<- temp$par[2]
}

plot(store.est~sigma.2.seq, type='b', xlab = 'Sigma^2', ylab = 'Beta2')