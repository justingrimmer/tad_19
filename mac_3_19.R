
load('FlakeMatrix.RData')

##that loads flake_matrix

extra_stop<- c('rep', 'jeff', 'flake', '2022252635', 'matthew', 'jagirdar', 'email', 'byline','specht', 'sarabjit', 'dateline') 

flake_matrix<- flake_matrix[-c(603, 604),-which(colnames(flake_matrix)%in% extra_stop)]


euc1<-as.matrix(dist(flake_matrix, method='euclidean'))
man1<- as.matrix(dist(flake_matrix, method='manhattan'))
max1<- as.matrix(dist(flake_matrix, method = 'minkowski', p = 4)

##quickly comparing distances

e_lower<- euc1[lower.tri(euc1)]
man_lower<- man1[lower.tri(man1)]
max_lower<- max1[lower.tri(max1)]

dists<- cbind(e_lower, man_lower, max_lower)

pairs(dists)


cor(dists)

###calculating the similarity between the documents

cos_sim<- matrix(NA, nrow = nrow(flake_matrix), ncol = nrow(flake_matrix))

inner_prod<- flake_matrix %*% t(flake_matrix)

vec_length<- sqrt(diag(inner_prod))


for(z in 1:nrow(flake_matrix)){
	for(y in 1:z){
		cos_sim[z,y]<- cos_sim[y,z]<- inner_prod[z,y]/(vec_length[z]*vec_length[y])
	}
}


dists2<- cbind(dists, cos_sim[lower.tri(cos_sim)])

pairs(dists2)

cor(dists2)
