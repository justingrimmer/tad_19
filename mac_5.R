######putting together some example R code

load('~/Dropbox/teaching/text/mac18/class5/FlakeMatrix.RData')

##that loads flake_matrix

extra_stop<- c('rep', 'jeff', 'flake', '2022252635', 'matthew', 'jagirdar', 'email', 'byline','specht', 'sarabjit', 'dateline') 

flake_matrix<- flake_matrix[-c(603, 604),-which(colnames(flake_matrix)%in% extra_stop)]


?kmeans

flake_norm<- flake_matrix
for(z in 1:nrow(flake_norm)){
	flake_norm[z,]<- flake_norm[z,]/sum(flake_norm[z,])
	}
	

n.clust<- 3
set.seed(8675309) ##complicated objective function
k_cluster<- kmeans(flake_norm, centers = n.clust)
table(k_cluster$cluster)

##labeling the topics
##just use the ``biggest" in each category
key_words<- matrix(NA, nrow=n.clust, ncol=10)
for(z in 1:n.clust){
	key_words[z,]<- colnames(flake_matrix)[order(k_cluster$center[z,], decreasing=T)[1:10]]
	}

##we can then try to compare the ``relative" strong words

key_words2<- matrix(NA, nrow=n.clust, ncol=10)
for(z in 1:n.clust){
	diff<- k_cluster$center[z,] - apply(k_cluster$center[-z, ], 2, mean)
	key_words2[z,]<- colnames(flake_matrix)[order(diff, decreasing=T)[1:10]]
	}


setwd('/Users/justingrimmer/dropbox/HousePress/JEFF_FLAKE_20100')
file.show(rownames(flake_matrix)[which(k_cluster$cluster==2)[11]])
file.show(rownames(flake_matrix)[which(k_cluster$cluster==2)[20]])
cluster2<- which(k_cluster$cluster==2)
for(z in 1:len(k_cluster)){
	file.show(rownames(flake_matrix)[which(k_cluster$cluster==2)[z]])
	readline('wait')
	}
	


#################
#################
##Code for the Mixture of Multinomials

mix_mult<- function(X, k, tol, seed){
	library(MCMCpack)
	
	##initializing parameters
	set.seed(seed)
	pis<- rdirichlet(1, alpha = rep(100, k))
	thetas<- matrix(NA, nrow=k, ncol=ncol(X))
	for(z in 1:k){
		thetas[z,]<- rdirichlet(1, alpha=rep(100, ncol(X)))
		}
	rs<- matrix(NA, nrow=nrow(X),ncol=k)
	a<- 0
	t<- 1 
	
	##writing a function to compute the expected value
	e.log<- function(X, pis, thetas, rs){
		log.pis<- log(pis)
		log.thetas<- log(thetas)
		score<- 0
		for(z in 1:nrow(X)){
			part1<- rs[z,]*log.pis
			part2<- 0
			for(j in 1:k){
				part2<- part2 + sum(rs[z,j]*X[z,]*log(thetas[j,] + .000001))
				}
			score<- score + sum(part1) + part2
			}
		return(score)
		}
	
	##iterating while 
	while(a==0){
		for(i in 1:nrow(X)){
			for(j in 1:k){
				denom<- thetas[j,]^{-X[i,]}
				nums<- thetas[-j,]
				new_num<- 0
				for(l in 1:nrow(nums)){
					new_num<- new_num + (pis[l]/pis[j])*prod(nums[l,]^{X[i,]}*denom)}
				rs[i,j]<- ifelse(is.na(1/(1 + new_num))==F,1/(1 + new_num), 0)
				}
			}



			e.old<- e.log(X, pis,thetas,  rs)
		
			##we have to be mildly creative to avoid underlow.  We can avoid this more complicated
			##argument with a variational approximation, which allows for a simple algebraic trick to avoid underflow.  Alternatively, distributions like VMF present no real underflow concern
		
		##now, maximizing parameters
		thetas<- t(rs)%*%X
		for(z in 1:k){
			thetas[z,]<- (thetas[z,] )/(sum(thetas[z,] ) )
			}
		pis<- apply(rs, 2, sum)/sum(rs)
		t<- t + 1
		if(t>1){
			e.new<- e.log(X, pis, thetas, rs)
			change<- e.new - e.old
			print(abs(change))
			if(abs(change)<tol){
				a<- 1}
			}
			}
		out<- list(thetas, pis, rs)
		names(out)<- c('thetas', 'pis', 'rs')
		return(out)
		}	
			
			
k<- 3

test<- mix_mult(flake_matrix, k, 1e-5, 12122007)

table(apply(test$rs, 1, which.max), k_cluster$cluster)

##creating similar statistics to label documents

mult_words<- matrix(NA, nrow = k, ncol=10)
for(z in 1:k){
	diff<- test$thetas[z,] - apply(test$thetas[-z,], 2, mean)
	mult_words[z,]<- colnames(flake_matrix)[order(diff, decreasing=T)[1:10]]
	}
	

#