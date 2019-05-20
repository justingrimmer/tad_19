##we need to install two packages to work with today
##stm is the structural topic model packages from Roberts and Stewart
install.packages('stm')

##SnowballC enables some kinds of stemming
install.packages('SnowballC')



cfp<- read.delim('ConsumerNarratives.csv', sep=',')
##check the column names of the cfp data.  we're going to work with a subset of the data
##but there is a lot of information here!!!

##we're going to work with a subset of the data
##looking at the 1000 most recent complaints
complaints<- as.character(cfp$Consumer.complaint.narrative)[1:1000]
state<- as.character(cfp$State)[1:1000]
prod<- as.character(cfp$Product)[1:1000]

##we're now in a position to ingest the texts.  We can use the tools from stm (which wrap around the tm package that we have used previously)

##this code processes the texts
part1<- textProcessor(complaints)

##now, extracting the relevant components 
vocab<- part1$vocab
docs<- part1$documents

##getting the documents ready for analysis
out<- prepDocuments(docs, vocab)
docs2<- out$documents
vocab2<- out$vocab


##we're now ready to use stm to fit an LDA run. the syntax is 
##docs2 = documents
##vocab2 = vocabulary
##10  = number of categories
##seed = setting the seed to ensure we can replicate our results
lda_fit<- stm(docs2, vocab2, 10, seed = 8675309)



##let's use the function labelTopics to label the topics. 
##the syntax is labelTopics(LDA_OUTPUT).  
##use that function to label the topics

labelTopics(lda_fit)


##focusing on the highest probability words, develop a hand label for each of the topics.  
##put those here in comments.  

##what does this tell you about the kind of narratives that go to the consumer financial protection bureau?

##you can access each complaints mixture across topics with lda_fit$theta
##for example, check the mixture for the first complaint
lda_fit$theta[1,]
##which topics are particularly prevalent?

##now, let's see if we can examine how well the model fit for documents that are a large proportion in 
##each category.

##to do this, try writing a for loop that does the following
##for each category, identify the 10 complaints with the highest proportion
##then, view those complaints.  Does the model perform well?

for(z in 1:10){
	out<- order(lda_fit$theta[,z], decreasing=T)[1:10]
	for(m in 1:10){
		print(complaints[out[m]])
		readline('wait')
		}
	print(paste('Topic ', z, sep='') )
	}
	
	
	


##let's now see how the distribution of topics varies across different types of product complaints.  
##to do this we're going to include information about the types of products 
##we need to reprocess the documents to include that information

##putting together the data set that we'll use for the analysis
data_use<- cbind(state, prod)
part1<- textProcessor(complaints, metadata = as.matrix(data_use))

##now creating the relevant objects again
vocab<- part1$vocab
docs<- part1$documents
meta<- part1$meta


##prepping the documents for the run
out2<- prepDocuments(docs, vocab, meta)
vocab<- out2$vocab
docs<- out2$documents
meta<- out2$meta


##now, running stm again, but now we're going to include state to measure prevalence
##to do this, we specify a formula with no dependent variable.  

lda_fit2<- stm(docs, vocab, K = 10, prevalence = ~prod, data = meta, seed = 1215228)


##we can now plot the prevalence effects using some of the tools provided in the package
##specifically, we set the topic that we want to examine---in the case three---and then the variable we're interested
prep<- estimateEffect(c(3)~prod, lda_fit2, metadata= meta)
plot.estimateEffect(prep, 'prod', method = 'pointestimate')

##note that this provides the relative attention to topics over the baseline
##we can also examine several topics at once.  Let's examine how attention to topics differs across topics 3 and 4
prep<- estimateEffect(c(3:4)~prod, lda_fit2, metadata= meta)
plot.estimateEffect(prep, 'prod', method = 'pointestimate')





