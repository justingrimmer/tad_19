load('ExampleLASSOData.RData')

library(glmnet)
##x is the dtm 
x<- LASSO.list[[1]]



##y are dichotomous indicators of credit claiming or not
y<- LASSO.list[[2]]



y.train<- y[1:697]
x.train<- x[1:697,]

y.valid<- y[-c(1:697)]
x.valid<- x[-c(1:697),]



##cv.glmnet uses a ``cross validation" method to determine a key parameter in LASSO, I strongly recommend using it
##alpha = 1 is the LASSO
##alpha = 0 is Ridge
train.model <- cv.glmnet(x = x.train, y = y.train, alpha = 1, nfolds=5, family="binomial", type.measure="mse")

predict.new<- predict(train.model, newx = x.valid, s = train.model$lambda.min)

predict.probs<- 1/(1 + exp(-predict.new))
final.pred<- ifelse(predict.probs>0.46, 1, 0) ## we determined this threshold via crossvalidation


###calculating the accuracy table quickly.  
table(y.valid, final.pred)