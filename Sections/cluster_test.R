library(cluster)

#example clustering

x <- rbind(matrix(rnorm(150,sd = 0.1), ncol = 2),
          matrix(rnorm(150, mean = 1, sd = 0.1), ncol = 2),
           matrix(rnorm(150, mean = 2, sd = 0.1), ncol = 2),
           matrix(rnorm(150, mean = 3, sd = 0.1), ncol = 2))

#plot our toy data
plot(x[,1],x[,2])

# calculate the cluster gap statistic
plot(clusGap(x=x, FUN=kmeans, K.max=8))

# cluster gap intution

xunif <- runif(nrow(x), min(x[,1]), max(x[,1]))
yunif <- runif(nrow(x), min(x[,2]), max(x[,2]))

un <- data.frame(x=xunif, y=yunif)

plot(un$x, un$y)
plot(x[,1],x[,2], col="red")


# demo intuition
k <-  rep(NA, 8)
nullw <- rep(NA, 8)
xw <- rep(NA, 8)

for(i in 1:8){

null_kmeans <- kmeans(un, centers=i)

x_kmeans <- kmeans(x, centers=i)

par(mfrow=c(1,2))
plot(un$x,un$y, col="grey50", xlab="x", ylab="y")
points(null_kmeans$centers[,1],null_kmeans$centers[,2], pch=16, col="dodgerblue")
points(x[,1],x[,2], col="red")
points(x_kmeans$centers[,1],x_kmeans$centers[,2], pch=16, col="darkred")

nullw[i] <- log(null_kmeans$tot.withinss)
xw[i] <- log(x_kmeans$tot.withinss)

plot(1:i,nullw[1:i], xlim=c(1,8), ylim=c(0,10), col="grey50", xlab="k", ylab="log Within-cluster sum squares")
lines(1:i,nullw[1:i], xlim=c(1,8), ylim=c(0,10), col="grey50")
points(1:i,xw[1:i], xlim=c(1,8), ylim=c(0,10), col="red")
lines(1:i,xw[1:i], xlim=c(1,8), ylim=c(0,10), col="red")

k[i] <- log(null_kmeans$tot.withinss)-log(x_kmeans$tot.withinss)

Sys.sleep(3)
}

k


# statistic is not infallible...

numk <- rep(NA, 10)

for(i in 1:10){
x <- rbind(matrix(rnorm(150,sd = 0.1), ncol = 2),
           matrix(rnorm(150, mean = 1, sd = 0.1), ncol = 2),
           matrix(rnorm(150, mean = 2, sd = 0.1), ncol = 2),
           matrix(rnorm(150, mean = 3, sd = 0.1), ncol = 2))
stat <- clusGap(x=x, FUN=kmeans, K.max=8)
numk[i] <- which(stat$Tab[,3]==max(stat$Tab[,3]))
}

hist(numk)