#read file
iris<-read.csv(file.choose(),header=T)
names(iris)
attach(iris)
dim(iris)
summary(iris)
iris.labs=iris[,1]
iris.data=iris[,2:5]
dim(iris.data)
table(iris.labs)

# Qestion 1 
data.dist=dist(iris.data)
hc=hclust(data.dist, method="average")
plot(hc, main="Average Linkage", xlab="", sub="",ylab="") 
hc.clusters=cutree(hc,4) 
table(hc.clusters,iris.labs) 

par(mfrow=c(1,1)) 
plot(hc, labels=iris.labs) 
abline(h=60, col="red") 
hc

# Qestion 2
set.seed(1)
km.out1 =kmeans (iris.data,4, nstart =1)
km.out1
km.out1$cluster
table(km.out1$cluster,iris.labs)

km.out1$withinss
km.out1$betweenss

# Qestion 3
wss = km.out1$totss  
for (i in 2:10) wss[i] = sum(kmeans(iris.data,centers=i)$withinss)       
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="the optimal value of K") 

set.seed(1)
km.out1 =kmeans (iris.data,3, nstart =1)
km.out1
km.out1$cluster