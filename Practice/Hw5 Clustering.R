USarrests<-read.csv(file.choose(),header=T)

names(USarrests)
attach(USarrests)
dim(USarrests)
summary(USarrests)
USarrests.labs=USarrests[,1]
USarrests.data=USarrests[,2:5]
dim(USarrests.data)
table(USarrests.labs)

#Question 1 hieratchical clustering


data.dist=dist(USarrests.data)
hc=hclust(data.dist, method="average")
plot(hc, main="Average Linkage", xlab="",sub="",ylab="")
hc.clusters=cutree(hc,4)
table(hc.clusters,USarrests.labs)

par(mfrow=c(1,1))
plot(hc, labels=USarrests.labs)
abline(h=60, col="red")
hc

#Question 2 K-means

set.seed(1)
km.out =kmeans (USarrests.data,4, nstart =1)
km.out
km.out$cluster
table(km.out$cluster,USarrests.labs)

km.out$withinss 
km.out$betweenss



#Question 3

wss = km.out$totss 
for (i in 2:10) wss[i] = sum(kmeans(USarrests.data,centers=i)$withinss)      

plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Find the Optimal Value of K")

set.seed(1)
km.out1 =kmeans (USarrests.data,3, nstart =1)
km.out1
km.out1$cluster
table(km.out1$cluster,USarrests.labs)

detach(USarrests)
