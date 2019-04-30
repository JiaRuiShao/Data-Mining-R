iris<-read.csv(file.choose(),header=T)
names(iris)
attach(iris)
dim(iris)
summary(iris)
iris.labs=iris[,5]
iris.data=iris[,1:4]
dim(iris.data)
table(iris.labs)

data.dist=dist(iris.data)
hc1=hclust(data.dist)
hc2=hclust(data.dist, method="average")
hc3=hclust(data.dist, method="single")

plot(hc2, main="Average Linkage", xlab="", sub="",ylab="")


hc.clusters1=cutree(hc1,3)
hc.clusters2=cutree(hc2,3)
hc.clusters3=cutree(hc3,3) 

table(hc.clusters1,iris.labs)
table(hc.clusters2,iris.labs)
table(hc.clusters3,iris.labs) 

library(RColorBrewer)
par(mfrow=c(1,2))
plot(Sepal.Width ~ Sepal.Length, data=iris, col=brewer.pal(3, "Set2")[iris$Species], main="Sepal.Width ~ Sepal.Length")
legend(x=6.5, y=4.5, legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=1)
plot(Petal.Width ~ Petal.Length, data=iris, col=brewer.pal(3, "Set2")[iris$Species],main="Petal.Width ~ Petal.Length")
legend(x=6.5, y=4.5, legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=1)

iris.data2=iris[,3:4]
data.dist2=dist(iris.data2)

newhc1=hclust(data.dist2)
newhc2=hclust(data.dist2, method="average")
newhc3=hclust(data.dist2, method="single")

newhc.clusters1=cutree(newhc1,3)
newhc.clusters2=cutree(newhc2,3)
newhc.clusters3=cutree(newhc3,3)

table(newhc.clusters1,iris.labs)
table(newhc.clusters2,iris.labs)
table(newhc.clusters3,iris.labs)
par(mfrow=c(1,1))
plot(newhc2, labels=iris.labs)
abline(h=1.3, col="red")
newhc2

set.seed(1)
km.out1 =kmeans (iris.data,3, nstart =1)
km.out1
km.out1$cluster
km.out1$betweenss
km.out1$withinss
km.out1$tot.withinss
km.out1$totss 

plot(iris.data, col=(km.out1$cluster+1), pch=20, cex=2)

table(km.out1$cluster,iris.labs)


set.seed(11)
km.out2 =kmeans (iris.data,3, nstart =1)
km.out2$betweenss
km.out2$withinss
km.out2$tot.withinss
km.out2$totss 
plot(iris.data, col=(km.out2$cluster+1), pch=20, cex=2)
table(km.out2$cluster,iris.labs)

set.seed(1)
km.out3 =kmeans (iris.data,3, nstart =20)
km.out3$betweenss
km.out3$withinss
km.out3$tot.withinss
km.out3$totss 
plot(iris.data, col=(km.out3$cluster+1), pch=20, cex=2)
table(km.out3$cluster,iris.labs)
iris.data2=iris[,3:4]
set.seed(1)

sd.data=scale(iris.data2)
km.out4 =kmeans (sd.data,3, nstart =20)
km.out4$betweenss
km.out4$withinss
km.out4$tot.withinss
km.out4$totss 
plot(sd.data, col=(km.out4$cluster+1), pch=20, cex=2)
table(km.out4$cluster,iris.labs)

km.out5 =kmeans (iris.data2,3, nstart =20)
km.out5$betweenss
km.out5$withinss
km.out5$tot.withinss
km.out5$totss 
plot(iris.data2, col=(km.out5$cluster+1), pch=20, cex=2)
table(km.out5$cluster,iris.labs)

wss = km.out5$totss 
for (i in 2:10) wss[i] = sum(kmeans(iris.data2,centers=i)$withinss)      

plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="find the optimal value of K")




