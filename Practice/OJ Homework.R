rm(list = ls())
oj<-read.csv(file.choose(),header=T)
library(tree)
names(oj)
attach(oj)
summary(oj)
set.seed(1)
train=sample(nrow(oj),nrow(oj)*0.8)
tree.model=tree(Purchase~.,oj,subset =train)
oj.test=oj[-train,]
Purchase.test=Purchase[-train]
cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)
cv.model
prune.model=prune.tree(tree.model,best=8)
plot(prune.model)
text(prune.model,pretty=0)
prunetree.pred=predict(prune.model,oj.test,type="class")
table(prunetree.pred,Purchase.test)
