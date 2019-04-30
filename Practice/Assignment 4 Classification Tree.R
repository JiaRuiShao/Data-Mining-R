rm(list = ls())
### a and b 
OJ<-read.csv(file.choose(),header=T) 

library(tree)
names(OJ) 
attach(OJ) 
summary(OJ)
set.seed(1)
train=sample(nrow(OJ),nrow(OJ)*0.8)
tree.model=tree(Purchase~.,OJ,subset =train)
OJ.test=OJ[-train,]

Purchase.test=Purchase[-train]
cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)  
cv.model
prune.model=prune.tree(tree.model,best=8)
plot(prune.model)
text(prune.model,pretty=0)
prunetree.pred=predict(prune.model,OJ.test,type="class")
table(prunetree.pred,Purchase.test)

##### c

library(randomForest)                          

dim(OJ)


set.seed(1) 
rf.oj=randomForest(Purchase~.,data=OJ,subset=train,mtry=4,importance=TRUE)
pred.test.rf = predict(rf.oj,newdata=OJ[-train,],type="class")
table(pred.test.rf,OJ.test$Purchase)

mean(pred.test.rf==OJ.test$Purchase) #### the prediction accuracy
mean(pred.test.rf!=OJ.test$Purchase)


importance(rf.oj) 
varImpPlot(rf.oj)




























