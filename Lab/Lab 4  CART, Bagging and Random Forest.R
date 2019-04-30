rm(list = ls())

iris<-read.csv(file.choose(),header=T) 

library(tree)

names(iris) 

attach(iris) 

summary(iris)

set.seed(1)

train=sample(nrow(iris),nrow(iris)*0.8)

tree.model=tree(Species~.,iris,subset =train)

iris.test=iris[-train,]

Species.test=Species[-train]

cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)  

cv.model

prune.model=prune.tree(tree.model,best=3)

plot(prune.model)

text(prune.model,pretty=0)

prunetree.pred=predict(prune.model,iris.test,type="class")

table(prunetree.pred,Species.test)

rm(list = ls())

library(tree)

Boston=read.csv(file.choose(),header=T)

head(Boston)

set.seed(1)

train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston=tree(medv~.,Boston,subset=train)  

cv.boston=cv.tree(tree.boston,K=10)                       

cv.boston
            
prune.boston=prune.tree(tree.boston,best=8)     
         
plot(prune.boston)

text(prune.boston,pretty=0)

boston.test=Boston[-train,"medv"]        
                 
tree.pred=predict(prune.boston,newdata=Boston[-train,])    

mean((tree.pred-boston.test)^2) 

library(randomForest)                          

set.seed(1) 

bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE) 

bag.boston

yhat.bag = predict(bag.boston,newdata=Boston[-train,]) 

mean((yhat.bag-boston.test)^2)

set.seed(1) 

rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=5,importance=TRUE) 

yhat.rf = predict(rf.boston,newdata=Boston[-train,]) 

mean((yhat.rf-boston.test)^2)

importance(rf.boston) 

varImpPlot(rf.boston)








