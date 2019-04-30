rm(list=ls())
# Load Data Default.csv to object lab3
HW3=read.csv(file.choose(),header=T)
attach(HW3)
names(HW3)
summary(HW3)

# a.i
train=(Year<2009)
HW3.test=HW3[!train,]
test.truevalue=Direction[!train]
train.truevalue=Direction[train]

glm.fit2=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=HW3,subset=train,family =binomial)
summary(glm.fit2)
exp(coef(glm.fit2))

#Answer
#Lag1 and Lag2 
# 0.9460709 and 1.0547353

# a.ii
# we are using dim function to find out how many variables we have which is 104
dim(HW3.test)

glm.probs2=predict(glm.fit2,HW3.test, type="response")
glm.pred2=rep("Down",104)
glm.pred2[glm.probs2>.5]="Up"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)

# a. iii
library(class)

train.X=cbind(Lag1,Lag2,Lag3,Lag4,Lag5)[train,]
test.X=cbind(Lag1,Lag2,Lag3,Lag4,Lag5)[!train,]
train.Direction=Direction[train]


set.seed(2)

knn.pred=knn(train.X,test.X,train.Direction,k=10)
table(knn.pred,test.truevalue) 
mean(knn.pred==test.truevalue)


# question b. 5 fold cross validation on the original data, not training

glm.original=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=HW3,family =binomial)

k=5
folds=sample(1:k,nrow(HW3),replace=TRUE)

accuracy=rep(0,k)

for(i in 1:k)
{
  glm.original=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,family="binomial",data=HW3[folds!=i,])
  HW3.test.original=HW3[folds==i,]
  glm.probs.original =predict(glm.original,HW3.test.original, type="response")
  glm.pred.original=rep("Down",nrow(HW3[folds==i,]))
  glm.pred.original[glm.probs.original>.5]="Up"
  
  test.truevalue.1=Direction[folds==i]
  accuracy[i]=mean(glm.pred.original==test.truevalue.1)  
}
mean(accuracy) #########accuracy for 5-fold cross-validation using logistic regression


#############b for 10-NN 

Input.standard=cbind(Lag1,Lag2,Lag3,Lag4,Lag5)

accuracy=matrix(0,10,5)

set.seed(2)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)
i=1
for(i in 1:5)
{
  train.standard=Input.standard[folds!=i,]
  test.standard=Input.standard[folds==i,]
  train.truevalue=Direction[folds!=i]
  test.truevalue=Direction[folds==i]
  knn.pred=knn(train.standard,test.standard,train.truevalue,k=10)
  table(knn.pred,test.truevalue)
  accuracy[10,i]=mean(knn.pred==test.truevalue)
}

mean(knn.pred==test.truevalue) ###### accuracy for confusion matrix 10-NN

cv.accuracy=apply(accuracy,1,mean) ##### thius is average, so i dont think we should use this percentage.
#I believe we need to use what we found when we run 5 fold CV separately
cv.accuracy


