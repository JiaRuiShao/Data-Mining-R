rm(list=ls())
# Load Data Default.csv to object lab3
HW3=read.csv(file.choose(),header=T)
attach(HW3)
names(HW3)
summary(HW3)
dim(HW3)
str(HW3)

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,family="binomial",data=HW3)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
exp(coef(glm.fit))

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
glm.probs2=predict(glm.fit2,HW3.test, type="response")
glm.pred2=rep("Down",104)
glm.pred2[glm.probs2>.5]="Up"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)

# a. iii
library(class)

standardized.Lag1=scale(Lag1)
standardized.Lag2=scale(Lag2)
standardized.Lag3=scale(Lag3)
standardized.Lag4=scale(Lag4)
standardized.Lag5=scale(Lag5)

Input.standard=cbind(standardized.Lag1,standardized.Lag2,standardized.Lag3,standardized.Lag4,standardized.Lag5)

# 10NN
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

cv.accuracy=apply(accuracy,1,mean)
cv.accuracy

# b.

# 5-fold cross-validation prediction using logistic regression
k=5
folds=sample(1:k,nrow(HW3),replace=TRUE)

accuracy=rep(0,k)

for(i in 1:k)
{
  glm.fit3=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,family="binomial",data=HW3[folds!=i,])
  HW3.test=HW3[folds==i, ]
  glm.probs=predict(glm.fit3,HW3.test, type="response")
  glm.pred=rep("Down",nrow(HW3[folds==i,]))
  glm.pred[glm.probs>.5]="Up"
  
  test.truevalue=Direction[folds==i]
  table(test.truevalue,glm.pred)       
  accuracy[i]=mean(glm.pred==test.truevalue)
}
mean(accuracy)
accuracy
