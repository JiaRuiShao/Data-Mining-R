Default=read.csv(file.choose(),header=T)

attach(Default)
names(Default)
dim(Default)
summary(Default)


#################################################
glm.fit=glm(default~balance+income+student,family="binomial",data=Default)
summary(glm.fit)    
coef(glm.fit)      
summary(glm.fit)$coef
exp(coef(glm.fit))            ### Odds Ratio


set.seed(1)
train=sample(nrow(Default),nrow(Default)*0.8)
Default.test=Default[-train, ]
test.truevalue=default[-train]
train.truevalue=default[train]

glm.fit2=glm(default~balance+student+income,data=Default,subset=train,family =binomial)
summary(glm.fit2)
exp(coef(glm.fit2))

glm.probs2=predict(glm.fit2,Default.test, type="response")
glm.pred2=rep("No",2000)
glm.pred2[glm.probs2>.5]="Yes"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)

k=5
folds=sample(1:k,nrow(Default),replace=TRUE)

accuracy=rep(0,k)

    for(i in 1:k)
     {
      glm.fit3=glm(default~balance+student+income,family="binomial",data=Default[folds!=i,])
      Default.test=Default[folds==i, ]
      glm.probs3 =predict(glm.fit3,Default.test, type="response")
      glm.pred3=rep("No",nrow(Default[folds==i,]))
      glm.pred3[glm.probs3>.5]="yes"
      
      test.truevalue=default[folds==i]
      table(test.truevalue,glm.pred3)       
      accuracy[i]=mean(glm.pred3==test.truevalue)  
     }
mean(accuracy)

#############KNN#####################
library(class)

standardized.balance=scale(balance)
standardized.income=scale(income)

Input.standard=cbind(standardized.balance,standardized.income,student)

accuracy=matrix(0,10,5)

set.seed(2)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)

for(j in 1:10){

    for(i in 1:5)
     {
      train.standard=Input.standard[folds!=i,]
      test.standard=Input.standard[folds==i,]
      train.truevalue=default[folds!=i]
      test.truevalue=default[folds==i]
      knn.pred=knn(train.standard,test.standard,train.truevalue,k=j)
      table(knn.pred,test.truevalue)
      accuracy[j,i]=mean(knn.pred==test.truevalue)
     }
  
}    

cv.accuracy=apply(accuracy,1,mean)
which.max(cv.accuracy)








