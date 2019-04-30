rm(list = ls())

wine=read.csv(file.choose(),header=T)

corr<-cor(wine[c(1:11)])
corr

library(ggplot2)
library(ggcorrplot)
ggcorrplot(corr)
ggcorrplot(corr, type = "lower",lab = TRUE)

pairs(wine) #Nothing appears to be highly correlated (>0.70 correlation value); 
#however, there are some pairs of variables that decently correlated. 
#For example, citric acid is decently correlated (>0.4) with the two
#other acidic variables (fixed acidity and volatile acidity). 
#Also, density and pH are decently correlated with fixed acidity. 
#(fixed.acidity and PH are negatively correlated)

pairs(wine[c(1:3,8,9)])
pairs(wine[c(4:8,11)])
pairs(wine[c(6:11)])

wine$quality[wine$quality>=7]<-"good" #converting a continuous variable into categorical variable
wine$quality[wine$quality<7]<-"bad"

wine[,12]=as.factor(wine[,12])

attach(wine)
names(wine)
dim(wine)
summary(wine)
str(wine)

apply(wine[1:11],2,mean)
apply(wine[1:11],2,max)
apply(wine[1:11],2,min)
apply(wine[1:11],2,median)

summary(wine[12])  # numbers of bad and good quality. Calculate the percentage for report
contrasts(quality) #dummy variables 

#now lets create boxplots and see what variables are the potential predictors of Quality
#potential predictors

plot(quality , alcohol, col ="grey",varwidth =T, xlab="Quality",ylab ="Alcohol ", main="Quality vs Alcohol", cex.main=1.5,cex.lab=1.3,cex.axis=1.3)
plot(quality , sulphates, col ="grey",varwidth =T, xlab="Quality",ylab ="Sulphates ", main="Quality vs Sulphates", cex.main=1.5)
plot(quality , volatile.acidity, col ="grey",varwidth =T, xlab="Quality",ylab ="Volatile Acidity ", main="Quality vs Volatile Acidity", cex.main=1.5)
plot(quality , density, col ="grey",varwidth =T, xlab="Quality",ylab ="Density ", main="Quality vs Density", cex.main=1.5)
plot(quality , citric.acid, col ="grey",varwidth =T, xlab="Quality",ylab ="Citric Acid ", main="Quality vs Citric Acid", cex.main=1.5)


plot(quality , fixed.acidity, col ="grey",varwidth =T, xlab="Quality",ylab ="Fixed Acidity ", main="Quality vs Fixed Acitidy", cex.main=1.5)
plot(quality , citric.acid, col ="grey",varwidth =T, xlab="Quality",ylab ="Citric Acid ", main="Quality vs Citric Acid", cex.main=1.5)

# variables that have no affect on response
plot(quality , free.sulfur.dioxide, col ="grey",varwidth =T, xlab="Quality",ylab ="Free Sulfur Dioxide", main="Quality vs Free Sulfur Dioxide", cex.main=1.5)
plot(quality , pH, col ="grey",varwidth =T, xlab="Quality",ylab ="pH ", main="Quality vs pH", cex.main=1.5)
plot(quality , residual.sugar, col ="grey",varwidth =T, xlab="Quality",ylab ="Residual Sugar ", main="Quality vs Residual Sugar", cex.main=1.5)
plot(quality , chlorides, col ="grey",varwidth =T, xlab="Quality",ylab ="Chlorides ", main="Quality vs Chlorides", cex.main=1.5)


#final boxplots for report with potential predictors
par(mfrow=c(1,3))
plot(quality , alcohol, col ="grey",varwidth =T, xlab="Quality",ylab ="Alcohol ", main="Quality vs Alcohol", cex.main=2)
plot(quality , volatile.acidity, col ="grey",varwidth =T, xlab="Quality",ylab ="Volatile Acidity ", main="Quality vs Volatile Acidity", cex.main=2)
plot(quality , sulphates, col ="grey",varwidth =T, xlab="Quality",ylab ="Sulphates ", main="Quality vs Sulphates", cex.main=2)

#final boxplots for report with predictors who has no effect

par(mfrow=c(1,3))
plot(quality , free.sulfur.dioxide, col ="grey",varwidth =T, xlab="Quality",ylab ="Free Sulfur Dioxide", main="Quality vs Free Sulfur Dioxide", cex.main=1.5)
plot(quality , pH, col ="grey",varwidth =T, xlab="Quality",ylab ="pH ", main="Quality vs pH", cex.main=1.5)
plot(quality , residual.sugar, col ="grey",varwidth =T, xlab="Quality",ylab ="Residual Sugar ", main="Quality vs Residual Sugar", cex.main=1.5)


# classsification

library(tree)

names(wine) 
summary(wine)

set.seed(5)

train=sample(nrow(wine),nrow(wine)*0.8)
tree.model=tree(quality~.,wine,subset =train)
wine.test=wine[-train,]
quality.test=quality[-train]
cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)  
cv.model


prune.model=prune.tree(tree.model,best=13)
plot(prune.model)

text(prune.model,pretty=0)
prunetree.pred=predict(prune.model,wine.test,type="class")
table(prunetree.pred,quality.test)
mean(prunetree.pred==wine.test$quality)

# bagging 

library(randomForest)                          
dim(wine)

set.seed(5) 

bag.wine=randomForest(quality~.,data=wine,subset=train,mtry=11,importance=TRUE)
bag.wine
pred.test.bag = predict(bag.wine,newdata=wine[-train,],type="class")
table(pred.test.bag,wine.test$quality)

mean(pred.test.bag==wine.test$quality) 
# random forest

library(randomForest)                          
dim(wine)


set.seed(5) 
rf.wine=randomForest(quality~.,data=wine,subset=train,mtry=3,importance=TRUE)
rf.wine 

pred.test.rf = predict(rf.wine,newdata=wine[-train,],type="class")
table(pred.test.rf,wine.test$quality)

mean(pred.test.rf==wine.test$quality) #### the prediction accuracy
mean(pred.test.rf!=wine.test$quality)


importance(rf.wine) 
varImpPlot(rf.wine)

################################ logistic regression

glm.fit=glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,family="binomial",data=wine)
summary(glm.fit) 
coef(glm.fit)      
exp(coef(glm.fit))            ### Odds Ratio
summary(glm.fit)$coef

#to check for multicollinearity
library(fmsb)

fit<-lm(fixed.acidity~volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol)
summary(fit)
VIF(fit)

fit2<-lm(fixed.acidity~volatile.acidity+residual.sugar+chlorides+total.sulfur.dioxide+density+sulphates+alcohol)
summary(fit2)
VIF(fit2)
#VIF is lower than 10. We are good.



set.seed(5)
train=sample(nrow(wine),nrow(wine)*0.8)
wine.test=wine[-train, ]
test.truevalue=quality[-train]
train.truevalue=quality[train]

glm.fit2=glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine,subset=train,family="binomial")
summary(glm.fit2)
exp(coef(glm.fit2))

dim(wine.test)

# evaluate the perfomance of the model using the test set 

glm.probs2=predict(glm.fit2,wine.test,type="response")
glm.pred2=rep("bad",320)
glm.pred2[glm.probs2>.5]="good"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue) 
mean(glm.pred2!=test.truevalue)

#10-fold cross-validation prediction of the model
k=10
folds=sample(1:k,nrow(wine),replace=TRUE)

accuracy=rep(0,k)

for(i in 1:k)
{
  glm.fit3=glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,family="binomial",data=wine[folds!=i,])
  wine.test=wine[folds==i, ]
  glm.probs3 =predict(glm.fit3,wine.test, type="response")
  glm.pred3=rep("bad",nrow(wine[folds==i,]))
  glm.pred3[glm.probs3>.5]="good"
  
  test.truevalue=quality[folds==i]
  table(test.truevalue,glm.pred3)       
  accuracy[i]=mean(glm.pred3==test.truevalue)  
}
mean(accuracy)


#we should remove pH, free.sulfur.dioxide and citric.acid from the model because 
#a larger (insignificant) p-value suggests that changes in the predictor are not associated with changes in the response.


glm.fit4=glm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+total.sulfur.dioxide+density+sulphates+alcohol,family="binomial",data=wine)
summary(glm.fit4)
coef(glm.fit4)      
exp(coef(glm.fit4))

AIC(glm.fit4)
AIC(glm.fit)

BIC(glm.fit)
BIC(glm.fit4)

set.seed(5)
train=sample(nrow(wine),nrow(wine)*0.8)
wine.test=wine[-train, ]
test.truevalue=quality[-train]
train.truevalue=quality[train]

glm.fit5=glm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+total.sulfur.dioxide+density+sulphates+alcohol,data=wine,subset=train,family="binomial")
summary(glm.fit5)
exp(coef(glm.fit5))

dim(wine.test)

# evaluate the perfomance of the model using the test set 

glm.probs5=predict(glm.fit5,wine.test,type="response")
glm.pred5=rep("bad",320)
glm.pred5[glm.probs5>.5]="good"
table(glm.pred5,test.truevalue)
mean(glm.pred5==test.truevalue) 
mean(glm.pred5!=test.truevalue)

#10-fold cross-validation prediction of the model
k=10
folds=sample(1:k,nrow(wine),replace=TRUE)


accuracy=rep(0,k)

for(i in 1:k)
{
  glm.fit6=glm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+total.sulfur.dioxide+density+sulphates+alcohol,family="binomial",data=wine[folds!=i,])
  wine.test=wine[folds==i, ]
  glm.probs6 =predict(glm.fit6,wine.test, type="response")
  glm.pred6=rep("bad",nrow(wine[folds==i,]))
  glm.pred6[glm.probs6>.5]="good"
  
  test.truevalue=quality[folds==i]
  table(test.truevalue,glm.pred6)       
  accuracy[i]=mean(glm.pred6==test.truevalue)  
}
mean(accuracy)

############################## KNN
library(class) 
dim(wine)
summary(wine)
str(wine)

# as we learned before, all predictors are numerical variables,
# so we have to normalize them so that they would be on a comparable scale

standardized.fixed.acidity=scale(fixed.acidity)
standardized.volatile.acidity=scale(volatile.acidity)
standardized.citric.acid=scale(citric.acid)
standardized.residual.sugar=scale(residual.sugar)
standardized.chlorides=scale(chlorides)
standardized.free.sulfur.dioxide=scale(free.sulfur.dioxide)
standardized.total.sulfur.dioxide=scale(total.sulfur.dioxide)
standardized.density=scale(density)
standardized.pH=scale(pH)
standardized.sulphates=scale(sulphates)
standardized.alcohol=scale(alcohol)

# use the function bind() to combine the standardized variables together
Input.standard=cbind(standardized.fixed.acidity,standardized.volatile.acidity,standardized.citric.acid,standardized.residual.sugar,standardized.chlorides,standardized.free.sulfur.dioxide,standardized.total.sulfur.dioxide,standardized.density,standardized.pH,standardized.sulphates, standardized.alcohol)

# To use KNN, we need to determine K. 
# We can use 5-fold cross-validation to select the best K from [1,10]. 
# Thus, we first create a matrix to store the accuracy results for five folds and ten different K values. 
# We set the initial values for this matrix as zero.  
accuracy=matrix(0,10,5)

set.seed(5)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)

for(j in 1:10){
  
  for(i in 1:5)
  {
    train.standard=Input.standard[folds!=i,]
    test.standard=Input.standard[folds==i,]
    train.truevalue=quality[folds!=i]
    test.truevalue=quality[folds==i]
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j)
    table(knn.pred,test.truevalue)
    accuracy[j,i]=mean(knn.pred==test.truevalue)
  }
  
}    

cv.accuracy=apply(accuracy,1,mean)
cv.accuracy
which.max(cv.accuracy)

# we can see from the result that when k=1, the model has the highest accuracy: 89%

# confusion matrix for 1-NN
# 10NN
accuracy=matrix(0,1,5)

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



##################################### KNN Modified
library(class) 
dim(wine)
summary(wine)
str(wine)

# as we learned before, all predictors are numerical variables,
# so we have to normalize them so that they would be on a comparable scale

glm.fit=glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,family="binomial",data=wine)
summary(glm.fit) 
exp(coef(glm.fit))            ### Odds Ratio
summary(glm.fit)$coef

set.seed(5)
train=sample(nrow(wine),nrow(wine)*0.8)
wine.test=wine[-train, ]
test.truevalue=quality[-train]
train.truevalue=quality[train]

standardized.fixed.acidity=scale(fixed.acidity)
standardized.volatile.acidity=scale(volatile.acidity)
standardized.citric.acid=scale(citric.acid)
standardized.residual.sugar=scale(residual.sugar)
standardized.chlorides=scale(chlorides)
standardized.free.sulfur.dioxide=scale(free.sulfur.dioxide)
standardized.total.sulfur.dioxide=scale(total.sulfur.dioxide)
standardized.density=scale(density)
standardized.pH=scale(pH)
standardized.sulphates=scale(sulphates)
standardized.alcohol=scale(alcohol)

# use the function bind() to combine the standardized variables together
Input.standard=cbind(standardized.fixed.acidity,standardized.volatile.acidity,standardized.citric.acid,standardized.residual.sugar,standardized.chlorides,standardized.free.sulfur.dioxide,standardized.total.sulfur.dioxide,standardized.density,standardized.pH,standardized.sulphates, standardized.alcohol)

# To use KNN, we need to determine K. 
# We can use 5-fold cross-validation to select the best K from [1,10]. 
# Thus, we first create a matrix to store the accuracy results for five folds and ten different K values. 
# We set the initial values for this matrix as zero.  
accuracy=matrix(0,10,5)

set.seed(5)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)

for(j in 1:10){
  
  for(i in 1:5)
  {
    train.standard=Input.standard[folds!=i,]
    test.standard=Input.standard[folds==i,]
    train.truevalue=quality[folds!=i]
    test.truevalue=quality[folds==i]
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j)
    table(knn.pred,test.truevalue)
    accuracy[j,i]=mean(knn.pred==test.truevalue)
  }
  
}    

cv.accuracy=apply(accuracy,1,mean)
cv.accuracy
which.max(cv.accuracy)

# we can see from the result that when k=1, the model has the highest accuracy: 89%

detach(wine)



