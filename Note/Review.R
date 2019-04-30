rm(list=ls())

############################# Lab 1 ##################################

Auto=read.csv(file.choose(),header=T)
head(Auto)
dim(Auto)
str(Auto)

summary(Auto) 

# converts a quantitative variable into a qualitative variable
Auto[,2] =as.factor (Auto[,2])
str(Auto)
summary(Auto[,2])

# Scatterplot 
plot(Auto$horsepower , Auto$mpg )

attach(Auto)
names(Auto)
plot(horsepower , mpg, col ="red", xlab="Horsepower",ylab ="MPG ",xlim=c(30,250), ylim=c(5,50), main="Horsepower vs. MPG", cex.main=1.75)

# multiple plots into one graph
par(mfrow=c(1,2)) 
plot(acceleration , mpg, col ="red", xlab="Acceleration",ylab ="MPG ", main="Acceleration vs. MPG", cex.main=1.75)
plot(weight , mpg, col ="red", xlab="Weight",ylab ="MPG ", main="Weight vs. MPG", cex.main=1.75)

# creates a scatterplot for every scatterplot pair of variables
pairs(Auto)
pairs(Auto[c(3:5)])

# Barplot
par(mfrow=c(1,1)) 
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" Cylinders ", ylab ="MPG ", main="Cylinders vs. MPG")

# Histogram
hist(mpg , breaks =10, col ="red", xlab ="MPG ",xlim=c(0,50),main="Histogram of MPG")
hist(horsepower , breaks =20, col ="red", xlab ="Horsepower ",xlim=c(0,250),main="Histogram of Horsepower")


############################# Lab 2 ##################################

# Load Data Lab2Data.csv to object Datlab2
Datlab2=read.csv(file.choose(),header=T)
names(Datlab2)
attach(Datlab2)
contrasts(x4) # dummy variable

# Multiple linear regression
M1=lm(y~x1+x2+x3+x4,data=Datlab2)
summary(M1)
plot(predict(M1), residuals(M1)) # needs a data transformation - non-linearity

# whether needs a non-linear transformation or not
plot(x1,y,col="blue")#strong pattern in residuals  - polynomial relation
# suggests a polynomial relation between y and x1
M2=lm(y~poly(x1,5))
summary(M2)

# check if the model should include an interaction effect
M3=lm(y~x1*x2)
summary(M3)

# try different models and select a better one
# Hold-out
set.seed(1)
train=sample(nrow(Datlab2),nrow(Datlab2)*0.8)#index for training data set
Datlab2.train=Datlab2[train, ] #training data set
Datlab2.test=Datlab2[-train, ] #test data set
y.test=y[-train]  #the response in the test set

# learn models using the training data set
M4train=lm(y~I(x1^2)+x1+x2+x3+x4,data=Datlab2.train)
M5train=lm(y~I(x1^2)+x3+x4,data=Datlab2.train)

# calculate the MSE of each model using the testing data set to compare model performance
y.predictM4=predict(M4train,Datlab2.test)
M4MSE=mean((y.test-y.predictM4)^2)

y.predictM5=predict(M5train,Datlab2.test)
M5MSE=mean((y.test-y.predictM5)^2)

# AIC and BIC
AIC(M4train)
BIC(M4train)
summary(M4train)

AIC(M5train)
BIC(M5train)
summary(M5train)

# 5-fold Cross-Validation
set.seed(1)
k=5

# create zero-matrix to store results 
M4CVMSE=rep(0,k)
M5CVMSE=rep(0,k)

# split the original dataset into 5 folds
folds=sample(1:k,nrow(Datlab2),replace=TRUE)

for(j in 1:k)
{
  M4CV=lm(y~I(x1^2)+x1+x2+x3+x4,data=Datlab2[folds!=j,])
  M4CVMSE [j]=mean((y-predict(M4CV,Datlab2))[folds==j]^2)
}

for(j in 1:k)
{
  M5CV=lm(y~I(x1^2)+x3+x4,data=Datlab2[folds!=j,])
  M5CVMSE [j]=mean((y-predict(M5CV,Datlab2))[folds==j]^2)
}

MeanM4MSE=mean(M4CVMSE)
MeanM5MSE=mean(M5CVMSE)

# bootstrap
boottrain=sample(nrow(Datlab2), replace=T)
M4boot=lm(y~I(x1^2)+x1+x2+x3+x4,data=Datlab2,subset=boottrain)
mean((y-predict(M4boot,Datlab2))[-boottrain]^2) ###MSE###

# Final Model
M5=lm(y~I(x1^2)+x3+x4,data=Datlab2)
summary(M5)
names(M5)
coef(M5) # returns all coefficients' values 
confint(M5) # shows their associated confidence interval

# identify the existence of outliers
plot(predict(M5), rstudent(M5))
max(rstudent(M5))
min(rstudent(M5))
range(rstudent)

############################# Lab 3 ##################################

# Load Data Default.csv to object lab3
lab3=read.csv(file.choose(),header=T)
attach(lab3)
names(lab3)
summary(lab3)
dim(lab3)
str(lab3)

# Logistic Regression
glm.fit=glm(default~balance+income+student,family="binomial",data=lab3)
summary(glm.fit)
coef(glm.fit)
exp(coef(glm.fit)) #odds ratio of the coefficients

# hold-out
set.seed(1)
train=sample(nrow(lab3),nrow(lab3)*0.8)
lab3.test=lab3[-train, ] #test dataset
test.truevalue=default[-train]
train.truevalue=default[train]

glm.fit2=glm(default~balance+student+income,data=lab3,subset=train,family=binomial)
summary(glm.fit2)
exp(coef(glm.fit2))

# evaluate the prediction accuracy
glm.probs2=predict(glm.fit2,lab3.test, type="response")
glm.pred2=rep("No",2000)
glm.pred2[glm.probs2>.5]="Yes"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)

# 5-fold cross-validation prediction
k=5
folds=sample(1:k,nrow(lab3),replace=TRUE)

accuracy=rep(0,k)

for(i in 1:k)
{
  glm.fit3=glm(default~balance+student+income,family="binomial",data=lab3[folds!=i,])
  lab3.test=lab3[folds==i, ]
  glm.probs3 =predict(glm.fit3,lab3.test, type="response")
  glm.pred3=rep("No",nrow(lab3[folds==i,]))
  glm.pred3[glm.probs3>.5]="yes"
  
  test.truevalue=default[folds==i]
  table(test.truevalue,glm.pred3)       
  accuracy[i]=mean(glm.pred3==test.truevalue)
}
mean(accuracy)

# KNN
library(class)

standardized.balance=scale(balance)
standardized.income=scale(income)

Input.standard=cbind(standardized.balance,standardized.income,student)

accuracy=matrix(0,10,5)

set.seed(2)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)

# use 5-fold cross-validation to select the best K from [1,10]

for(j in 1:10)
  
{
  
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
mean(accuracy)

detach(lab3)

############################# HW 3 ##################################
rm(list=ls())
# Load Data Default.csv to object lab3
HW3=read.csv(file.choose(),header=T)
attach(HW3)
names(HW3)
summary(HW3)
dim(HW3)
str(HW3)

# a.i
train=(Year<2009)
HW3.test=HW3[!train,]
test.truevalue=default[!train]
train.truevalue=default[train]

glm.fit2=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=HW3,subset=train,family =binomial)
summary(glm.fit2)
exp(coef(glm.fit2))

#Answer
#Lag1 and Lag2 
# 0.9460709 and 1.0547353

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,family="binomial",data=HW3)
summary(glm.fit)    
coef(glm.fit)      
summary(glm.fit)$coef
exp(coef(glm.fit))

# a.ii
glm.probs2=predict(glm.fit,HW3.test, type="response")
glm.pred2=rep("Down",104)
glm.pred2[glm.probs2>.5]="Up"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)