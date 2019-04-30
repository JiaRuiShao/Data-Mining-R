rm(list = ls())

# Load Data Lab2Data.csv to object Datlab2
Datlab2=read.csv(file.choose(),header=T)
names(Datlab2)
attach(Datlab2)
contrasts(x4) # dummy variable

# Multiple linear regression
M1=lm(y~x1+x2+x3+x4,data=Datlab2)
summary(M1)
plot(predict(M1), residuals(M1)) #strong pattern in residuals - non-linearity

# whether needs a non-linear transformation or not
plot(x1,y,col="blue")
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

###bootstrap### ?????
boottrain=sample(nrow(Datlab2), replace=T)
M4boot=lm(y~I(x1^2)+x1+x2+x3+x4,data=Datlab2,subset=boottrain)
mean((y-predict(M4boot,Datlab2))[-boottrain]^2) ###MSE###

# Final Model
M5=lm(y~I(x1^2)+x3+x4,data=Datlab2)
summary(M5)
names(M5)
coef(M5) # returns all coefficients values 
confint(M5) # shows their associated confidence interval

# identify the existence of non-linearity
plot(predict(M5), rstudent(M5))
max(rstudent(M5))
min(rstudent(M5))





