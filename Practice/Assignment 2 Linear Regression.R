HW=read.csv(file.choose(),header=T)
names(HW)
attach(HW)
contrasts(Region)

#a
M1=lm(Units~Hours+Lines+Workers+Region)
summary(M1)

M2=lm(Units~Hours+Lines+Workers+Region+Region*Workers)
summary(M2)

AIC(M1)
AIC(M2)

#c

set.seed(1)
train=sample(nrow(HW),nrow(HW)*0.7)#index for training data 
HW.train=HW[train, ] #training data set
HW.test=HW[-train, ] #test data set
Units.test=Units[-train]  #the response in the test set

M3train=lm(Units~Hours+Lines+Workers+Region,data=HW.train)
M4train=lm(Units~Hours+Lines+Workers+Region+Region*Workers,data=HW.train)

Units.predictM3=predict(M3train,HW.test)
M3MSE=mean((Units.test-Units.predictM3)^2)

Units.predictM4=predict(M4train,HW.test)
M4MSE=mean((Units.test-Units.predictM4)^2)

#d

set.seed(1)

k=10

M3CVMSE=rep(0,k)
M4CVMSE=rep(0,k)

folds=sample(1:k,nrow(HW),replace=TRUE)

for(j in 1:k)
{
  M3CV=lm(Units~Hours+Lines+Workers+Region, data=HW[folds!=j,])
  M3CVMSE [j]=mean((Units-predict(M3CV,HW))[folds==j]^2)
}

for(j in 1:k)
{
  M4CV=lm(Units~Hours+Lines+Workers+Region+Region*Workers, data=HW[folds!=j,])
  M4CVMSE [j]=mean((Units-predict(M4CV,HW))[folds==j]^2)
}

MeanM3MSE=mean(M3CVMSE) ###MSE###
MeanM4MSE=mean(M4CVMSE) ###MSE###

`#f



M5=lm(Units~Region*Workers)
summary(M5)

plot(predict(M5), rstudent(M5))
plot(predict(M5),residuals(M5))
