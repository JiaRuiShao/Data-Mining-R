hw=read.csv(file.choose(),header=T)
names(hw)
attach(hw)

#a

contrasts(Region)
M1=lm(Units~Hours+Lines+Workers+Region,data= hw)
summary(M1)

M2=lm(Units~Hours+Lines+Workers+Region+Region*Workers)
summary(M2)

AIC(M1)
AIC(M2)

#c
set.seed(1)
train=sample(nrow(hw),nrow(hw)*0.7)
hw.train=hw[train, ] 
hw.test=hw[-train, ]
Units.test=Units[-train]  

M3train=lm(Units~Hours+Lines+Workers+Region,data=hw.train)
M4train=lm(Units~Hours+Lines+Workers+Region+Region*Workers,data=hw.train)

Units.predictM3=predict(M3train,hw.test)
M3MSE=mean((Units.test-Units.predictM3)^2)

Units.predictM4=predict(M4train,hw.test)
M4MSE=mean((Units.test-Units.predictM4)^2)

#d 

set.seed(1)

k=10

M3CVMSE=rep(0,k)
M4CVMSE=rep(0,k)

folds=sample(1:k,nrow(hw),replace=TRUE)

for(j in 1:k)
{
  M3CV=lm(Units~Hours+Lines+Workers+Region,data=hw[folds!=j,])
  M3CVMSE [j]=mean((Units-predict(M3CV,hw))[folds==j]^2)
}

for(j in 1:k)
{
  M4CV=lm(Units~Hours+Lines+Workers+Region+Region*Workers,data=hw[folds!=j,])
  M4CVMSE [j]=mean((Units-predict(M4CV,hw))[folds==j]^2)
}

MeanM3MSE=mean(M3CVMSE) 
MeanM4MSE=mean(M4CVMSE)

#e

M5=lm(Units~Workers*Region)
summary(M5)

M3=lm(Units~Hours+Lines+Workers+Region,data=hw)
summary(M3)

#f

plot(predict(M1), rstudent(M1))

detach(hw)
