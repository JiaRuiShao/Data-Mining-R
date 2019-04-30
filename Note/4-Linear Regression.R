rm(list = ls())
#load advertising.csv###############
Ad=read.csv(file.choose(),header=T)
names(Ad)
attach(Ad)

M1=lm(Sales~TV)
summary(M1)

plot(TV,Sales,col="red")
abline(M1,lwd=3,col="red")

M2=lm(Sales~TV+Radio+Newspaper)
summary(M2)

cor(Ad)

###############Subset Selection##########################
library (leaps)

#######Stepwise Selection#####
regfit.fwd=regsubsets(Sales~.,data=Ad,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Sales~.,data=Ad,method="backward")
summary(regfit.bwd)
regfit.hybrid=regsubsets(Sales~.,data=Ad,method="seqrep")
summary(regfit.hybrid)

#######Best Subset Selection####
reg.best=regsubsets(Sales~.,data=Ad)
summary(reg.best)

reg.summary=summary(reg.best)
names(reg.summary)
reg.summary$rsq   ######unadjusted R2######
which.max(reg.summary$rsq)
reg.summary$rss   ###residual sum of squares###
which.min(reg.summary$rss)

par(mfrow=c(1,2))
######adjusted R2######
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b",pch=8)
which.max(reg.summary$adjr2)
points(2,reg.summary$adjr2[2], col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="b",pch=8)
which.min(reg.summary$bic)
points(2,reg.summary$bic[2],col="red",cex=2,pch=20)

######coefficient of the model selected from best selection#########
coef(reg.best,2)
##################subset selection end####################


###########################Other Practical Issues##############

#########Interaction effect######
M3=lm(Sales~TV*Radio)
summary(M3)

#####Outliers#####
par(mfrow=c(1,2))
plot(predict(M3), residuals(M3))
plot(predict(M3), rstudent(M3))

plot(M3)

#############Collinearity#############
library(car)
library(MASS)
library(nnet)
vif(M2)


########Polynomial Model##########
#####Load AutoLab1.csv##########
AutoData=read.csv(file.choose(),header=T)
names(AutoData)
attach(AutoData)

Model1=lm(AutoData$mpg~horsepower)
summary(Model1)
Model2=lm(AutoData$mpg~I(horsepower^2)+horsepower)
summary(Model2)

par(mfrow=c(1,2))
plot(predict(Model1), residuals(Model1))
plot(predict(Model2), residuals(Model2))

par(mfrow=c(1,2))
plot(predict(Model1), rstudent(Model1))
plot(predict(Model2), rstudent(Model2))

########Qualitative Predictors##########
#####load credit2010.csv###
credit=read.csv(file.choose(), header=T)
attach(credit)
names(credit)

contrasts(Student)

Mod1=lm(Balance~Income*Student+Limit+Rating,data=credit)
summary(Mod1)
vif(Mod1) ####Rating and Limit have Collinearity###

plot(Mod1)

Mod1=lm(Balance~I(Income^2)+Student+Rating,data=credit)
summary(Mod1)
vif(Mod1)  ####No Collinearity###
plot(Mod1)