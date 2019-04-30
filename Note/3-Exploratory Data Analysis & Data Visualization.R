rm(list = ls())
#load data credit2010.csv
credit=read.csv(file.choose(), header=T)

dim(credit)
str(credit)

mean(credit[,4])

#Add a new variable Age
Age<-2010-credit[,4] 
credit<-cbind(credit,Age)

#Exclude one variable YearofBirth
credit<-credit[,-4]

attach(credit)
names(credit)

#Summary Statistics
summary(credit[,c(4:6)])

apply(credit[,c(1:3,7:8)],2,mean)
apply(credit[,c(1:3,7:8)],2,min)
apply(credit[,c(1:3,7:8)],2,max)
apply(credit[,c(1:3,7:8)],2,median)
apply(credit[,c(1:3,7:8)],2,sd)

#Histogram

par(mfrow=c(2,1))
hist(Balance,breaks=10,col=2,xlab="Balance",xaxt="n",main="Histogram of Balance. Breaks at 200",cex=1,cex.lab=1.5,cex.main=1.75)
axis(1, at=seq(0,2000,by=200), labels=seq(0,2000,by=200) )
hist(Balance,breaks=40,col=2,xlab="Balance",xaxt="n",main="Histogram of Balance. Breaks at 50",cex=1,cex.lab=1.5,cex.main=1.75)
axis(1, at=seq(0,2000,by=200), labels=seq(0,2000,by=200) )


#SCATTERPLOTS

#Pairwise scatterplot for all variables
pairs(credit)

#Bivariate scatterplot
par(mfrow=c(1,2))
cor(Limit,Balance)
plot(Limit,Balance,pch=16,xlab="Limit",ylab="Balance",cex.lab=1.5)
title("Correlation between Limit and Balance is 0.86")

cor(Income,Balance)
plot(Income,Balance,pch=16,xlab="Income",ylab="Balance",cex.lab=1.5)
title("Correlation between Income and Balance is 0.46")
 
cor(Rating,Balance)
plot(Rating,Balance,pch=16,xlab="Rating",ylab="Balance",cex.lab=1.5)
title("Correlation between Rating and Balance is 0.86")

cor(Age,Balance)
plot(Age,Balance,pch=16,xlab="Age",ylab="Balance",cex.lab=1.5)
title("Correlation between Age and Balance is 0.002")

#Pairwise scatterplot for four continuous variables
pairs(credit[c(1:3,8)])

#Boxplot
par(mfrow=c(1,3))
plot(Gender,Balance,col ="red", varwidth =T, xlab=" Gender ",ylab ="Balance ",main="Boxplot of Gender vs Balance",cex.lab=1.5)
plot(Student,Balance,col ="red", varwidth =T, xlab=" Student ",ylab ="Balance ",main="Boxplot of Student vs Balance",cex.lab=1.5)
plot(Married,Balance,col ="red", varwidth =T, xlab=" Married ",ylab ="Balance ",main="Boxplot of Married vs Balance",cex.lab=1.5)

#Conditional Pairwise Scatterplot
par(mfrow=c(1,2))
plot(credit[Student=="No",2],credit[Student=="No",7],pch=16,xlab="Limit",ylab="Balance")
title("Non-Students")
cor(credit[Student=="No",2],credit[Student=="No",7])
text(12000,200,"r=0.90")

plot(credit[Student=="Yes",2],credit[Student=="Yes",7],pch=16,xlab="Limit",ylab="Balance")
title("Students")
cor(credit[Student=="Yes",2],credit[Student=="Yes",7])
text(8000,200,"r=0.85")

par(mfrow=c(1,2))
plot(credit[Student=="No",1],credit[Student=="No",7],pch=16,xlab="Income",ylab="Balance")
title("Non-Students")
cor(credit[Student=="No",1],credit[Student=="No",7])
text(50,1800,"r=0.49")

plot(credit[Student=="Yes",1],credit[Student=="Yes",7],pch=16,xlab="Income",ylab="Balance")
title("Students")
cor(credit[Student=="Yes",1],credit[Student=="Yes",7])
text(150,200,"r=0.33")

