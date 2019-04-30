college=read.csv(file.choose(), header=T)
fix(college)

#name the first column 
rownames(college)=college[,1]
fix(college)

#eliminate the first column in the data
college<-college[,-1]

#question 1
dim(college)

#question 2
str(college)

#question 3
summary(college)
attach(college)
names(college)

apply(college[,2:18],2,mean)
apply(college[,2:18],2,max)
apply(college[,2:18],2,min)
apply(college[,2:18],2,median)

#question 4
hist(Grad.Rate,col=2,xlab="Graduation Rate",main="Histogram of Graduation Rate. Breaks at 20",cex=1,cex.lab=1.5,cex.main=1.75)
hist(Grad.Rate,breaks=20,col=2,xlab="Graduation Rate",main="Histogram of Graduation Rate. Breaks at 5",cex=1,cex.lab=1.5,cex.main=1.75)     

#question 5
plot(Private,Grad.Rate,col ="red", varwidth =T, xlab=" Private ",ylab ="Graduation Rate ",main="Boxplot of Private vs Graduation Rate",cex.lab=1.5)


#question 6
pairs(college[c(2:4,18)])
pairs(college[c(5:7,18)])
pairs(college[c(8:10,18)])
pairs(college[c(11:13,18)])
pairs(college[c(14:17,18)])

cor(Grad.Rate,PhD)
cor(Grad.Rate,Top10perc)
cor(Grad.Rate,Top25perc)
cor(Grad.Rate,perc.alumni)
cor(Grad.Rate,Outstate)
cor(Grad.Rate,Room.Board)
cor(Top10perc,Top25perc)
cor(Outstate,Room.Board)

#After observing scatterplots and correlation I chose 3 variables: perc.alumni, Top10perc, Outsate. 
#I eliminated Top25 and Room.Board because they there is multicollinearity with Top10 and Rom.Board
#final scatterplot

pairs(college[c(5,9,16,18)],cex.labels = 2)

detach(college)
