x<-55/5
y=sqrt(16)
z=x*2
a=z^2
help(rnorm)
5==6
3<5
2!=2
(5==6)&(3<5)
a=false
y<- c(T,T,F,T,F)
y<- ((1:8)>5)
y
x=c(1,3,2,5,7,9)
y=x[2]
y=x[-2]
y
z=x[2:5]
z
a=x[c(2,5)]
a
rm(list=ls())
b=rep(2,5)
b
b=seq(1,6,1)
b
length(b)
c=b+x
b
c
cor(b,c)

x=matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=3,ncol=4)
x
x[1,2]
x[2,]
x[1,c(1,3)]
x[c(1,3),c(1,3)]
x[1:2,1:2]
x[,-4]
apply(x,1,mean)

y=matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=3,ncol=4,byrow=TRUE)
y
dim(y)
x <- matrix(c(1,3,5,7,9,11),ncol=2,byrow=T) 
x
rownames(x)=c("R1","R2","R3")
colnames(x)=c("V1","V2")
x
dimnames(x)<-list(c("Row 1","Row 2","Row 3"),c("Col1","Col2")) 
x
x<-data.frame(x)
attach(x)
Col1
e<- data.frame(     id = c (1:5),      name = c("A","B","C","D","E"),   score = c(80,90.2,85,93,92) )
e

y<-0
while(y<5)
y<-y+2  
y
