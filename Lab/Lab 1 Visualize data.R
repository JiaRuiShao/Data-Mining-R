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



