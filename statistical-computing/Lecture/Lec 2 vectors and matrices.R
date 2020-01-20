#########################
## Lecture 3:          ##
## Vectors & Matrices  ##
#########################

###########
## Intro ##
###########

mat = matrix(c(1,2,4,4), nrow = 2, ncol = 4)
vec1 = c(1, 2, 3, 5)
vec2 = c(1, 3, 2, 5)

# Vectors: "lists" of things
vec1

# order of elements matters
vec1
vec2
# vec1 and vec2 aren't the same

# Matrices: "boxes" of things
mat
# matrices are 2D objects: they have rows and columns




#############
## Vectors ##
#############

## Basics ##

# we can define vectors
x1 = c(1, 2, 3, 4, 5, 6)
y1 = c("a","b","c","d","efg")
z1 = c("a", 2, 3, "e")

# print vectors
z1
# ranges of numeric values, ascending and desc.
1:2019
20:10
# lengths of vectors
length(x1)
## Operations with numeric vectors ##
x1 = 1:6
x2 = 7:12

# scalar-vector operations: +, -, *, /
x1+5
# componentwise vector-vector operations: +, -, *, /
x1
x2
x1*x2
# find mean, sd, var, sum and prod of a vector
mean(x1)
sd(x1)
var(x1)
sum(x1)
prod(x2)
## Exercises ##

# what is the value of 1 * 2 * ..... * 10?
v1 = 1:10
prod(v1)
prod(1:10)

# what happens if you try to find 1 * 2 * .... * 2019?
log(prod(1:2019))

sum(log(1:2019))

# Let
x = 1:10
y = 11:20
# find the R equivalent of 
# the function SUMPRODUCT in Excel
x
y
sum(x*y)

## Concatenating ##
x1 = 1:6
x2 = 7:12

## add the value 10 to the end of x1
x3 = c(x1, 10)
## add the value 10 to the beginning of x1
x4 = c(10,x1)
## concatenate x1 and x2
c(x1, x2)

## Indexing: brackets! ##

x1 = 1:6

# first entry
x1[1]
# 4th entry 
x1[4]
# last entry 
x1[length(x1)]
# third and fifth entry 
x1[c(3,5)]
# second through fifth entries 
x1[2:5]
# all but first entry 
x1[-1]
# exclude third and fifth entries  
x1[-c(3,5)]
# exclude 2nd through 5th
x1[-(2:5)]
x = c("a", "b", "c", "e")
# modify fourth entry of x from "e" to "d"
x[4] = "d"
x

##############
## Matrices ##
##############

## Basics ##

A1 = matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=FALSE) # read by row 
A2 = matrix(c(1,3,2,4), nrow=2, ncol=2, byrow=FALSE) # read by column
A3 = matrix(c("A","B","C","D"), nrow=2, ncol=2, byrow=TRUE) # read by row

# default is byrow = FALSE
A4 = matrix(c(T, F, T, F), nrow = 2)
A4

# print
A1
# class

# scalar-matrix operations: +, -, *, /
A1*5

# compwise +, -, *, /
A1
A2

A1+A2
A1-A2
A1*A2
A1%*%A2 # matrix product
## Indexing ##
A1 = matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE) # read by row 
A2 = matrix(c(1,3,2,4), nrow=2, ncol=2, byrow=FALSE) # read by column

# first row, second column of A1
A1[1,2]
# first row of A1
A1[1,]
# second column of A2
A1[,2]
B = matrix(c(1:9), nrow = 3, ncol=3)

# first 2 rows & cols of B
B[1:2,1:2]
B[-3,-3]
## Useful functions ##
B[,-c(1,3)]
# rownames and colnames
rownames(B) = c("Annie","Bobbie","Carol")
colnames(B) = c("HW1", "HW2", "Project")
B
# colSums, rowSums, sum
colSums(B)
rowSums(B)
sum(B)
## TopHat exercises ##

# Bob ate:
# Breakfast: 50g of carbs, 8g of fat, and 20g of protein
# Lunch: 60g of carbs, 30g of fat, and 40g of protein
# Dinner: 40g of carbs, 30g of fat, 40g of protein

bob = matrix(c(50,8,20,60,30,40,40,30,40), nrow = 3, ncol = 3, byrow=T)
bob
# fat, and protein in the meals  

# `rownames` should be `breakfast`, `lunch`, and `dinner`
rownames(bob) = c("breakfast","lunch","dinner")
# `colnames` should be `carbs`, `fat`, and `protein`
colnames(bob) = c("carbs","fat","protein")
bob

# how many grams of carbs, fat, and protein did 
# Bob eat yesterday?
colSums(bob)

# how many kcals did Bob eat for breakfast, lunch, and dinner?
# 1g carbs = 1g protein = 4 kcal
# 1g fat = 9 kcal
bob
# how many calories did he eat in total?
carbs = bob[,1]
fat = bob[,2]
protein = bob[,3]

calcarbs = 4*carbs
calfat = 9*fat
calprotein = 4*protein
calcarbs
calfat
calprotein

calcarbs+calfat+calprotein
sum=sum(calcarbs+calfat+calprotein)
# what % calories came from carbs, fat, protein?
sum(calcarbs)/sum
sum(calfat)/sum
sum(calprotein)/sum

##########################
## Intro to data.frames ##
##########################

# work with iris dataset
?iris

data(iris)

# str

# summary

# head

# tail

# we can index the same way we indexed matrices
# e.g. subset rows 1 through 10 and exclude first two columns

# we can extract variables using $ followed by the name of the variable
# e.g. create a new variable that extracts "Species" out of iris

# we can also get subsets filtering by logical conditions
# e.g. create subset that contains only setosa

# creating data.frames from scratch:
df = data.frame(var1 = c(1, 2, 3), var2 = c("A","B","C"))
# equivalently
var1 = c(1, 2, 3)
var2 = c("A","B","C")
df = data.frame(var1, var2)

# can rename rows and columns of data.frame with rownames & colnames

# adding new variables to a data.frame
# e.g. add var3 below to df
var3 = c("X","Y","Z")

# we can use cbind

# or, we can simply write
df$var3 = var3

## ------- ##
## Factors ##
## ------- ##

# another way of working with categorical data

# defining factors
fac1 = factor(c("dog","cat","cat","dog"))

# summary


## TopHat Exercise
# In the iris dataset, how many species of type setosa are there?

# levels: categories contained in factor

# read in hsb2 data
hsb2 = read.csv("http://vicpena.github.io/sta9750/spring19/hsb2.csv")

# str, summary, head

# levels of ses

# if you produce summaries, the order will be counterintuitive
# e.g. tabulate ses by race

# reorder levels


##
## Installing libraries
## 

# install ggplot2
install.packages('ggplot2')
# load ggplot2
library(ggplot2)

## ----------------------------- ##
## Basic data summaries with `R` ##
## ----------------------------- ##

# load mpg
?mpg
data(mpg)
str(mpg)
summary(mpg)

# Tables
# ------

# table make

# table make by year

# proportion table of make
# define table first, then use prop.table

# total proportion table of make / year

# row proportions

# column proportions


# Tophat Exercises
###################


# What is the maximum value of highway miles per gallon in the mpg dataset?
# How many cars with manual transmission are there in the mpg dataset?
# What % of the cars in the mpg dataset are SUVs?
# How many SUVs in the mpg dataset are 4-wheel drives?
# What is the % of Toyotas in the mpg dataset that are SUVs?



# Plots
# -----

# histogram displ

# can play around with scales, titles, etc

# boxplot displ

# boxplot displ by make


# ggplots look a little different
# qplot of displ

# qplot of make











