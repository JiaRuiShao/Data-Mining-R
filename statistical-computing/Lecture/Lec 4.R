## Tophat exercise ##

# Bob ate:
# Breakfast: 50g of carbs, 8g of fat, and 20g of protein
# Lunch: 60g of carbs, 30g of fat, and 40g of protein
# Dinner: 40g of carbs, 30g of fat, 40g of protein
# Create a matrix that combines the information above

# Each meal should be in a different row
# and the columns should contain the grams of carbs,
# fat, and protein in the meals  
bob = matrix(c(50, 8, 20, 60, 30, 40, 40, 30, 40), 
             nrow = 3, ncol = 3, byrow = T)
# `rownames` should be `breakfast`, `lunch`, and `dinner`
rownames(bob) = c("breakfast","lunch","dinner")
# `colnames` should be `carbs`, `fat`, and `protein`
colnames(bob) = c("carbs","fat","protein")
bob

# how many grams of carbs, fat, and protein did Bob eat yesterday?
colSums(bob)[2]
sum(bob[,2])

# how many kcals did Bob eat for breakfast, lunch, and dinner?
# 1g carbs = 1g protein = 4 kcal
# 1g fat = 9 kcal


# find vectors w grams of carbs fat & protein
carb = bob[,1]
fat = bob[,2]
protein = bob[,3]
# convert to kcal
calcarbs = 4*carb
calfat = 9*fat  
calprotein = 4*protein 
calcarbs+calfat+calprotein
total = sum(calcarbs+calfat+calprotein) # total cal
# % of total calories that came from carbs, fat, and protein
round(100*sum(calcarbs)/total, 2)
round(100*sum(calfat)/total, 2)
round(100*sum(calprotein)/total, 2)


##########################
## Intro to data.frames ##
##########################

# work with iris dataset
?iris

data(iris)
iris
class(iris)

# str
str(iris)

# summary
summary(iris)

# head
head(iris)
# tail
tail(iris)

# we can index the same way we indexed matrices
# e.g. subset rows 1 through 10 and exclude first two columns
iris[1:10,-(1:2)]

# we can extract variables using 
# $ followed by the name of the variable
# e.g. create a new variable that extracts 
# "Species" out of iris
species = iris$Species

# creating data.frames from scratch:
df = data.frame(var1 = c(1, 2, 3), var2 = c("A","B","C"))
df

# equivalently
var1 = c(1, 2, 3)
var2 = c("A","B","C")
df = data.frame(var1, var2)
df

# can rename rows and columns of 
# data.frame with rownames & colnames
rownames(df) = c("Ann", "Bob", "Carol")
df
# adding new variables to a data.frame

# e.g. add var3 below to df using cbind
var3 = c("X","Y","Z")
# cbind
cbind(df,var3)
cbind(var3,df)

# or, we can simply define df$var3
df$var3 = var3
df

# adding a new row to df using rbind
row = c(3,"A",3)
rbind(df,row)
rbind(row, df)

## ------- ##
## Factors ##
## ------- ##

# another way of working with categorical data

# defining factors
fac1 = factor(c("dog","cat","cat","dog"))
fac1
# summary
summary(fac1)
# levels: categories contained in factor
levels(fac1)

## TopHat Exercise
# In the iris dataset,
# how many species of type setosa are there?
species = iris$Species
summary(species)

# read in hsb2 data
hsb2 = read.csv("http://vicpena.github.io/sta9750/spring19/hsb2.csv")

# str, summary, head
str(hsb2)
summary(hsb2)
# levels of ses
levels(ses)

# if you produce summaries, the order will be counterintuitive
# e.g. tabulate ses by race
table(hsb2$ses, hsb2$race)

# reorder levels
hsb2$ses = factor(hsb2$ses, levels = c("low", "middle", "high"))
levels(hsb2$ses) # levels are now in a nice order
table(hsb2$ses, hsb2$race)

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
table(mpg$manufacturer)
# table make by year
table(mpg$manufacturer, mpg$year)

# proportion table of make
# define table first, then use prop.table
tab = table(mpg$manufacturer)
tab
# rounding to 2 dec places
round(prop.table(tab), 2)


# total proportion table of make / year
tab2 = table(mpg$manufacturer, mpg$year)
prop.table(tab2)

# row proportions
prop.table(tab2, 1)

# column proportions
prop.table(tab2, 2)
colSums(prop.table(tab2, 2)) # checking that cols add up to 1

# Tophat Exercises
###################


# What is the maximum value of highway miles 
# per gallon in the mpg dataset?
max(mpg$hwy)
summary(mpg)

# How many cars with manual transmission 
# are there in the mpg dataset?
table(mpg$trans)
58+19

# What % of the cars in the mpg 
# dataset are SUVs?
tab = table(mpg$class)
100*prop.table(tab)
# How many SUVs in the mpg dataset
# are 4-wheel drives?
table(mpg$class, mpg$drv)

# What is the % of Toyotas in the mpg 
# dataset that are SUVs?
tab = table(mpg$manufacturer, mpg$class)
prop2 = round(prop.table(tab, 1),2)
prop2[,7]

