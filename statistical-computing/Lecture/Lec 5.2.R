# Installing libraries
# --------------------

library(ggplot2)

# Plots
# -----
data(mpg)
?mpg
str(mpg)
summary(mpg)

# histogram displ
hist(mpg$cty, main = "City miles per gallon",
        xlim = c(0, 50),
        xlab = "city miles",
        col = rainbow(5))

# can play around with scales, titles, etc

# boxplot displ
boxplot(mpg$cty, main = "City miles per gallon",
     ylim = c(0, 50),
     ylab = "city miles",
     col = "blue")


# boxplot cty by make
boxplot(mpg$cty~mpg$manufacturer, main = "City miles per gallon",
        ylim = c(0, 50),
        ylab = "city miles",
        col = "blue")


# ggplots look a little different
library(ggplot2)
# qplot of displ
qplot(mpg$displ)
# qplot of make
qplot(mpg$manufacturer)

#####################
## Import / export ##
#####################
depression <- read.csv("C:/Users/vPena/Downloads/depression.csv")
class(depression)
# Reading in data 
# ---------------

# It is easy with the import dataset option in Rstudio

# read in depression.csv

# use read.csv function
# http://vicpena.github.io/sta9750/fall18/depression.csv

# read in http://users.stat.ufl.edu/~winner/data/femrole.dat
femrole = read.table("http://users.stat.ufl.edu/~winner/data/femrole.dat")

# Exporting data to *.csv 
# -----------------------

data(iris)
# export iris data to *.csv
write.csv(iris, file = "C:/Users/vPena/Desktop/iris.csv")
# you can change "./" to whichever path is most convenient to you

# save workspace with save.image or save
save.image(file = "C:/Users/vPena/Desktop/Lecture5.RData")
save(depression, femrole, file = "C:/Users/vPena/Desktop/dep.RData")
# can save all or just a subset of workspace

# save all the variables in your workspace

# save only depression and mpg


##########################
## Subsetting variables ##
##########################

data(iris)

# first second and fifth column of iris
iris[,c(2,5)]
# can also subset by telling R to exclude
iris[,-c(2,5)]

# We can use library(dplyr) as well
# install.packages("dplyr")
library(dplyr)

# select creates subsets of variables
# for example, create a subset of iris with
# first, second, and fifth columns
iris %>% select(1, 2, 5)
iris %>% select(5, 1, 2)
iris %>% select(-c(1,2,5))
# order matters: if you want to get variable 5 first in the new dataset

# we can use variable names,
# e.g. Sepal.Length, Sepal.Width, Species
iris %>% select(Sepal.Length, Sepal.Width, Species)

# nb: don't have to type "iris$ " all the time, which is nice

# you can exclude variables too
# e.g. exclude Sepal.Length and Sepal.Width


#####################
## Subsetting rows ##
#####################

# first, 30th, and 50th rows of iris
iris[c(1, 30, 50),]
# in practice, we rarely do this. we usually want to
# get subsets of data that satisfy some condition

# for example, if we want a subset with setosa, 
# we can

# 1. create a logical vector that is 
# TRUE if row is iris$Species is "setosa
cond = iris$Species == "setosa"
cond
# 2. index by the logical vector
iris[cond,]

##
## Logical operators in R
##
# == equal to
# != not equal to
# > greater than
# < less than
# >= greater or equal to
# <=  less than or equal to


# observations with Sepal.Length > 5
sepal = iris$Sepal.Length > 5
iris[sepal,]

# observations where Species is not setosa
cond = iris$Species != "setosa"
iris[cond,]  
cond2 = iris$Species == "setosa"
iris[-cond2,]

## 
## and, or, not in R
##
# & and
# | or
# ! not

# setosas whose Sepal.length is greater than 5
cond = iris$Species == "setosa" & iris$Sepal.Length > 5
iris[cond,]

# setosas or Sepal.length is greater than 5
cond = iris$Species == "setosa" | iris$Sepal.Length > 5
iris[cond,]

# not setosas whose Sepal.Width is less than or equal to 4
cond = iris$Species != "setosa" & iris$Sepal.Width <= 4
iris[cond,]
  
# Using dplyr::filter to do the same thing
# ----------------------------------------

# versicolors whose Sepal.Width is less than or equal to 5
iris %>% filter(Species == "versicolor"  & Sepal.Width <= 5)
# We can combine filter and select

# create a subset that only contains `setosas` 
# and excludes `Species` 
iris %>% filter(Species == "setosa") %>% select(-Species)

## Tophat exercises


hsb2 = read.csv("http://vicpena.github.io/sta9750/spring19/hsb2.csv")
str(hsb2)

# What is the average `math` score in the dataset?
summary(hsb2)
mean(hsb2$math)

# What is the average `math` score 
# for those who scored 50 or greater in `read`? 
read50 = hsb2 %>% filter(read >= 50)
mean(read50$math)
mean(hsb2$math)
# What is the average `read` score in the dataset?
mean(hsb2$read)
# What is the average `read` score for 
# those who scored 50 or greater in `math`? 
cond = hsb2$math > 50
mean(hsb2[cond,]$read)
mean(hsb2$read)
# What is the average difference in `math` scores between individuals
# whose race is `white` and those whose race is not `white`?
cond1 = hsb2$race == 'white'
whitemath=mean(hsb2[cond1,]$math)
cond2 = hsb2$race != 'white'
nonwhite_math=mean(hsb2[cond2,]$math)
nonwhite_math-whitemath

# Consider only those students whose `ses` is `high`.
# What is the average difference in `math` scores between individuals whose race is `white`
# and those whose race is not `white`?

seshigh = hsb2 %>% filter(ses == 'high')
cond1 = seshigh$race == 'white'
whitemath=mean(seshigh[cond1,]$math)
cond2 = seshigh$race != 'white'
nonwhite_math=mean(seshigh[cond2,]$math)
nonwhite_math-whitemath

# What is the percentage of individuals in the sample whose `race` is `white`?

white = hsb2 %>% filter(race == 'white')
summary(hsb2)

# What is the percentage of individuals of high `ses` that are `white`?

seshigh = hsb2 %>% filter(ses == 'high')
summary(seshigh)

#  What percentage of low `ses` students went to `public` schools?

seslow = hsb2 %>% filter(ses == 'low')
summary(seslow)

# What is the percentage of students with a `math` score greater than 50
# who went to `public` schools?

summary(hsb2 %>% filter(math > 50))
summary(hsb2)



##################
## Missing data ##
##################

# missing data are marked with NA


# applying functions to vectors with missing data is tricky

# arithmetic with NAs is NA


# is.na can be used to filter NAs

# complete.cases and na.omit are useful

# load in airquality
?airquality
data(airquality)


# complete.cases: TRUE if there is no missingness at all

# select complete cases

# can also create a subset with incomplete cases

# na.omit

