# Installing libraries
# --------------------
install.packages("ggplot2")
library(ggplot2)

# Plots
# -----

# histogram displ

# can play around with scales, titles, etc

# boxplot displ

# boxplot displ by make


# ggplots look a little different
library(ggplot2)
# qplot of displ

# qplot of make

#####################
## Import / export ##
#####################

# Reading in data 
# ---------------

# It is easy with the import dataset option in Rstudio

# read in depression.csv

# use read.csv function

# read in http://users.stat.ufl.edu/~winner/data/femrole.dat

# Exporting data to *.csv 
# -----------------------

data(iris)
# export iris data to *.csv

# you can change "./" to whichever path is most convenient to you

# save workspace with save.image or save
# can save all or just a subset of workspace

# save all the variables in your workspace

# save only depression and mpg


##########################
## Subsetting variables ##
##########################

# first second and fifth column of iris

# can also subset by telling R to exclude

# We can use library(dplyr) as well
install.packages("dplyr")
library(dplyr)

# select creates subsets of variables
# for example, create a subset of iris with
# first, second, and fifth columns

# order matters: if you want to get variable 5 first in the new dataset

# we can use variable names,
# e.g. Sepal.Length, Sepal.Width, Species

# nb: don't have to type "iris$ " all the time, which is nice

# you can exclude variables too
# e.g. exclude Sepal.Length and Sepal.Width


#####################
## Subsetting rows ##
#####################

# first, 30th, and 50th rows of iris

# in practice, we rarely do this. we usually want to
# get subsets of data that satisfy some condition

# for example, if we want a subset with setosa, 
# we can

# 1. create a logical vector that is 
# TRUE if row is iris$Species is "setosa

# 2. index by the logical vector

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

# observations where Species is not setosa

## 
## and, or, not in R
##
# & and
# | or
# ! not

# setosas whose Sepal.length is greater than 5

# not setosas whose Sepal.Width is less than or equal to 4

# Using dplyr::filter to do the same thing
# ----------------------------------------

# versicolors whose Sepal.Width is between 4 and 5

# We can combine filter and select

# create a subset that only contains `setosas` and excludes `Species` 


## Tophat exercises




# What is the average `math` score in the dataset?

# What is the average `math` score for those who scored 50 or greater in `read`? 

# What is the average `read` score in the dataset?

# What is the average `read` score for those who scored 50 or greater in `math`? 


# What is the average difference in `math` scores between individuals
# whose race is `white` and those whose race is not `white`?

# Consider only those students whose `ses` is `high`.
# What is the average difference in `math` scores between individuals whose race is `white`
# and those whose race is not `white`?

# What is the percentage of individuals in the sample whose `race` is `white`?

# What is the percentage of individuals of high `ses` that are `white`?

#  What percentage of low `ses` students went to `public` schools?

# What is the percentage of students with a `math` score greater than 50
# who went to `public` schools?

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

