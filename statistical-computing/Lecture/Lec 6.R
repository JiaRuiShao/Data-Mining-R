## Tophat exercises
library(dplyr)

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


# What is the average difference in `math` scores 
# between individuals
# whose race is `white` and those whose race is not `white`?

# Consider only those students whose `ses` is `high`.
# What is the average difference in `math` scores
# between individuals whose race is `white`
# and those whose race is not `white`?

# What is the percentage of individuals 
# in the sample whose `race` is `white`?

# What is the percentage of individuals 
# of high `ses` that are `white`?

#  What percentage of low `ses` students
# went to `public` schools?

# What is the percentage of students with 
# a `math` score greater than 50
# who went to `public` schools?


##################
## Missing data ##
##################

# missing data are marked with NA
x = c(1:5, NA)

# applying functions to vectors with missing data is tricky

# arithmetic with NAs is NA


# is.na can be used to filter NAs

# when working with data.frames
# complete.cases and na.omit are useful 

# load in airquality
?airquality
data(airquality)


# complete.cases: TRUE if there is no missingness at all

# select complete cases

# can also create a subset with incomplete cases

# na.omit(data)


##################
## Sorting data ##
##################

data(iris)

# Old R
# ------

# sort Sepal.Length in increasing order

# sort Species in increasing (alphabetic) order

# sort Sepal.Length in decreasing order

# sort iris dataset in increasing order by Sepal.Length

# sort iris dataset in decreasing order by Sepal.Length

# sort iris in decreasing order by Species, then in increasing order by Petal.Width

# dplyr
# -----

# arrange (sort) iris in increasing order by Sepal.Length

# arrange (sort) iris in desc order by Sepal.Length

# arrange (sort) iris in decreasing order by Species, then in increasing order by Petal.Width


# Type conversions
# ----------------

# femrole data
# http://users.stat.ufl.edu/~winner/data/femrole.txt

# read in femrole
femrole = read.table("http://users.stat.ufl.edu/~winner/data/femrole.dat", header = FALSE)
femrole

# change colnames to something interpretable!

# convert variables to factors

# change levels to something interpretable

# Tophat exercise
# ---------------
# Interfaith dating data
# http://users.stat.ufl.edu/~winner/data/interfaith.txt

# Read it in, change column names
# and convert variables to factors
# Then, change levels to something interpretable

# What % of catholics are of low socioeconomic status?
# What % of protestants are of low socioeconomic status?
# What % of catholics are in an interfaith relationship? 
# What % of protestants are in an interfaith relationship?  


#########################
# Reformatting datasets #
#########################

# Aggregated data 
# ----------------

# femrole data

# annoying format!  

# table of dates?

# uncount

# once you have the data at the individual level
# it's easy to work with it

# table of dates 

# you can go from individual level to aggregate using count

# agreggate by personality, role, friends, date

# aggregate by personality, role

####################
# TopHat exercises #
####################

# Answer the following questions using the interfaith.dat 
# dataset

# What is the percentage of low socioeconomic status
# indivduals in an interfaith relationship?

# What is the percentage of high socioeconomic status
# individuals in an interfaith relationship?

# What is the value of
# (% men in interfaith relationship) - (% women in interfaith relationship)?

# Let's consider protestants only. 
# What is the value of
# (% men in interfaith relationship) - (% women in interfaith relationship)?

# Let's consider catholics only. 
# What is the value of
# (% men in interfaith relationship) - (% women in interfaith relationship)?




