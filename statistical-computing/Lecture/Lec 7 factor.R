##################
## Sorting data ##
##################

data(iris)

# Old R
# ------


# sort: a function that works on vectors

# sort Sepal.Length in increasing order
sort(iris$Sepal.Length)

# sort Species in increasing (alphabetic) order
sort(iris$Species)

# sort Sepal.Length in decreasing order
sort(iris$Sepal.Length, decreasing = T)
#----

# index using order() for data.frames
head(iris)

# sort iris dataset in increasing order by Sepal.Length

# 1. create order
cond = order(iris$Sepal.Length)

# 2. use ordering for indexing
iris[cond,]

# sort iris dataset in decreasing order by Sepal.Length

# 1. create order, decreasing
cond = order(iris$Sepal.Length, decreasing = TRUE)

# 2. use ordering for indexing
iris[cond,]

# sort iris in decreasing order by Species
cond = order(iris$Species)
iris[cond,]

# dplyr
# -----

library(dplyr)

# arrange (sort) iris in increasing order by Sepal.Length
iris %>% arrange(Sepal.Length)
# arrange (sort) iris in desc order by Sepal.Length
iris %>% arrange(desc(Sepal.Length))
# arrange (sort) iris in decreasing order by Species, then in increasing order by Petal.Width
iris %>% arrange(desc(Species),Sepal.Length)

# Type conversions
# ----------------

# femrole data
# http://users.stat.ufl.edu/~winner/data/femrole.txt

# read in femrole
femrole = read.table("http://users.stat.ufl.edu/~winner/data/femrole.dat", header = FALSE)
femrole

# change colnames to something interpretable!
colnames(femrole) = c("personality","role", "friends", "dates", "count")
femrole

# convert variables to factors
femrole$personality = factor(femrole$personality)
femrole$role = factor(femrole$role)
femrole$friends = factor(femrole$friends)
femrole$dates = factor(femrole$dates)

str(femrole)
# change levels to something interpretable
levels(femrole$personality) = c("modern", "traditional")
levels(femrole$role) = c("modern","traditional")
levels(femrole$friends) = c("low", "high")
levels(femrole$dates) = c("low","high")
femrole

# Tophat exercise
# ---------------
# Interfaith dating data
# legend: http://users.stat.ufl.edu/~winner/data/interfaith.txt
# data: http://users.stat.ufl.edu/~winner/data/interfaith.dat

inter = read.table("http://users.stat.ufl.edu/~winner/data/interfaith.dat", header = F)

inter = read.table("http://users.stat.ufl.edu/~winner/data/interfaith.dat")
colnames(inter) = c("ses","religion","gender","interfaith","count")

inter$ses = factor(inter$ses)
levels(inter$ses) =  c("low","middle", "high")
inter$religion = factor(inter$religion)
levels(inter$religion) = c("protestant", "catholic")
inter$gender = factor(inter$gender)
levels(inter$gender) = c("male","female")
inter$interfaith = factor(inter$interfaith)
levels(inter$interfaith) = c("yes", "no")
inter 

# Read it in, change column names, and convert variables to factors
# Then, change levels to something interpretable

# What percentage of catholics are of low socioeconomic status?
catholics = inter %>% filter(religion == "catholic")
catholics

cathlow = inter %>% filter(religion == "catholic"&ses=="low")
100*sum(cathlow$count)/sum(catholics$count)

# What % of protestants are of low socioeconomic status?
prot = inter %>% filter(religion == "protestant")
prot

protlow = inter %>% filter(religion == "protestant"&ses=="low")
protlow
100*sum(protlow$count)/sum(prot$count)

# What percentage of catholics are 
# in an interfaith relationship? 
catholics = inter %>% filter(religion == "catholic")
catholics

cathyes = inter %>% filter(religion == "catholic" & interfaith == "yes")
cathyes
100*sum(cathyes$count)/sum(catholics$count)


# What percentage of protestants are 
# in an interfaith relationship?  
prot = inter %>% filter(religion == "protestant")
prot

protyes = inter %>% filter(religion == "protestant" & interfaith == "yes")
protyes

100*sum(protyes$count)/sum(prot$count)

#########################
# Reformatting datasets #
#########################

# Aggregated data 
# ----------------

install.packages("tidyr")
library(tidyr)
# femrole data
femrole
fem2 = femrole %>% uncount(count)
nrow(fem2)
sum(femrole$count)

# table of ses
table(fem2$personality)

####################
# TopHat exercises #
####################

# Answer the following questions using the interfaith.dat 
# dataset

inter2 = inter %>% uncount(count)
nrow(inter2)
str(inter2)

# What is the percentage of low socioeconomic status
# indivduals in an interfaith relationship?

# What is the percentage of high socioeconomic status
# individuals in an interfaith relationship?

prop.table(table(inter2$ses, inter2$interfaith), 1)


# What is the value of
# (% men in interfaith relationship) - (% women in interfaith relationship)?

prop.table(table(inter2$gender, inter2$interfaith), 1)
0.4055944 - 0.3375796


# Let's consider protestants only. 
# What is the value of
# (% men in interfaith relationship) - (% women in interfaith relationship)?

prot
prop.table(table(inter2$gender, inter2$interfaith),1)
table(prot$gender, prot$interfaith)

meninterfaith = inter2 %>% filter(gender == 'male' & interfaith == 'yes' & religion == 'protestant')
womeninterfaith = inter2 %>% filter(gender == 'female' & interfaith == 'yes' & religion == 'protestant')
sum(meninterfaith$count)/sum(prot$count)
sum(womeninterfaith$count)/sum(prot$count)

# Let's consider catholics only. 
# What is the value of
# (% men in interfaith relationship) - (% women in interfaith relationship)?

catholics
prop.table(table(catholics$gender, catholics$interfaith),1)


