###################
# apply functions #
###################


# apply(df, margin, fun)

# apply the function fun to the rows / columns of df 
# rows: margin = 1, column: margin = 2
df = data.frame(a = 1:5, b = c(3,2,1,3,2))
df
rowMeans(df)
apply(df, 1, mean)

# row sd
apply(df, 1, sd)

# column medians
apply(df, 2, median)

# find the minimum for all 
# normalized scores, separately
# (i.e. minimum of rt_norm, imdb_norm, etc.)
library(fivethirtyeight)
data(fandango)
?fandango
library(dplyr)

norm = fandango %>% 
  select(rt_norm, rt_user_norm, metacritic_norm, metacritic_user_nom, imdb_norm)
apply(norm, 2, min)

# for each movie, 
# find its minimum normalized score
apply(norm, 1, min)

# lapply: apply function on elements of a list / vector
# output is a list

l = list(a = 1:5, b = 10:20)
l
lapply(l, min)
# find minimum of elements of the list


x = c(1, 2 , 3 , 4)

# lapply can be used on data.frames as well, 
# but it only operates by column

# sapply: like lapply, but the output is a vector (if possible)
sapply(l, min)

# tapply: similar to group_by and summarize
data(iris)
?iris
str(iris)

# mean petal length by species
tapply(iris$Petal.Length, iris$Species, mean)

iris %>% 
  group_by(Species) %>% 
    summarize(avg = mean(Petal.Length))
