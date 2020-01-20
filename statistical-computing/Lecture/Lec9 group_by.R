
# utility functions for selecting and mutating
# --------------------------------------------

# start_with #
# ends_width #
# contains #


# iris data
data(iris)

# select variables that have "Sepal" information
# select(starts_width())

# select variables that have "Width" information
# ends_width

# num_range #

# prefix + number
# e.g. V1, V2, V3, ... 
# select(num_range("V", <range>))
femrole = read.table("http://users.stat.ufl.edu/~winner/data/femrole.dat")
# femrole data
fem_scramble = femrole %>% select(2,3,1,5,4)
fem_scramble

# select variables V1, V2, V3, V4
# select(num_range)


##
## Summaries of variables by different categories
## group_by() and summarize()
##

# back to hsb2 dataset

# get mean, median, and sd of math score by race


# get table of how many students scored > 70 in math by race



####################
# TopHat Exercises #
####################

install.packages("gapminder")
library(dplyr)
library(gapminder)
data("gapminder")
str(gapminder)
?gapminder

head(gapminder)
# what was the average life expectancy in 
# Africa in 1952?

summary(gapminder)

gapminder %>% filter(year == 2007 & continent == 'Africa')  %>%
  summarize(avg = mean(lifeExp, na.rm = T))


# what was the average life expectancy 
# in Africa in 2007?

# which continent experienced the highest %
# increase in life expectancy in the 1952-2007 
# period?


# what is the maximum gdp per capita
# in Africa in 2007? (in $ amount, not country)
# what is the maximum gdp per capita 
# in Europe in 2007?


gapminder %>% filter(year == 2007 & continent == 'Africa')  %>%
  arrange(desc(gdpPercap)) %>% top_n(1)

gapminder %>% filter(year == 2007 & continent == 'Europe')  %>%
  arrange(desc(gdpPercap)) %>% top_n(1)

# id countries

# what % of countries had a population 
# of more than 50 million in Asia in 2007?

a=nrow(gapminder %>% filter(year == 2007 & continent == 'Asia' & pop > 50000000))
b=nrow(gapminder %>% filter(year == 2007 & continent == 'Asia'))
a/b


# what % of countries had a population 
# of more than 50 million in Europe in 2007?

a=nrow(gapminder %>% filter(year == 2007 & continent == 'Europe' & pop > 50000000))
b=nrow(gapminder %>% filter(year == 2007 & continent == 'Europe'))
a/b


