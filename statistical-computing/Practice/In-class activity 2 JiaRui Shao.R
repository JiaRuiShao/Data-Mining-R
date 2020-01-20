# STA 3000 In-class activity 2

library(dplyr)
library(ggplot2)

install.packages('GGally')
install.packages('broom')
library(GGally)

install.packages('gridExtra')
library(gridExtra)

## I. Gapminder

install.packages("gapminder")
library(gapminder)
data(gapminder)

?gapminder

# Create a figure that shows the relationship between 
# the continent, year, life expectancy, population, 
# and GDP per capita

str(gapminder)

# population per continent over years
p1=qplot(x = year, y = pop, 
      color = continent, facets = . ~ continent,
      data = gapminder)+ 
  xlab("Year") +
  ylab("Population") + 
  ggtitle("Population Per Continent Over Years") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=12))


# ggpairs
continent = gapminder$continent
gm = gapminder %>% select(year, lifeExp, pop, gdpPercap)

ggpairs(gm, title = "Relationship between the continent, years, life expectancy, population and GDP per capita", ggplot2::aes(colour=continent))

## II. Italian Restaurants in NYC

itlRest = read.csv("http://vicpena.github.io/sta9750/spring19/nyc.csv")

str(itlRest)

# 1. Create a figure that contains plots for 
# all the pairs of variables in the dataset, 
# except Case (i.e., a figure that contains 
# plots for Restaurant vs Price, Food vs Price, Decor vs Service, etc.). 

pairs(itlRest[2:7], cex.labels=2)

# 2. Provide a heatmap for the correlation between 
# the numerical variables in the dataset

numeric = itlRest %>% select (3,4,5,6)
ggcorr(numeric, label = TRUE)

# 3. Find 2 examples of cheap restaurants that have relatively 
# good food and 2 examples of expensive restaurants that have relatively bad food

itlRest$Food = cut(itlRest$Food,
                        breaks=quantile(itlRest$Food), 
                        include.lowest = TRUE)

ggplot(itlRest) + 
  aes(x = Food, y = Price) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggtitle("Food vs Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=13))


itlRest %>%
  select(Food, Price, Restaurant) %>%
  arrange(Food, Price)

# 4. Suppose you're going on a date and want to use the information in this dataset 
# to pick where to go. Assume your budget is at most $40. 
# Assuming that you can get a table anywhere you want, where would you go and why? 

Price <- (itlRest$Price - mean(itlRest$Price))/sd(itlRest$Price)
Food <- (itlRest$Food - mean(itlRest$Food))/sd(itlRest$Food)
Decor <- (itlRest$Decor - mean(itlRest$Decor))/sd(itlRest$Decor)
Service <- (itlRest$Service - mean(itlRest$Service))/sd(itlRest$Service)
itlRest$rating = (Price+Food+Decor+Service)/4

itlRest %>%
  filter(Price <= 40) %>%
  arrange(desc(rating)) %>%
  select(Restaurant, rating) %>%
  top_n(1)

# 5. Create a figure that displays the relationship between price, food, decor, service, 
# and the East / West indicator

ggpairs(itlRest, columns = c(3:6), title = "Relationships between price, food, decor, service, and the East / West indicator", ggplot2::aes(colour=itlRest$East))


## III. Interfaith dating data

install.packages("tidyr")
library(tidyr)
library(dplyr)

intf = read.table("http://users.stat.ufl.edu/~winner/data/interfaith.dat")

colnames(intf) <- c("Socio_Economic_Class", "Religion", "Gender", "Interfaith_dating", "Count")
str(intf)

intf$Socio_Economic_Class = factor(intf$Socio_Economic_Class)
levels(intf$Socio_Economic_Class) =  c("low","middle", "high")

intf$Religion = factor(intf$Religion)
levels(intf$Religion) =  c("Protestant","Catholic")

intf$Gender = factor(intf$Gender)
levels(intf$Gender) =  c("Male","Female")

intf$Interfaith_dating = factor(intf$Interfaith_dating)
levels(intf$Interfaith_dating) =  c("Yes","No")


intf <- intf %>% uncount(Count)

ggpairs(intf, switch = "both", title = "Relationship between socioeconomic class, religion, gender, and interfaith dating")

