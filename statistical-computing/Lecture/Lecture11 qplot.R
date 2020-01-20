####################
# left_join review #
####################
band_members
band_instruments

# variables with different names
band_members
band_instruments2

####################
# TopHat exercises #
####################

library(dplyr)

library(tidyr)
install.packages("nycflights13")
library(nycflights13)
airports
flights
airlines

# What is the average delay of flights that departed from EWR, JFK, and LGA?
# group by and summarize 
str(flights)

flights %>% filter(year==2013) %>% group_by(origin) %>% summarize(avgDelay = mean(arr_delay, na.rm = TRUE), 
                                           sdDelay = sd(arr_delay, na.rm = TRUE))

# What airline had the highest average delay time in 2013?

flights %>% group_by(carrier) %>% summarize(avgDelay = mean(arr_delay, na.rm = TRUE)) %>% 
  left_join(airlines, by = "carrier")

#rearrange using SELECT statement
flights %>% group_by(carrier) %>% 
  summarize(avgDelay = mean(arr_delay, na.rm = TRUE)) %>% 
  left_join(airlines, by = "carrier") %>% 
  select(carrier, name, avgDelay) %>% 
  arrange(avgDelay)

# Creating Plots

# create a map with size and color of dots 
# correspond with avg delays
install.packages("ggplot2")
library(ggplot2)
install.packages("maps")
library(maps)
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

###########################
# Intro to ggplot2: qplot #
###########################

library(ggplot2)
data(diamonds)
?diamonds

###############
# For Categorical Data#
###############

# barplot of cut
cutplot = qplot(cut, data = diamonds)

# change x,y label and title
cutplot + 
  xlab("Quality of diamond") +
  ylab("Count") + 
  ggtitle("Barplot of Quality") +
  coord_flip() +
  theme_minimal()

# center the title, change the font
cutplot + 
  xlab("Quality of diamond") +
  ylab("Count") + 
  ggtitle("Barplot of Quality") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10))

cutplot
# ggsave: save ggplots


# ggthemes

# some options:
# xlab, ylab
# ggtitle
# coord_flip
# theme
# theme(plot.title = element_text(hjust = 0.5))
# theme(text=element_text(size=15))

###############
# For Numerical Data#
###############
# histogram of price
histprice = qplot(price, bins = 100, data = diamonds)
histprice + xlim(c(0, 15000))
# can change num of bins

# density plot
qplot(price, geom = "density", data = diamonds)

#############################
### Two Plots at one time###
############################

##############################
# Categorical vs Categorical #
##############################

# cut vs color
qplot(cut, fill = color, data = diamonds) + coord_flip()
qplot(cut, fill = cut, data = diamonds) + coord_flip()

# scale_fill_brewer
# https://ggplot2.tidyverse.org/reference/scale_brewer.html
qplot(cut, fill = cut, data = diamonds) + coord_flip() +
  scale_fill_brewer(palette = "Set2")

###############################
# Categorical vs Quantitative #
###############################

# price vs cut
#stat histogram
qplot(price, fill = cut, data = diamonds)

# price vs cut: density plot
qplot(price, fill = cut, geom = "density", data = diamonds)
qplot(price, color = cut, geom = "density", data = diamonds)

# price vs cut: boxplot 
#facets's like group by in graph
qplot(price, facets = cut ~ .,
      data = diamonds)

qplot(price, facets = . ~ cut,
      data = diamonds)

qplot(arr_delay, facets = orign ~ .,
      data = flights)
qplot(arr_delay, facets = carrier ~ origin,
      data = flights)

# can add color to boxplot

# we can use facets 

################################
# Quantitative vs Quantitative #
################################

# carat v price
qplot(x = carat, y = price, data = diamonds)

# add smoothed trend
qplot(x = carat, y = price, data = diamonds) + geom_smooth()

# force the trend to be linear
qplot(x = carat, y = price, data = diamonds) + geom_smooth(method = 'lm')
