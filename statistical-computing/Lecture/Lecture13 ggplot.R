library(dplyr)
library(ggplot2)
library(babynames)

# ggplot(<data>)

# variables that are in the plot along with their role
# aes(x =, y =, color =, fill = , shape =  , linetype, ...)

# geom_point(), geom_bar(), geom_line(), geom_smooth()... 

# faceting w/ facet.grid

# change labels, title, theme


install.packages('openintro')
library(openintro)

data(hsb2)
View(hsb2)


# display math scores by ses


#############
# Babynames #
#############

# Taylor: gender-neutral

# Plot that shows prop of Taylors by sex as function of time
tay = babynames %>% filter(name == "Taylor")

# colors in rhttp://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

ggplot(tay)+
  aes(x = year, y = prop, color = sex)+
  geom_line()+
  scale_color_manual(name = <name in legend>,
                     values = <colors> ),
                     labels = <names in legend>)


####################
# NFL field goal % #
####################

fg = read.csv("http://vicpena.github.io/sta9750/nflFG.csv")
View(fg)

# More: http://www.cookbook-r.com/Graphs/

###########################
# NYC Italian restaurants #
###########################

rest = read.csv("http://vicpena.github.io/sta9750/spring19/nyc.csv")
str(rest)
