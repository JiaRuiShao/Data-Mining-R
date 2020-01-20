
#####################################
# More than 2 variables in one plot #
#####################################
library(dplyr)
library(ggplot2)
data(diamonds)


# size of dots
# color coding
# faceting

# breaking up numerical into categories,
# and then use faceting


# plot carat (quant), price (quant), cut (qual),
# sizes given by price
qplot(x = carat, y = price, 
      color = cut, facets = cut ~ ., #by column
      data = diamonds)

qplot(x = carat, y = price, 
      color = cut, facets = . ~ cut, #by row
      data = diamonds)

qplot(x = carat, y = price, 
      color = cut, size = price,
      facets = cut ~ color, data = diamonds)

# plot carat, price, cut, color

# carat, price, clarity, color, cut

## depth, carat (weight), price
# transforming the numerical
# variable into ranges 
diamonds$depth

quantile(diamonds$depth)

diamonds$catdepth = cut(diamonds$depth,
    breaks=quantile(diamonds$depth), 
    include.lowest = TRUE)

diamonds$catdepth
View(diamonds$catdepth)

# use it for facetting
qplot(x=carat, y=price,
      facets=.~catdepth, data=diamonds)

################
# grid.arrange #
################

install.packages('gridExtra')
library(gridExtra)

# plot of price
# plot of cut

p1 = qplot(price, data = diamonds)
p2 = qplot(cut, data = diamonds)

# combine them in one figure
# USE par(mfrow=c()) in base R
grid.arrange(p1,p2, ncol = 2)


###################
# library(GGally) #
###################

install.packages('GGally')
install.packages('broom')
library(GGally)

data(mtcars)
?mtcars

cars = mtcars %>% select(mpg, cyl, hp, wt)

# ggpairs
ggpairs(cars)

# corr(corrolation) only measures linear relationships!!

# ggcorr: heatmaps
# https://briatte.github.io/ggcorr/
ggcorr(cars, label = TRUE)

# ggcoef: regression coefficients
mod = lm(mpg~wt+hp+cyl,data=mtcars)
summary(mod)
ggcoef(mod) #if 0, means not important

###############
# More ggplot #
###############
## general procedure

# ggplot(<data>)
# aes(x =, y =, color =, fill = , shape = )
# geom_point(), geom_bar(), 
# change labels, title, theme

install.packages('openintro')
library(openintro)

data(hsb2)
?hsb2
str(hsb2)

# barplot of ses by race
ggplot(hsb2) + 
  aes(x = ses, fill = race) +
  geom_bar() +
  ggtitle("socialeconomic status against racist") +
  xlab("social economic status")

# position = "fill"
ggplot(hsb2) + 
  aes(x = ses, fill = race) +
  geom_bar(position = "fill") +
  ggtitle("socialeconomic status against racist") +
  xlab("social economic status")

# position = "dodge"
ggplot(hsb2) + 
  aes(x = ses, fill = race) +
  geom_bar(position = "dodge") +
  ggtitle("socialeconomic status against racist") +
  xlab("social economic status")

# color-coded scatterplot of reading vs math by ses
ggplot(hsb2) + 
  aes(x = read, y = math, color = ses) +
  geom_point()

# add in trend
ggplot(hsb2) + 
  aes(x = read, y = math, color = ses) +
  geom_point() +
  geom_smooth()

ggplot(hsb2) + 
  aes(x = read, y = math, color = ses) +
  geom_point() +
  geom_smooth(method = "lm")

# math scores by ses
ggplot(hsb2) + 
  aes(x = math, fill = ses) +
  geom_density()

ggplot(hsb2) + 
  aes(x = math, color = ses) +
  geom_density()

ggplot(hsb2) + 
  aes(x = ses, y = math) +
  geom_boxplot()


ggplot(hsb2) + 
  aes(x = math) +
  geom_histogram() +
  facet_grid(ses ~ .)

# math scores by race



# math scores by ses and race

# math scores by gender

# math scores by school type

# babynames data
install.packages("babynames")
library(babynames)

str(babynames)

# colors in R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# create a plot for the name Taylor, 
# broken down by gender/sex
taylor = babynames[babynames$name == "Taylor"]
tay = babynames %>% filter(name == "Taylor")

ggplot(tay) +
  aes(x = year, y = prop, 
      color = sex, linetype = sex) +
  geom_line(size = 1)

# Riley
# Jordan



