#######################
# Coffee shop, part 2 #
#######################

# link: https://vicpena.github.io/sta9750/Coffee%20shop%2C%20part%202.pdf
# normal distribution

weeklycosts = 2500+2000+1750+2500

## In any given week, what is the probability that ##
########## the restaurant is making money? ##########

# weekdaysales ~ Normal(1500, sd = 200)
# weekend sales ~ Normal(1500, sd = 500)

# weekly sales ~ Normal(7*1500, sd = sqrt(5*200^2+2*500^2))

# pnorm: P(Normal <= x)
# with lower.tail = FALSE: P(Normal > x)

# P(Normal > weeklycosts)
pnorm(weeklycosts,
      mean = 7*1500,
      sd = sqrt(5*200^2+2*500^2), 
      lower.tail = FALSE)

## What is the probability that they lose money in December? ##

# weekend days = 9 ~ Normal(1500, 500)
# week days = 31-9 ~ Normal(1500, 200)
# dist of monthly sales in December 
# Normal(31*1500, sd = sqrt((31-9)*200^2+9*500^2))

# 4 weeks and 3 days
# 31 days

daylycosts = weeklycosts/7
monthlycosts = 31*daylycosts
monthlycosts

# P( Normal < monthlycosts)
pnorm(monthlycosts,
      mean = 31*1500,
      sd = sqrt((31-9)*200^2+9*500^2))

## What is the yearly expected profit of the business? ##
######### [Assuming all costs remain constant.] #########

# 52 weeks in a year
yearlycosts = weeklycosts*52
expyearlysales = 7*1500*52
expyearlysales-yearlycosts  
