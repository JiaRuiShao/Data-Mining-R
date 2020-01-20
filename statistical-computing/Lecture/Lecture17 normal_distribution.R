#######################
# Normal distribution #
#######################

library(ggplot2)
m = 2
s = 2
x = seq(-10, 10, 0.01)
x
qplot(x = x, y = dnorm(x, mean = m, sd = s), geom = "line") 

?rnorm # random normal values
rnorm(5, mean = 3, sd = 1/10)

pnorm # probabilities P( Normal(mean, sd) <= x )
# P( N(mean = 3, sd = 2) <= 3) 
pnorm(6, mean = 3, sd= 2 )
# P( N( mean = 3, sd = 2) > 3)
pnorm(6, mean = 3, sd= 2, lower.tail = FALSE)



# P( N ( mean = 10, sd = 3) <= 5)
pnorm(5, mean = 10, sd = 3)

# P( N( mean = 5, sd =3) > 4)
pnorm(4, mean = 5, sd = 2, lower.tail = FALSE)
1-pnorm(4, mean = 5, sd = 2)


# quantiles: given q, find x such that P( Normal(mean, sd) < = x) = q
qnorm(0.95, mean = 3, sd = 2)

# normal density: 
# we're not going to use this often... 
# it's what we integrate to find probabilities
dnorm 

# Tophat exercise
# IQ is known to be distributed as Normal(mean = 100, sd = 16).  

# What is the probability that a 
# randomly selected person has an IQ of less than 70?
# P( N < 3) = P(N <= 3)
pnorm(70, mean = 100, sd = 16)*100

# What is the probability that a 
# randomly selected person has an IQ greater than 110?
1-pnorm(110, mean = 100, sd = 16)
pnorm(110, mean = 100, sd = 16, lower.tail = FALSE)

# What is the probability that a 
# randomly selected person has an 
# IQ between 80 and 120?
pnorm(120, mean = 100, sd = 16)-pnorm(80, mean = 100, sd = 16)


# Mensa is a society for "intelligent people"  
# To qualify for Mensa
# ,one needs to be in at least the 
# upper 2% of the population in IQ score.  
# What is the score needed to qualify for Mensa? 
qnorm(0.98, mean = 100, sd = 16)

# Tophat exercise
# Heights of 10 year olds, regardless of gender, closely 
# follow a normal distribution with 
# mean 55 inches and standard deviation 6 inches.

# What is the probability that a 
# randomly chosen 10 year old is shorter than 48 inches?
pnorm(48, mean = 55, sd = 6)

# What is the probability 
# that a randomly chosen 10 year old 
# is between 60 and 65 inches?
pnorm(65, mean = 55, sd = 6) - pnorm(60, mean = 55, sd = 6)

# If the tallest 10% of the class is considered "very tall"
# what is the height cutoff for "very tall"?
qnorm(0.9, mean = 55, sd = 6)

# The height requirement 
# for Batman the Ride at 
# Six Flags Magic Mountain is 54 inches.
# What percent of 10 year olds cannot go on this ride?
pnorm(54, mean = 55, sd = 6)


############
# Binomial #
############

# Binomial(n, p)
# Number of successes in n independent trials
# each trial has probability of success p

# Suppose you roll a die 4 times

# Number of sixes ~ Binomial(4, 1/6)
rbinom # random binomial numbers
pbinom # P(Bin <= k)
qbinom # binomial quantiles
dbinom # P(Bin = k)
?dbinom

# What is the probability that you get exactly 3 sixes?
dbinom(3, size = 5, prob = 1/6)

# What is the probability that you get at least 1 six?
1-dbinom(0, size = 5, prob = 1/6)

# lower.tail = FALSE: P(Bin > k)
# Want: P(Bin >= 1) = P(Bin > 0)
pbinom(0, size = 4, prob = 1/6, lower.tail = FALSE)
1-pbinom(0, size = 4, prob = 1/6)

# Game:
# Flip a coin 4 times
# If the coin shows heads exactly twice, you lose $40
# Otherwise, you win $40
# Would you play?

# Number of heads ~ Binomial(size = 4, prob = 1/2)

# Pr(losing)
dbinom(2, size = 4, prob = 1/2)

# Pr(winning)
1-dbinom(2, size = 4, prob = 1/2)


# A multiple choice exam has 20 questions
# Each question has 3 different answers. Only one is correct.
# You guess the answers at random

# Number of right questions ~ Binomial(size = 20, prob = 1/3)
# What is the probability that you get at least half of them right?
# P(Bin <= k)

# P(Bin >= 10) = 1 - P(Bin < 9)
1-pbinom(9 , size = 20, prob = 1/3)

# A small business is required to test their 72 employees
# for a disease after being exposed to some agent
# The probability that the test gives a false positive is 0.01
# Suppose that all the employees are healthy
# What is the probability that there is at least one false positive?

# Number of false positives ~ Binomial(72, 0.01)

# P(Bin >= 1)
1-pbinom(0, size = 72, prob = 0.01)

###########
# Poisson #
###########

# Model for counts
# Can be equal to 0, 1, 2, ...

# Poisson(lambda)
# lambda is the expected value / long-run mean

# Tophat
# The number of faulty cell phones produced 
# in a factory approximately follows 
# a Poisson distribution with an average of
# 2 faulty phones per day

# dpois: P(Pois = k)
# ppois: P(Pois <= k)
# qpois: quantiles
# rpois: random poisson numbers

# What is the probability that no 
# faulty phones will be produced tomorrow?
ppois(0, lambda = 2)

# Number of defective phones per day ~ Poisson(lambda = 2)
dpois(0, lambda = 2)

# What is the probability that 3 or more faulty
# phones will be produced tomorrow?
# Pr(Poi >= 3) = 1 - Pr(Poi <= 2)
1-ppois(2, lambda = 2)


# A website manager has noticed that
# during the evening hours, about 3 people per 
# minute check out from their shopping cart
# and make an online purchase.
# Use the Poisson model to answer the following
# questions

# Number of purchases in 1 minute ~ Poisson(3)

# What is the probability that in any given minute
# at least one purchase is made?
# P(Poi >= 1)
1-ppois(0, lambda = 3)

# What is the probability that no one 
# makes a purchase in the next 2 minutes?

# Number of purchases in 2 minutes ~ Poisson(6)
dpois(0, lambda = 6)*100
ppois(0, lambda = 6)

# What is the probability that, in the next
# 5 minutes, at least one purchase is 
# made in every minute?
# prob that in any given min, purchase
prob = 1-ppois(0, lambda = 3) 
# Number of minutes with a purchase ~ Bin(5, prob)
dbinom(5, size = 5, prob = prob)
