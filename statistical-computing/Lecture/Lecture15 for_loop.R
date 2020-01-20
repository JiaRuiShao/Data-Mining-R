# create while loop that flips a coin until you get heads
# heads with 1
# tails with 0
rbinom(1, size = 1, prob = 0.5)  # coinflip

# count number of coinflips before getting heads
nflip = 0
heads = 0

while ( heads == 0 ) {
  coinflip = rbinom(1, size = 1, prob = 0.5) 
  nflip = nflip + 1
  # checks whether coinflip was head or tails
  if (coinflip == 1) {
    heads = heads + 1
  } 
}
nflip

# alternative
coinflip = 1
while ( rbinom(1, size = 1, prob = 0.5) != 1 ) {
  coinflip = coinflip + 1
}
coinflip

# ----------#
# for loops #
# ----------#

# for ( variable in sequence ) {
#   expression
# }


for ( i in 1:30 ) {
  print(i)
}

for ( i in c("a","b","c","d") ) {
  print(i)
}

cities = c("New York", "London", "Madrid", "Paris")
population = c(8.623, 8.9, 6.55, 2.141)

# print cities
for (i in 1:length(cities)) {
  print(cities[i])  
}

# print populations
for (i in 1:length(cities)) {
  print(population[i])  
}

# print cities & populations
for (i in 1:length(cities)) {
  print(paste("The population of", cities[i], "is", population[i]))
}


# average number of times you flip a coin until
# you get 5 heads?
nsim = 1e6
flipNum = numeric(nsim)

for (i in 1:nsim) {
  nflip = 0
  heads = 0
  while ( heads < 5 ) {
    coinflip = rbinom(1, size = 1, prob = 0.5) 
    nflip = nflip + 1
    # checks whether coinflip was head or tails
    if (coinflip == 1) {
      heads = heads + 1
    } 
  }
  flipNum[i] = nflip
}

mean(flipNum)


# Men's height (in inches) is approximately normal with mean = 70 and sd = 3
# Suppose you're looking outside your window, seeing people pass by
# On average, how many men will pass by until you see a man
# who's 80 inches tall or taller?
m = 0
s = 1
x = seq(-3, 3, 0.01)
qplot(x = x, y = dnorm(x, mean = m, sd = s), geom = "line") 


?rnorm




# Women's height (in inches) is approximately normal with mean = 64 and sd = 3
# Suppose we pick a woman and a man at random
# what is the probability that the woman is taller than the man?


#############
# functions #
#############


# myfun = function(args) {
#   body
# }


# Input: numeric variable x
# Output: 3 times the value of x
triple = function(x) {
  3*x
}

triple(4)


# Create a function called homeadvantage
# Input: numeric variables homewinperc (home win%) and awaywinperc (away win%)
# Output: 
# - If homewinperc is greater than awaywinperc, print "there's home advantage" 
# - Otherwise, print "there's no home advantage" 

homeadvantage = function(homewinperc, awaywinperc){
  if (homewinperc > awaywinperc)
    print("There's home advantage")
  else
    print("There's no home advantage")
}

homeadvantage(32.3, 50)

# Create a function called flip_until_n
# Input: numeric value n, which is the desired number of heads 
# Output: number of flips until obtaining n heads

flip_until_n = function(n){
  nflip = 0
  heads = 0
  while ( heads < n ) {
    coinflip = rbinom(1, size = 1, prob = 0.5) 
    nflip = nflip + 1
    # checks whether coinflip was head or tails
    if (coinflip == 1) {
      heads = heads + 1
    } 
  }
  nflip
}

flip_until_n(4)
