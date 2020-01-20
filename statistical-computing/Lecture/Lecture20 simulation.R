################
# Taxi problem #
################

# A taxi company has divided the city into three regions 
# -- Northside, Downtown, and Southside.

# By keeping track of pickups and deliveries, 
# the company has found that of the fares picked up 

# From Northside,
# 50\% stay in that region, 
# 20\% are taken Downtown,
# and 30\% go to Southside. 

# From Downtown, 
# only 10\% go to Northside,
# 40\% stay Downtown, 
# and 50\% go to Southside. 

# From Southside, 
# 30\% go to each of Northside
# 30 %  Downtown, 
# 40\% stay in Southside.

# If I start Downtown
# What are
# Pr( Downtown )
# Pr( Southside )
# Pr ( Northside )
# after 5 pickups?


# input: current location
# output: random location, with probabilities
#         as specified by the problem statement

randomPickup = function(currentLocation) {
  if (currentLocation == "Northside") {
    # From Northside,
    # 50\% stay in that region, 
    # 20\% are taken Downtown,
    # and 30\% go to Southside. 
    location = sample(x = c("Northside", "Downtown", "Southside"),
                      prob = c(0.5, 0.20, 0.30),
                      size = 1)
    
  } else if (currentLocation == "Downtown") {
    # Downtown, 
    # only 10\% go to Northside,
    # 40\% stay Downtown, 
    # and 50\% go to Southside. 
    location = sample(x = c("Northside", "Downtown", "Southside"),
                      prob = c(0.1, 0.40, 0.50),
                      size = 1)
  } else {
    #  Southside, 
    # 30\% go to each of Northside
    # 30 %  Downtown, 
    # 40\% stay in Southside.
    location = sample(x = c("Northside", "Downtown", "Southside"),
                      prob = c(0.30, 0.30, 0.40),
                      size = 1)
  }
  
  location
}

# Suppose that a cab is currently downtown:
# what is the 
# probability that it is downtown, 
# prob that is northside 
# prob that is southside after 5 pickups?


# 1st step: find a way to
# simulate 5 pickups
# starting downtown

npickups = 5
location = "Downtown"
for (i in 1:npickups) {
  location = randomPickup(location)
}
location

# last step: simulate a bunch of trajectories of 5 pickups
# and then, find probabilities



# Inputs:
# - Initial location
# - Number of pickups

# Output:
# - A vector with probs of being Downtown, Northside, Southside



taxiProblem(initialLocation = "Northside", npickups = 3)

# Pr(Downtown), Pr(Northside), Pr(Southside)
# Starting Northside
# After 100 pickups?


# Suppose that a cab is currently downtown: what is the 
# probability that it is downtown, northside or southside after 100 pickups?

####################
# Gunfight problem #
####################

# Three gunfighters, 
# Alice, Bob, and Carl are entering a three-way duel.  

# Alice (A) hits her target 40 percent of the time
# Bob (B) is not as good a shot and hits his target 30 percent of the time. 
# Carl (C) is wondering how he got into this mess, 
# since he hits hit target only 20 percent of the time. 

# Alice is the best
# Bob 2nd best
# Carl 3rd best / worst one

# On the count of three,
# all three gunfighters draw and shoot 
# one shot at one of the others (simultaneously).  
# As their strategy, each 
# will shoot at the strongest opponent still remaining.  
# If more than one gunfighter survives, the process is repeated. 

#  Find the probability that each gunfighter survives the gunfight.
#  Find the average number of rounds  it takes to finish the duels
#  Find the probability that no one survives

# 1st step: be able to simulate a single
# gunfight

A = 1
B = 1
C = 1 # Alive = 1, Dead = 0
# at least 2 people alive

# print status of gunfighters
A
B
C
round

gunfight = function(A, B, C) {
  
  round = 1
  # Alice hits 40%
  # Bob hits 30%
  # Carl hits 20 %
  while ( A+B+C>=2  ) {
    # all alive
    if ( A == 1 & B == 1 & C == 1) {
      # Alice will shoot Bob
      B = sample(x = c(0, 1) ,
                 prob = c(0.4, 0.6),
                 size = 1)
      # Bob will shoot Alice
      # Carl will shoot Alice
      # Prob that Alice dies 30% (Bob) + 20% (Carl) = 50%
      A = sample(x = c(0, 1) ,
                 prob = c(0.5, 0.5),
                 size = 1)
    }
    # Alice dead, rest alive
    if (A == 0 & B == 1 & C == 1) {
      # Bob is shooting Carl
      C = sample(x = c(0, 1) ,
                 prob = c(0.3, 0.7),
                 size = 1)
      # Carl is shooting Bob
      B = sample(x = c(0, 1) ,
                 prob = c(0.2, 0.8),
                 size = 1)
    }
    # Bob dead, rest alive
    if (A == 1 & B == 0 & C == 1) {
      # Alice shoots Carl
      C = sample(x = c(0, 1) ,
                 prob = c(0.4, 0.6),
                 size = 1)
      # Carl shooting Alice
      A = sample(x = c(0, 1) ,
                 prob = c(0.2, 0.8),
                 size = 1)
    }
    # Carl dead, rest alive
    if (A == 1 & B == 1 & C == 0) {
      # Alice shoots Bob
      B = sample(x = c(0, 1) ,
                 prob = c(0.4, 0.6),
                 size = 1)
      # Bob shooting Alice
      A = sample(x = c(0, 1) ,
                 prob = c(0.3, 0.7),
                 size = 1)
    }
    round = round + 1
    
  }
  
  
  # output: 
  # status of gunfighters
  # rounds
  list(A=A, B=B, C=C, round = round)
}

result = gunfight(A = 1, B = 1, C = 1)
result$A
result$B
result$C
result$round

# 2nd step: simulate many
nsim = 1e5
resA = numeric(nsim)
resB = numeric(nsim)
resC = numeric(nsim)
resround = numeric(nsim)
for (i in 1:nsim) {
  result = gunfight(A = 1, B = 1, C = 1)
  resA[i] = result$A
  resB[i] = result$B
  resC[i] = result$C
  resround[i] = result$round
}
sum(resA)/nsim # how many times she survived
mean(resA) # prob A lives
mean(resB) # prob B lives
mean(resC) # prob C lives
1-(mean(resA)+mean(resB)+mean(resC)) # prob all dead
mean(resround)


