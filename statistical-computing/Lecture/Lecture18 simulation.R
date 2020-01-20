###############
# Coffee shop #
###############

# Problem statement:
# https://docs.google.com/document/d/1KmYF9oh6k2S_haf1pLrXIdLwPENxKn1ReqSbGEwaA8Q/edit?usp=sharing
# Possion Distribution

customers = c(62, 55, 43, 42, 30, 23)
# average # of customers in a busy week(work 5 days per week)
5*sum(customers)

# lambda: expected value
# expected # of customers in a busy week
lambda = 5*sum(customers)
nsim = 1e5
randomdemand = rpois(nsim, lambda = lambda) 
mean(randomdemand)

# customers at 8am-9am
# in a busy day
nsim = 1e3
sim = rpois(nsim, lambda = 62)
hist(sim)
quantile(sim)

################################################################
## What is the average number of customers they get in a day? ##
################################################################

# avg number of cust
# in a day
busy = c(62, 55, 43, 42, 30, 23)
sum(busy) # expected cust. busy
normal = c(45, 40, 35, 33, 27, 16)
sum(normal) # expected cust. normal
0.80*sum(normal)+0.20*sum(busy)

# simulate normal and busy days
# 1 if busy, 0 if normal
nsim = 1e5
# simulate whether days are busy or not
day = rbinom(nsim, size = 1, prob = 0.2)
tab = table(day)
tab

demand_busy = rpois(tab[2], lambda = sum(busy))
demand_normal = rpois(tab[1], lambda = sum(normal))
demands = c(demand_busy, demand_normal)
mean(demands)

###################
# Sample function #
###################

# simulate rolling a die
sample(x = c("a","b","c","d","e","f"), 
       size = 10, prob = rep(1/6, 6), replace = TRUE)

rep(1/6, 6)
rep("a", 4) # repeating a value a given number of times

# simulating outcome of a game where 
# a team has a 30% prob of winning,
# 30% of drawing, and 40% of losing

sample(x = c("winning", "draw", "losing"),
       size = 1, 
       prob = c(0.3, 0.3, 0.4),
       replace = FALSE)


#############
# SIR model #
#############

# A population of 100 people 
# are exposed to an infectious disease
# that you can have only once
# Every day,

# Prob( susceptible ->  susceptible ) = 0.8 
# Prob( susceptible -> infected ) = 0.2
# Prob( susceptible -> recovered ) = 0 
# Prob( infected -> susceptible ) = 0
# Prob( infected -> infected ) = 0.75
# Prob( infected -> recovered ) = 0.25
# Prob( recovered -> recovered ) = 1

# Assume that, on day 1,
# everyone is susceptible

# What is the expected number of days until
# everyone is recovered?

# We need to be able
# to simulate the number of days until full recovery
# for a population of 100 people

S = 100 # susceptible
I = 0 # infected
R = 0 # recovered people

day = 1

# simulate states / health outcomes
# until everyone recovers 

while ( R != 100 ) {
  # simulate outcomes for susceptible individuals
  simS = sample(x = c("S", "I", "R"),
         size = S, 
         prob = c(0.8, 0.2, 0),
         replace = TRUE)

  
  # simulate outcomes for infected individuals
  simI = sample(x = c("S", "I", "R"),
                size = I, 
                prob = c(0, 0.75, 0.25),
                replace = TRUE)
  
  # simulate outcomes for recovered individuals
  simR = sample(x = c("S", "I", "R"),
                size = R, 
                prob = c(0, 0, 1),
                replace = TRUE)
  
  # combine the outcomes
  sim = c(simS, simI, simR)
  
  # count how many people are susc.
  S = sum(sim=="S")
  # count how many people are inf.
  I = sum(sim=="I")
  # count how mane people are recovered
  R = sum(sim=="R")
  
  # update the number of days
  day = day + 1
}
day

# 