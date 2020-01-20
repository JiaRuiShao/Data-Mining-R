#################
# What is a CI? #
#################

# Population
# Normal(mean = 2, sd = 1)
# Assume sd = 1 is known, but population mean is unknown


# What is a 95% CI for the unknown population mean?

# https://seeing-theory.brown.edu/frequentist-inference/index.html

# Repeat these 3 steps 100,000 times:
#  1. Simulate a sample with 30 values 
#       from a normal with mean = 2 and sd = 1. 
#  2. Compute 95% confidence interval for the population mean
#  3. Check if it contains the true mean
# When you're done, report the fraction of times the 
# 100,000 intervals contained the true mean.

# Inputs: sample size n

CI = function(n) { 
  #  1. Simulate a sample with n values 
  #       from a normal with mean = 2 and sd = 1. 
  samp = rnorm(n, mean = 2, sd = 1)
  samp
  
  #  2. Compute 95% confidence interval for the population mean
  # sample mean +/- qnorm(1-alpha/2)*sd/sqrt(n)
  # 95% CI: alpha = 0.05
  # 99% CI: alpha = 0.01
  alpha = 0.05
  L = mean(samp) - qnorm(1-alpha/2)*1/sqrt(n)
  U = mean(samp) + qnorm(1-alpha/2)*1/sqrt(n)
  L
  U
  #  3. Check if it contains the true mean
  if ( L <= 2 & 2 <= U ) {
    containstruth = TRUE
  } else {
    containstruth = FALSE
  }
  containstruth
}

# Get 100,000 samples of
# size 30
# For each of them, see if the 
# corresponding CI contained the truth
# Then, give me the fraction of 
# intervals that contained the truth
res = numeric(100000)
for (i in 1:100000) {
  res[i] = CI(30)
}
prop.table(table(res))

########################################
# CI for normal mean, unknown variance #
########################################

# In reality, we never know what the true sd is

# Suppose we draw samples from a normal with unknown
# mean and sd

# We can use the same formula
# we used for the CI with known pop sd
# but substitute pop sd by the sample std deviation, i.e.

#########################################################
# (1-alpha)% CI:                                        #
# sample mean +/- qnorm(1-alpha/2)* sample sd / sqrt(n) #
#########################################################

# again, alpha = 0.05 if we want 95% CI
#        alpha = 0.01 if we want 99% CI
#        etc.


# This approximation gets better as the sample
# size n gets bigger

# Let's test out the accuracy of the approximation

# Write code that allows you to find the fraction of 
# times that the approximate CI with based on a sample
# of size n traps the truth in 100,000 simulations

# Then, answer the following questions:

approxCI = function(n) { 
  # First all, figure out
  # how to find one interval
  # and see if it contains the truth
  
  # 1. Sampling n normal values
  samp = rnorm(n, mean = 2, sd = 1)
  
  # 2. Finding approximate CI
  
  # sample mean +/- qnorm(1-alpha/2)* sample sd / sqrt(n) #
  alpha = 0.05
  L = mean(samp) - qnorm(1-alpha/2)* sd(samp)/sqrt(n)
  U = mean(samp) + qnorm(1-alpha/2)* sd(samp)/sqrt(n)
  
  # 3. Checking whether approximate CI contains truth
  if ( L <= 2 & 2 <= U ) {
    containstruth = TRUE
  } else {
    containstruth = FALSE
  }
  containstruth
}
# How big should n be to have a good approximation?
n = 3 
approxCI(n)

# Get 100,000 samples of
# size 3
# For each of them, see if the 
# corresponding CI contained the truth
# Then, give me the fraction of 
# intervals that contained the truth
res = numeric(100000)
for (i in 1:100000) {
  res[i] = approxCI(100)
}
prop.table(table(res))

# Once you have that,
# write code that does it many times
# so that you can estimate the actual confidence level



# If n isn't very big and the approximation isn't
# very good, what is the problem? Is the fraction 
# of times that we trap the truth bigger or smaller 
# than it should be?

########################################
# Approx CIs via Central Limit Theorem #
########################################

# CLT: averages are roughly normal

# X1, X2, X3, ... , Xn 

# (X1 + X2 + X3 + ... + Xn)/n
# approximately normal mean = population mean
#                       sd = population sd/sqrt(n)


# We can use this fact to find approximate CIs for 
# population means

#########################################################
# (1-alpha)% CI:                                        #
# sample mean +/- qnorm(1-alpha/2)* sample sd / sqrt(n) #
#########################################################

# Can do CI for population means from any 
# distribution using the fact that 
# sample averages are roughly normal
# and the fact that the approximate normal interval
# has the right confidence, provided that the
# sample size is big enough

# Let's test out the accuracy of the approximation
# when the samples come from an Exponential(1)
# distribution


# 1. Figure out how to test whether
# approximate CI contains truth, given exponential data

n = 3
# 1. simulating Exponential(rate = 1) data
samp = rexp(n, rate = 1)

# 2. Finding approximate CI
alpha = 0.05
L = mean(samp) - qnorm(1-alpha/2)* sd(samp)/sqrt(n)
U = mean(samp) + qnorm(1-alpha/2)* sd(samp)/sqrt(n)


# 3. Checking whether it contains truth
# The true mean / expectation of Exponential(1) is 1
# 3. Checking whether approximate CI contains truth
if ( L <= 1 & 1 <= U ) {
  containstruth = TRUE
} else {
  containstruth = FALSE
}
containstruth


approxCIexp = function(n) { 
  # First all, figure out
  # how to find one interval
  # and see if it contains the truth
  
  # 1. Sampling n Exponential(1) values
  samp = rexp(n, rate = 1)
  
  # 2. Finding approximate CI
  
  # sample mean +/- qnorm(1-alpha/2)* sample sd / sqrt(n) #
  alpha = 0.05
  L = mean(samp) - qnorm(1-alpha/2)* sd(samp)/sqrt(n)
  U = mean(samp) + qnorm(1-alpha/2)* sd(samp)/sqrt(n)
  
  # 3. Checking whether approximate CI contains truth
  if ( L <= 1 & 1 <= U ) {
    containstruth = TRUE
  } else {
    containstruth = FALSE
  }
  containstruth
}


# 2. Repeat many times to estimate the confidence
# of the approximate CI

res = numeric(100000)
for (i in 1:100000) {
  res[i] = approxCIexp(50)
}
prop.table(table(res))
library(ggplot2)
qplot(rexp(10000, rate = 1))

# Write code that allows you to find the fraction of 
# times that the approximate CI with based on a sample
# of n Exponential(1) random values traps the truth in 100,000 simulations

# Then, answer the following questions:

# How big should n be to have a good approximation?

# If n isn't very big and the approximation isn't
# very good, what is the problem? Is the fraction 
# of times that we trap the truth bigger or smaller 
# than it should be?

