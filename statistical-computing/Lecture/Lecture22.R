#################
# What is a CI? #
#################

# Population
# Normal(mean = 2, sd = 1)
# Assume sd = 1 is known, but population mean is unknown


# What is a 95% CI for the unknown population mean?

# https://seeing-theory.brown.edu/frequentist-inference/index.html

# Repeat these 3 steps 100,000 times:
#  1. Simulate a sample with 30 values from a normal with mean = 2 and sd = 1. 
#  2. Compute 95% confidence interval for the population mean
#  3. Check if it contains the true mean
# When you’re done, report the fraction of times the 100,000 intervals contained the true mean.

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

# How big should n be to have a good approximation?

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

# sample mean +/- qnorm(1-alpha/2)* sample sd / sqrt(n)


# Let's test out the accuracy of the approximation
# when the samples come from an Exponential(1)
# distribution

# The true mean / expectation of Exponential(1) is 1


# Write code that allows you to find the fraction of 
# times that the approximate CI with based on a sample
# of n Exponential(1) random values traps the truth in 100,000 simulations

# Then, answer the following questions:

# How big should n be to have a good approximation?

# If n isn't very big and the approximation isn't
# very good, what is the problem? Is the fraction 
# of times that we trap the truth bigger or smaller 
# than it should be?

##############################
# Exact CIs for normal means #
##############################

# In the case where the population
# is normal and you don't know 
# the population mean and population sd
# you don't have to use any approximations:
# there is an exact confidence interval

# It is based on the so-called t-distribution.
# You've probably seen this before, but we're not
# going to cover the details

# Given x, which is a vector, for example
x = rnorm(10, mean = 2, sd = 2)
# We can find exact confidence intervals with the t.test function:


# This interval will have 95% confidence even if the sample size
# is small: it's an exact interval

# Compare the performance of the approximate interval with 
# this one if n is small (n can be, say, 3)




