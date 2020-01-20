##############################
# Exact CIs for normal means #
##############################

# What is a 95% CI for the unknown population mean?

# https://seeing-theory.brown.edu/frequentist-inference/index.html


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
t.test(x, conf.level = 0.99)
t.test(x)$conf.int[1]
t.test(x)$conf.int[2]

# Study the performance of the exact interval 
# if n is small (n can be, say, 3)

# truth is normal (mean = truemean, sd = truesd)

# 1. simulate random numbers

# 2. define function that checks
# whether samples are so that CIs contain true mean

# define function CIcontainstruth
# input: a sample of sample size n
# output: TRUE if CI contains truth, FALSE otherwise

# 3. run the function on the random numbers

# 4. find results

####################
# Hypothesis tests #
####################

# Test
# H0: null hypothesis
# H1: alternative hypothesis

# example: Marketing a new drug
# H0: new drug is inefective
# H1: new drug is effective

# Presumption of "innocence" (innocent is H0)
# Don't reject H0 unless
# there's "overwhelming evidence" in favor of H1

# How to do a hypothesis test 
# at 100*alpha% significance
# (usually alpha = 0.05)

# Starting point
# H0: null
# H1: alternative

# 1. Set significance level alpha
# 2. Compute p-value (with R)
# 3. If p-value is less than alpha -> reject H0
#               isn't less than alpha -> don't reject H0

# Properties of this procedure
# -----------------------------
# In our professional careers, after running many 
# hypothesis tests at the 0.05 significance level,
# we will have wrongly rejected H0 roughly 5% of the time


# One normal mean


# Some accounting firms give the client an option
# to pay a fee when the tax return is completed that 
# guarantees advice and support from the accountant 
# if the client were to be audited.  
# A large accounting firm is trying to determine what fee 
# to charge for next year's returns. In previous 
# years, the actual mean cost to the firm for attending a
# client audit session was $690. 

# To determine if this cost has changed, 
# the firm randomly samples 37 client audit fees. 

audit = read.csv("http://vicpena.github.io/sta9750/audit.csv")
colnames(audit) = c("case","cost")
audit

# plot data
library(ggplot2)
qplot(cost, data = audit)

# find summaries
summary(audit$cost)

# do test and report CI
t.test(audit$cost, mu = 690)

# H0: the average cost hasn't changed
# H1: the average cost has changed


# A tutor wants to know 
# if his one-to-one tutoring 
# for a standardized test is effective 

# The average score in the test is 150. 

# Scores of 160 are considered relatively high,
# and the maximum score is 180

# The tutor only takes students 
# who have taken the test before. 

# He's had 21 students in the past.
tutor = read.csv("http://vicpena.github.io/sta9750/tutor.csv")
tutor = tutor[,-1]
tutor

# plot data
qplot(score, data = tutor)
# find summaries
summary(tutor$score)

# Is there evidence at the 5% confidence 
# level that the tutor improves the scores?
t.test(tutor$score, mu = 0, alternative = "greater")

# Find a 95% confidence interval for the average improvement
# What would you say to the tutor?
t.test(tutor$score)$conf.int






################################
# Two independent normal means #
################################

# A pharmaceutical is interested in knowing whether
# their new treatment is significantly different than 
# the current gold standard. They collected a sample of 40 
# individuals: 20 of them were assigned the new treatment, 
# and 20 of them were assigned the current treatment. 

# The outcome is on an ordinal scale that goes from 0 to 100, 
# where 0 is "bad" and 100 is "great".

# Read in the data:
pharma = read.csv("https://vicpena.github.io/sta9750/fall18/pharma.csv")
pharma

# plot data
qplot(outcome, data = pharma) +
  facet_grid(group ~ .)


qplot(outcome, data = pharma) +
  facet_grid(. ~ group)


# find summaries by group
library(dplyr)
pharma %>% group_by(group) %>%
    summarize(mean(outcome), sd(outcome))

# The pharmaceutical is interested in knowing whether
# there is evidence to claim that the population means of the 
# health outcomes are different at the 0.01 significance level.
t.test( pharma$outcome ~ pharma$group , alternative = "less" )
# Would you market the drug?

