
# -- #
# if #
# -- #

# if (condition) { 
#   expression
# }

x = 10
if (x > 5) {
  print("x is greater than 5")
}

# ----------------- #
# logical operators #
# ----------------- #

# > : greater than
# >= : greater than or equal to
# < : less than
# <= : less than or equal to
# == : equal
# ! : not
# & : and
# | : or
# %in% : in

# if statement that tells us if the mean of a vector is between 5 and 10
x = 1:6
if ( cond? ) {
  print("The mean of this vector is between 5 and 10")
}

# if statement that tells us if a vector contains "New York" or "NYC"
cities = c("London", "Paris", "Los Angeles", "Milan")
if ( cond? ) {
  print("this vector contains New York or NYC")
}


# ------- #
# if-else #
# ------- #

# if (cond) {
#   expression
# } else {
#   expression
# }

x = 10
if (x > 5) {
  print("x is greater than 5")
} else {
  print("x is not greater than 5")
}

# modify if statement of cities
# so that it prints a message if the vector doesn't contain NYC or New York


# find mean of a vector if it doesn't contain NAs;
# otherwise: print "this vector has missing data"


#------------------#
# if/else if/else  #
#------------------#

# if (condition) {
#   expression 
# } else if (condition) {
#   expression
# } else {
#   expression
# }

x = 4

if (x > 3) {
  print("x is greater than 3")
} else if (x >= 0) { 
  print("x is <= 3, but non-negative")
} else {
  print("x is negative")
}

# Datacamp exercise 

# Suppose you have a spreadsheet 
# where you record your views on LinkedIn 
# and Facebook profiles

# Add code to both control structures such that:
# 1. R prints out "Showing Facebook information" 
# if medium is equal to "Facebook"
# 2. "Your number of views is average" is printed if num_views 
# is between 15 (inclusive) and 10 (exclusive) 

medium = "LinkedIn"
num_views = 14

# Control structure for medium
if (medium == "LinkedIn") {
  print("Showing LinkedIn information")
} else if (medium == "Facebook") {
  # Add code to print correct string when condition is TRUE
  
} else {
  print("Unknown medium")
}

# Control structure for num_views
if (num_views > 15) {
  print("You're popular!")
} else if (num_views <= 15 & num_views > 10) {
  # Add code to print correct string when condition is TRUE
  
} else {
  print("Try to be more visible!")
}

# ----------- #
# while loops #
# ----------- #

# while ( cond ) {
#   expression
# }


x = -3
while (x <= 10) {
  print(x)
  x = x+1
}


# Datacamp exercise

# Code a while loop with the following characteristics:
  
# The condition of the while loop should check if speed is higher than 30.
# Inside the body of the while loop, print out "Slow down!".
# Inside the body of the while loop, decrease the speed 
# by 7 units and assign this new value to speed again.


# Initialize the speed variable
speed = 64

# Code the while loop
while ( ) {
  ___
  ___ = ___
}

# Print out the speed variable
speed


# Flip a coin until you get heads 
?rbinom
rbinom(1, size = 1, prob = 1/2) # 1 == heads; 0 == tails

heads 
while ( ) {
  
}

# Modify it: flip a coin until you get 5 heads


# Draw standard normal random variables until you get a value greater than 2
x = seq(-5, 5, 0.01)
qplot(x = x, y = dnorm(x), geom = "line") + geom_line()


# ----------#
# for loops #
# ----------#

# for ( variable in sequence ) {
#   expression
# }


for ( i in 1:30 ) {
  print(i)
}



cities = c("New York", "London", "Madrid", "Paris")
population = c(8.623, 8.9, 6.55, 2.141)
ncities = length(cities)

# print cities
for (i in 1:ncities) {
  print(cities[i])  
}

# print populations

# print cities & populations
for (i in 1:ncities) {

}


# find expected number of flips until you get heads

# find expected number of flips until you get a std normal draw greater than 5




