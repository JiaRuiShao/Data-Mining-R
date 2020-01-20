### Intermdiate R

Reference: https://rstudio-pubs-static.s3.amazonaws.com/442144_429e9d1343db42b3a86be5246b722058.html

## Relational Operators

# equality '=='
TRUE == TRUE
TRUE == FALSE
2==3

# Compare a logical with a numeric
TRUE == 1

# Inequality
TRUE != TRUE
TRUE != FALSE
2!=3


# '<' and '>'
3<5
# alphabetical order
"Hello" > "Goodbye"

# Comparison of character strings
"raining" <= "raining dogs"

# Comparison of logicals
TRUE > FALSE

# keep in mind that TRUE is treated as 1 for arithmetic, 
# and FALSE is treated as 0. Therefore, FALSE < TRUE is TRUE.

## Relational Operators and Vectors

# The linkedin and facebook vectors have already been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

# Popular days
linkedin > 15

# Quiet days
linkedin <= 5

# LinkedIn more popular than Facebook
linkedin > facebook

## Logical Operators
# The linkedin and last variable are already defined for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
last <- tail(linkedin, 1)

# Is last under 5 or above 10?
last < 5 | last > 10

# Is last between 15 (exclusive) and 20 (inclusive)?
last > 15 & last <20

## Conditional Statement
'''
if (condition) {
  expr
}

if (condition) {
  expr1
} else {
expr2
}

if (condition1) {
  expr1
} else if (condition2) {
  expr2
} else if (condition3) {
  expr3
} else {
  expr4
}
'''

## While loop

'''
while (condition) {
  expr
}
'''

## for loop

primes <- c(2, 3, 5, 7, 11, 13)

# loop version 1
for (p in primes) {
  print(p)
}

# loop version 2
for (i in 1:length(primes)) {
  print(primes[i])
}

## Introduction to Functions

?mean

mean(x, trim = 0, na.rm = FALSE, ...)

# The linkedin and facebook vectors have already been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

# Calculate the mean of the sum
avg_sum<-mean(linkedin+facebook)

# Calculate the trimmed mean of the sum
avg_sum_trimmed<-mean(linkedin+facebook,trim=0.2)

# Inspect both new variables
avg_sum
avg_sum_trimmed

# Create a function pow_two()
pow_two <- function(x) {
  y<- x^2
  return(y)
}

# Use the function
pow_two(12)

# Create a function sum_abs()
sum_abs <- function(w,x) {
  y<-abs(w)+abs(x)
  return(y)
}

# Use the function
sum_abs(-2,3)

# Finish the pow_two() function
pow_two <- function(x, print_info=TRUE) {
  y <- x ^ 2
  if (print_info==TRUE){
    print(paste(x, "to the power two equals", y))}
  return(y)
}
pow_two(3)

## Load an R Package

# Load the ggplot2 package
library(ggplot2)

# Retry the qplot() function
qplot(mtcars$wt, mtcars$hp)

# Check out the currently attached packages again
search()

# Chunk 1
library(data.table)
require(rjson)

# Chunk 2
library("data.table")
require(rjson)

## The apply family

# Lappy
# The vector pioneers has already been created for you
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

# Split names from birth year
split_math <- strsplit(pioneers, split = ":")

# Convert to lowercase strings: split_low
split_low <-lapply(split_math, tolower)


# Take a look at the structure of split_low
str(split_low)


# Code from previous exercise:
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Write function select_first()
select_first <- function(x) {
  x[1]
}

# Apply select_first() over split_low: names
names <- lapply(split_low, select_first)

# Write function select_second()
select_second <- function(x) {
  x[2]
}

# Apply select_second() over split_low: years
years <- lapply(split_low, select_second)

# Transform: use anonymous function inside lapply
names <- lapply(split_low, function(x) {x[1]})

# Transform: use anonymous function inside lapply
years <- lapply(split_low, function(x) {x[2]})

# Use lapply with additional arguments
# Definition of split_low
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Generic select function
select_el <- function(x, index) {
  x[index]
}

# Use lapply() twice on split_low: names and years
names <- lapply(split_low, select_el, index = 1)
years <- lapply(split_low, select_el, index = 2)
names

# temp has already been defined in the workspace
temp_1<-c(3, 7, 9, 6, -1)
temp_2<-c(6, 9, 12, 13, 5)
temp_3<-c(4, 8, 3, -1, -3)
temp_4<-c(1, 4, 7, 2, -2)
temp_5<-c(5, 7, 9, 4, 2)
temp_6<-c(-3, 5, 8, 9, 4)
temp_7<-c(3, 6, 9, 4, 1)
temp<-c(temp_1,temp_2,temp_3,temp_4,temp_5,temp_6,temp_7)
temp

# Create a function that returns min and max of a vector: extremes
extremes <- function(x) {
  c(min = min(x), max = max(x))
}

# Apply extremes() over temp with sapply()
sapply(temp, extremes)

# Apply extremes() over temp with lapply()
lapply(temp, extremes)

# Definition of below_zero()
below_zero <- function(x) {
  return(x[x < 0])
}

# Apply below_zero over temp using sapply(): freezing_s
freezing_s<-sapply(temp,below_zero)

# Apply below_zero over temp using lapply(): freezing_l
freezing_l<-lapply(temp,below_zero)

# Are freezing_s and freezing_l identical?
identical(freezing_s,freezing_l)

## vapply
# apply function over list or vector
# output = list
lapply()

# apply function over list or vector
# try to simplify list to array
sapply()

# apply function over list or vector
# explicitly specify output format
vapply()

# Definition of basics()
basics <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x))
}

# Apply basics() over temp using vapply()
vapply(temp, basics, FUN.VALUE=numeric(3))

# Practice:
# Convert the sapply call to vapply
vapply(logs, length, integer(1))

# Convert the sapply call to vapply
vapply(logs, `[[`, "success", FUN.VALUE = logical(1))

# Convert the sapply() call to a vapply() or lapply() call
vapply(logs, `[[`, c("details", "message"), FUN.VALUE=character(1))

# Convert the sapply() call to a vapply() or lapply() call
lapply(logs, function(x) { x$details })


# Return vector with uppercase version of message elements in log entries
extract_caps <- function(x) {
  toupper(x$details$message)
}

# Apply your function to logs and print the result
result <- vapply(logs, extract_caps, FUN.VALUE=character(1))

result


# Utilities

'''
R features a bunch of functions to juggle around with data structures::
  
seq(): Generate sequences, by specifying the from, to, and by arguments.
rep(): Replicate elements of vectors and lists.
sort(): Sort a vector in ascending order. Works on numerics, but also on character strings and logicals.
rev(): Reverse the elements in a data structures for which reversal is defined.
str(): Display the structure of any R object.
append(): Merge vectors or lists.
is.*(): Check for the class of an R object.
as.*(): Convert an R object from one class to another.
unlist(): Flatten (possibly embedded) lists to produce a vector.
'''

# The linkedin and facebook lists have already been created for you
linkedin <- list(16, 9, 13, 5, 2, 17, 14)
facebook <- list(17, 7, 5, 16, 8, 13, 14)

# Convert linkedin and facebook to a vector: li_vec and fb_vec
li_vec<-unlist(linkedin)
fb_vec<-unlist(facebook)

# Append fb_vec to li_vec: social_vec
social_vec<-append(li_vec,fb_vec,after=length(li_vec))

# Sort social_vec
sort(social_vec, decreasing=TRUE)

'''
grepl(), which returns TRUE when a pattern is 
found in the corresponding character string.
grep(), which returns a vector of indices of 
the character strings that contains the pattern.
'''
# The emails vector has already been defined for you
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org",
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

# Use grepl() to match for "edu"
grepl(pattern="edu", x=emails)

# Use grep() to match for "edu", save result to hits
hits<-grep(pattern="edu", x=emails)
hits

# Subset emails using hits
emails[hits]

# sub and gsub
# The emails vector has already been defined for you
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "global@peace.org",
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

# Use sub() to convert the email domains to datacamp.edu
sub(pattern="@.*\\.edu$", replacement="@datacamp.edu",x=emails)

awards <- c("Won 1 Oscar.",
            "Won 1 Oscar. Another 9 wins & 24 nominations.",
            "1 win and 2 nominations.",
            "2 wins & 3 nominations.",
            "Nominated for 2 Golden Globes. 1 more win & 2 nominations.",
            "4 wins & 1 nomination.")

sub(".*\\s([0-9]+)\\snomination.*$", "\\1", awards)

## Times and dates
# Get the current date: today
today<-Sys.Date()
today

# See what today looks like under the hood
unclass(today)

# Get the current time: now
now<-Sys.time()
now

# See what now looks like under the hood
unclass(now)

# as.Date()
as.Date(str, format = "%b %d, '%y")


'''
%Y: 4-digit year (1982)
%y: 2-digit year (82)
%m: 2-digit month (01)
%d: 2-digit day of the month (13)
%A: weekday (Wednesday)
%a: abbreviated weekday (Wed)
%B: month (January)
%b: abbreviated month (Jan)
'''

# Definition of character strings representing dates
str1 <- "May 23, '96"
str2 <- "2012-03-15"
str3 <- "30/January/2006"

# Convert the strings to dates: date1, date2, date3
date1 <- as.Date(str1, format = "%b %d, '%y")
date2 <- as.Date(str2)
date3 <- as.Date(str3, format = "%d/%B/%Y")

# Convert dates to formatted strings
format(date1, "%A")
format(date2, "%d")
format(date3, "%b %Y")

# as.POSIXct()

?strptime

'''
%H: hours as a decimal number (00-23)
%I: hours as a decimal number (01-12)
%M: minutes as a decimal number
%S: seconds as a decimal number
%T: shorthand notation for the typical format %H:%M:%S
%p: AM/PM indicator
'''

# Definition of character strings representing times
str1 <- "May 23, '96 hours:23 minutes:01 seconds:45"
str2 <- "2012-3-12 14:23:08"

# Convert the strings to POSIXct objects: time1, time2
time1 <- as.POSIXct(str1, format = "%B %d, '%y hours:%H minutes:%M seconds:%S")
time2 <- as.POSIXct(str2)

# Convert times to formatted strings
format(time1,"%M")
format(time2,"%I:%M %p")

## Titanic  Practice

# Import titanic from csv
titanic <- read.csv("titanic.csv")

# Call dim on titanic
dim(titanic)

# Generate histogram of Age column
hist(titanic$Age)

# Print out total value of fares
sum(titanic$Fare)

# Print out proportion of passengers that survived
mean(titanic$Survived)


# Extract the name column from titanic
pass_names <- titanic$Name

# Create the logical vectror is_man
is_man <- grepl(", Mr\\.", pass_names)

# Count the number of men
sum(is_man)

# Count number of men based on gender
sum(titanic$Sex == "male")


# Extract the name column from titanic
pass_names <- titanic$Name

# Create titles
titles <- gsub("^.*, (.*?)\\..*$", "\\1", pass_names)

# Call unique() on titles
unique(titles)


# Create variables for passenger names and titles
pass_names <- titanic$Name
titles <- paste(",", c("Mr\\.", "Master", "Don", "Rev", "Dr\\.", "Major", "Sir", "Col", "Capt", "Jonkheer"))

# Finish the vapply() command
hits <- vapply(titles,
               FUN = grepl,
               FUN.VALUE = logical(length(pass_names)),
               pass_names)

# Calculate the sum() of hits
sum(hits)

# Count number of men based on gender
sum(titanic$Sex == "male")


# Finish the convert_name() function
convert_name <- function(name) {
  # women: take name from inside parentheses
  if (grepl("\\(.*?\\)", name)) {
    gsub("^.*?\\((.*?)\\)$", "\\1", name)
    # men: take name before comma and after title
  } else {
    # Finish the gsub() function
    gsub("^(.*?),\\s[a-zA-Z\\.]*?\\s(.*?)$", "\\2 \\1", name)
  }
}

# Call convert_name on name
clean_pass_names <- vapply(pass_names, FUN = convert_name,
                           FUN.VALUE = character(1), USE.NAMES = FALSE)

# Print out clean_pass_names
print(clean_pass_names)

# Have a look at head() of dob1 and dob2
head(dob1)
head(dob2)

# Convert dob1 to dob1d, convert dob2 to dob2d
dob1d <- as.Date(dob1, '%Y-%m-%d')
dob2d <- as.Date(dob2, '%B %d, %Y')

# Combine dob1d and dob2d into single vector: birth_dates
birth_dates <- c(dob1d,dob2d)

# titanic, dob1 and dob2 are preloaded
dob1d <- as.Date(dob1)
dob2d <- as.Date(dob2, format = "%B %d, %Y")
birth_dates <- c(dob1d, dob2d)
disaster_date <- as.Date("1912-04-15")

# Add birth_dates to titanic (column Birth)
titanic$Birth <- birth_dates
str(titanic)

# Create subset: survivors
survivors <- subset(titanic, Survived == TRUE)

# Calculate average age of survivors
mean(disaster_date - survivors$Birth, na.rm=T)




