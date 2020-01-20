# removes all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# creating variables
var1 = 1e-3
var2 = "hello"
var3 = TRUE
var4 = FALSE

# waht is the type of the variables?
class(var1)
class(var2)
class(var3)
class(var4)

# operations w/ variables
var1+var2
var1/var2

# Exercise
var1 = 3
var2 = -3
var3 = 2
var4 = TRUE
var5 = "0"

(var1+var3*var4)/4
var3^var1+var5^2
var1^0+var2^0

(exp(var1)+var3^(1/2))^(2)/(var3+var4)

2+1+TRUE # TRUE is 1

3 - ((var2 - var4)/4)

log(var2, base = 10)/var1

exp((-1)^(1/2) * 3.1415) + 1


# math function
sqrt(4)#4^(1/2)
exp(0)
exp(1)
log(10^2, base=2)
log(exp(3))

sum
