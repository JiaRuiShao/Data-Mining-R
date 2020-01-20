library(dplyr)
library(ggplot2)
install.packages("tidyr")
library(tidyr)

## A. College admissions dataset

ca = read.csv("http://vicpena.github.io/admin.csv")
str(ca)

# 1. Provide a plot that shows the admission rates for men and women separately. 
# Make sure that your plot has an interpretable title, legend, and axes labels.  

install.packages('openintro')
library(openintro)

install.packages('scales')
library(scales)

# ungroup Freq

ca <- ca %>% uncount(Freq)

# label added
percentdata <- ca %>% group_by(Gender) %>% count(Admit) %>%
  mutate(ratio=scales::percent(n/sum(n)))
percentdata

ggplot(ca) + 
  aes(x = Gender, fill = Admit) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  ylab("Admission Rates") +
  ggtitle("Gender vs Admission Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(data = percentdata, aes(y=n, label=ratio), colour="white", position=position_fill(vjust=0.5)) +
  theme(text=element_text(size=14))
 

# 2. Provide a plot that shows admission rates by department and gender. 
# Make sure that your plot has an interpretable title, legend, and axes labels.  

ggplot(ca) + 
  aes(x = Dept, fill = Gender)  + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent_format()) +
  geom_bar(position = "dodge") +
  ylab("Admission Rates") +
  xlab("Department and Gender") +
  ggtitle("Admission Rates by Department and Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14)) + 
  scale_fill_brewer(palette="Set2")

# aes(y = (..count..)/sum(..count..), 

# vertical stacked bar chart
ggplot(ca) + 
  aes(Dept, Gender, fill = Admit) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~Gender, nrow = 1) +
  ylab("Admission Rates") +
  xlab("Department and Gender") +
  ggtitle("Admission Rates by Department and Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))

# lebel added
percentData <- ca %>% group_by(Dept, Gender) %>% count(Admit) %>%
  mutate(ratio=scales::percent(n/sum(n)))
percentData

# 100% vertical stacked bar chart(--add text and change color schema--)
ggplot(ca) + 
  aes(Dept, Gender, fill = Admit) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  facet_wrap(~Gender, nrow = 1) +
  ylab("Admission Rates") +
  xlab("Department and Gender") +
  ggtitle("Admission Rates by Department and Gender") +
  geom_text(data = percentData, aes(y=n, label=ratio), colour="white", position=position_fill(vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))

ggplot(ca) + 
  aes(Gender, Dept, fill = Admit) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  facet_wrap(~Dept) +
  ylab("Admission Rates") +
  xlab("Department and Gender") +
  ggtitle("Admission Rates by Department and Gender") +
  geom_text(data = percentData, aes(y=n, label=ratio), colour="white", position=position_fill(vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))

# 3. Give a one-paragraph explanation of what you see in the plots in your own words. 
# Make sure that your explanation isn't too technical.  

## B. Salaries and anxiety 

salary = read.csv("http://vicpena.github.io/sta9750/salary.csv")
str(salary)

# 1. Create a figure that displays the relationship between salaries, anxiety, and education levels. 
# Make sure that the labels and the title are interpretable. 
# Interpret in detail the relationships that you see. 

ggplot(salary, aes(x = Education, y = Salary, fill = Education)) +
  geom_boxplot() + 
  xlab("Education Level") + 
  ylab("Salary") +
  ggtitle("Education Level vs Salary") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))+ scale_fill_brewer(palette="Set2")

ggplot(salary) + 
  aes(x = Anxiety, y = Salary, color = Education) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Relationships among salaries, anxiety, and education levels") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))+ scale_fill_brewer(palette="Set2")

install.packages('GGally')
install.packages('broom')
library(GGally)


### Can't work
ggpairs(data=salary, title="Relationships among salaries, anxiety, and education levels",
        mapping=ggplot2::aes(colour = Education),
        lower=list(combo=wrap("facethist",binwidth=1)))

# colored
ggpairs(data=salary, title="Relationships among salaries, anxiety, and education levels",
        mapping=ggplot2::aes(colour = Education))

# monochrome
ggpairs(data=salary)


## C. Call center (simulation)

# 1. Assume that there is a very busy week, where all the days are busy days. 
# Find the expected number of calls they get that week.  

# expected # of customers in a busy week
lambda = 5*6*200
nsim = 1e5
call = rpois(nsim, lambda = lambda) 
mean(call)

# 2. Assume that an operator can take approximately 30 calls per hour. 
# How many operators should they get if they want to make sure that 
# they can handle busy days well? 

nsim = 1e5
sim = rpois(nsim, lambda = 6*200)
round(quantile(sim)/30,0)

# 3. What is the average number of calls they get in a day? 

busy = 6*200
quiet = 6*60

# simulate quiet and busy days
# 1 if busy, 0 if quiet
nsim = 1e5

day = rbinom(nsim, size = 1, prob = 0.2)
tab = table(day)
tab

call_busy = rpois(tab[2], lambda = busy)
call_normal = rpois(tab[1], lambda = quiet)
calls = c(call_busy, call_normal)
mean(calls)

# compare simulation result with the expected # of calls 
0.2*(6*200) + 0.8*(6*60)

# 4. Given what you learned in parts 1 and 3, 
# how many operators would you have in the call center?

# quantiles: given q, find x such that P( Normal(mean, sd) < = x) = q
round(qpois(0.95,lambda = mean(calls))/30,0)
round(qpois(0.95,lambda = busy)/30,0)
round(qpois(0.95,lambda = quiet)/30,0)

average_sim = rpois(nsim, lambda = (0.2*(6*200) + 0.8*(6*60)))
round(quantile(average_sim)/30,0)

busy_sim = rpois(nsim, lambda = busy)
round(quantile(busy_sim)/30,0)

quiet_sim = rpois(nsim, lambda = quiet)
round(quantile(quiet_sim)/30,0)

# 5. What is the average number of calls they will get in the month of December? 

# 31 days in Dec
busy = 31*6*200
quiet = 31*6*60

# simulate quiet and busy days
# 1 if busy, 0 if quiet
nsim = 1e5

day = rbinom(nsim, size = 1, prob = 0.2)
tab = table(day)
tab

call_busy = rpois(tab[2], lambda = busy)
call_normal = rpois(tab[1], lambda = quiet)
calls = c(call_busy, call_normal)
mean(calls)

# compare simulation result with the expected # of calls 
31*(0.2*(6*200) + 0.8*(6*60))

## D. Restaurant (simulation)

# weekday sales ~ Normal(2000, sd = 500)
# weekend sales ~ Normal(3000, sd = 700)

# weekly sales ~ Normal(5*2000+2*3000, sd = sqrt(5*500^2+2*700^2))

# 1. In any given week, what is the probability 
# that the restaurant is making money? 

weeklycosts = 2500+4500+4500+2500

# P(weeklysales > weeklycosts)
pnorm(weeklycosts,
      mean = 5*2000+2*3000,
      sd = sqrt(5*500^2+2*700^2), 
      lower.tail = FALSE)

# 2. What is the probability that they lose money in December? 

# week days = 31-9 ~ Normal(2000, sd = 500)
# weekend days = 9 ~ Normal(3000, sd = 700)

# dist of monthly sales in December 
# Normal((31-9)*2000+9*3000, sd = sqrt((31-9)*500^2+9*700^2))

# 4 weeks and 3 days
# 31 days

daylycosts = weeklycosts/7
monthlycosts = 31*daylycosts
monthlycosts

# P( monthlysales < monthlycosts)
pnorm(monthlycosts,
      mean = (31-9)*2000+9*3000,
      sd = sqrt((31-9)*500^2+9*700^2))


# 3. What is the yearly expected profit of the business? 

# 52 weeks in a year
yearlycosts = weeklycosts*52
expyearlysales = (5*2000+2*3000)*52
expyearlysales-yearlycosts

# 4. The manager is thinking about running a marketing campaign which costs $10000. 
# The probability that the marketing campaign is successful is 80%. 
# If it is successful, average sales on weekdays and weekends 
# will increase by 15% (the standard deviation will stay the same). 
# Would you recommend running the marketing campaign? Why or why not?  

yearlycosts = weeklycosts*52
expyearlysales = (5*2000+2*3000)*52

# 0 is fail, 1 is success
outcome = function(){
  result = sample(x = c(0,1), 
         size = 1, prob = c(0.2,0.8), replace = FALSE)
  result
}

random_simulation = function(marketing_campaign){
  if (marketing_campaign == 1){
    netprofit = expyearlysales*0.15 - 10000
  }
  if (marketing_campaign == 0){
    netprofit = -10000
  }
  netprofit
}

totalearning = 0

for (i in 1:1e5){
  result = outcome()
  earning = random_simulation(result)
  totalearning = totalearning + earning
}

avgearning = totalearning/1e5
avgearning

# double-check
0.8*(expyearlysales*0.15 - 10000)+0.2*(-10000)

## E. Reference

# https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/morecat.html
# https://stackoverflow.com/questions/24776200/ggplot-replace-count-with-percentage-in-geom-bar/48602277
# https://stackoverflow.com/questions/3695497/show-instead-of-counts-in-charts-of-categorical-variables
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
