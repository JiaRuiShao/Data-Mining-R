# Class Survey

rm(list=ls())

cs <- read.csv("D:/Baruch/STA 3000 Statistical Computing/STA3000_ Survey (Responses) - Form Responses 1.csv", header=TRUE)

class(cs)

summary(cs)

nrow(cs)

# What is the percentage of students who have prior coding experience?
code_exp=table(cs$Do.you.have.any.prior.coding.experience.)
100*round(prop.table(code_exp), 4)

100*round(mean(cs$Do.you.have.any.prior.coding.experience. == 'Yes'),4)

# What percentage of students is from NYC?
from_nyc=table(cs$Are.you.from.NYC.)
100*round(prop.table(from_nyc), 4)

100*round(mean(cs$Are.you.from.NYC. == 'Yes'),4)

# provide a table that gives the percentage of students who are from 
# Manhattan, Brooklyn, Queens, the Bronx, and Staten Island.
from_mbqbs=table(cs$If.you.are.from.NYC..where.from.)
from_mbqbs_perc=100*round(prop.table(from_mbqbs), 4)
from_mbqbs_perc=from_mbqbs_perc[-1]
from_mbqbs_perc

# create a table that gives the percentage of students who 
# prefer the Yankees, the Mets, or don't care by borough. 
# Do you think that the data indicates that there is some 
# dependence between borough and the baseball team 
# that you prefer?

baseball_borough=table(cs$Which.baseball.team.do.you.prefer.,cs$If.you.are.from.NYC..where.from.)
100*round(prop.table(baseball_borough), 4)
100*round(prop.table(baseball_borough, 1),4)
100*round(prop.table(baseball_borough, 2),4)

# What is the sport that most students seem to care about? 
# Give the percentage of students who 
# have a preference for a football, basketball, and baseball team
100*round(mean(cs$Which.basketball.team.do.you.prefer. != 'I don\'t care'),4)
100*round(mean(cs$Which.baseball.team.do.you.prefer. != 'I don\'t care'),4)
100*round(mean(cs$Which.football.team.do.you.prefer. != 'I don\'t care'),4)

# What percentage of students speak only one language?
install.packages('qdap')

#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_112")

#LD_LIBRARY_PATH=`/usr/libexec/java_home/jre/lib/server`

library(qdap)


cs$How.many.languages.do.you.speak. = tolower(cs$How.many.languages.do.you.speak.)
cs$How.many.languages.do.you.speak. <- as.character(cs$How.many.languages.do.you.speak.)
cs$How.many.languages.do.you.speak.[cs$How.many.languages.do.you.speak. == 'one'] <- '1'
100*round(mean(cs$How.many.languages.do.you.speak. == '1'),4)

# Hair Length

rm(list=ls())

hl = read.table("http://users.stat.ufl.edu/~winner/data/hairlength.dat", header = FALSE)
colnames(hl) = c("HairLength","Age","Count")


hl$HairLength = factor(hl$HairLength)
levels(hl$HairLength) =  c("short","medium", "long")
hl$Age = factor(hl$Age)
levels(hl$Age) = c("14-24", "25-34", "35-49", "50-60")

# What's the percentage of women in the sample that are in each age group???
Age1424 = hl %>% filter(Age == "14-24")
100*round(sum(Age1424$Count)/sum(hl$Count),4)

Age2534 = hl %>% filter(Age == "25-34")
100*round(sum(Age2534$Count)/sum(hl$Count),4)

Age3549 = hl %>% filter(Age == "35-49")
100*round(sum(Age3549$Count)/sum(hl$Count),4)

Age5060 = hl %>% filter(Age == "50-60")
100*round(sum(Age5060$Count)/sum(hl$Count),4)

# What is the percentage of women in the sample 
# who have short, medium and long hair?

short = hl %>% filter(HairLength == "short")
100*round(sum(short$Count)/sum(hl$Count),4)

medium = hl %>% filter(HairLength == "medium")
100*round(sum(medium$Count)/sum(hl$Count),4)

long = hl %>% filter(HairLength == "long")
100*round(sum(long$Count)/sum(hl$Count),4)

# provide a table that shows the percentage of women who 
# have short, medium, and long hair given an age group
hl2 <- hl
hl2 %>% filter(Age == "14-24") %>% hl2$Count/sum(Age1424$Count)

sum(Age1424$Count)
sum(Age2534$Count)
sum(Age3549$Count)
sum(Age5060$CouSnt)


install.packages("tidyr")
library(tidyr)
hl2 = hl %>% uncount(Count)
100*round(prop.table(table(hl2$HairLength, hl2$Age), 1),4)

#--------------not used-------------------------
100*round(prop.table(Age1424$Count),4)
100*round(prop.table(Age2534$Count),4)
100*round(prop.table(Age3549$Count),4)
100*round(prop.table(Age5060$Count),4)

Age1424 <- Age1424[-2] #remove age col
Age1424$Prop <- 100*round(Age1424$Count/sum(Age1424$Count),4)
Age1424 <- Age1424[-2] #remove count col

Age2534 <- Age2534[-2] #remove age col
Age2534$Prop <- 100*round(Age2534$Count/sum(Age2534$Count),4)
Age2534 <- Age2534[-2] #remove count col
Age2534

Age3549 <- Age3549[-2] #remove age col
Age3549$Prop <- 100*round(Age3549$Count/sum(Age3549$Count),4)
Age3549 <- Age3549[-2] #remove count col
Age3549

Age5060 <- Age5060[-2] #remove age col
Age5060$Prop <- 100*round(Age5060$Count/sum(Age5060$Count),4)
Age5060 <- Age5060[-2] #remove count col
Age5060

# out of all the women in the sample who have long hair, 
# what percentage is in the youngest group?
ly <- long %>% filter(Age == "14-24")
100*round(ly$Count/sum(long$Count),4)

# Height, weight, and age of NBA players

rm(list=ls())

nba = read.csv("http://users.stat.ufl.edu/~winner/data/nba_ht_wt.csv")
nba

install.packages('measurements')
library(measurements)

# convert the height variable from inches into meters
nba$Height <- conv_unit(nba$Height, "inch", "m")
nba

# convert the weight variable from pounds into kilograms
nba$Weight <- conv_unit(nba$Weight, "lbs", "kg")
nba

# create a column that contains the body mass index (BMI) 
# of the players

nba$BMI <- nba$Weight/(nba$Height^2)
nba

# which player has the maximum BMI in the sample
nba$Player[apply(nba,2,which.max)$BMI]

# what percentage of NBA players in the sample have a BMI 
# over 25, which is considered "overweight"? 
overweight <- nba %>% filter(BMI > 25)
100*round(nrow(overweight)/nrow(nba),4)

# create a boxplot of height by position
boxplot(Height~Pos,
        data=nba,
        main="NBA Player Height by Position",
        xlab="Player Position",
        ylab="Player Height",
        col="orange",
        border="brown"
)

# create a boxplot of BMI by position
boxplot(BMI~Pos,
        data=nba,
        main="NBA Player BMI by Position",
        xlab="Player Position",
        ylab="Player BMI",
        col="orange",
        border="brown"
)


# others
table(Age1424$HairLength,Age1424$Count)

prop.table(table(Age1424$HairLength,Age1424$Count),1)

hl2 = hl %>% uncount(Count)
hl2

Age1424 %>% group_by(HairLength) %>% tally()

nrow(fem2)
sum(femrole$count)

table(hl$HairLength, hl$Age)
prop.table(table(hl$HairLength, hl$Age),1)
