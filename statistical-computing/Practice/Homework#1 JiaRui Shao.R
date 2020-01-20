
library(dplyr)
library(tidyr)
library(ggplot2)

install.packages('fivethirtyeight')
library(fivethirtyeight)

data(candy_rankings)
?candy_rankings
str(candy_rankings)

#######################################################
###### The Ultimate Halloween Candy Power Ranking #####
#######################################################

#1. Find the top 5 best rated and top 5 worst rated candy. 

# top 5 worst rated candy
candy_rankings %>% 
  group_by(competitorname) %>% 
  summarize(winpercent) %>% 
  arrange(winpercent) %>%
  top_n(-5)

# top 5 best rated candy
candy_rankings %>% 
  group_by(competitorname) %>% 
  summarize(winpercent) %>% 
  arrange(desc(winpercent)) %>%
  top_n(5)

# 2. Plot winpercent against sugarpercent. Do you see any association? 
# Now, plot winpercent against pricepercent. Do you see any association? 

qplot(x = winpercent, y = sugarpercent, data = candy_rankings) +
  geom_smooth() + 
  ggtitle("winpercent vs. sugarpercent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10))

qplot(x = winpercent, y = pricepercent, data = candy_rankings) +
  geom_smooth() + 
  ggtitle("winpercent vs. pricepercent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10))

# 3. Consider all the logical-type variables in the dataset. 
# For each logical variable, find the average difference in winpercent 
# between the treats that satisfy the condition and 
# the treats that don't satisfy it. 
# Which logical variable seems to have the strongest effect on winpercent? 

str(candy_rankings)

candy_rankings %>%
  group_by(chocolate) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(fruity) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(caramel) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(peanutyalmondy) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(nougat) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(crispedricewafer) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(hard) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(bar) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(pluribus) %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

candy_rankings %>%
  group_by(chocolate, fruity, caramel, peanutyalmondy, nougat, crispedricewafer, hard, bar, pluribus) %>%
  ungroup %>%
  summarize(avgWinPerc = mean(winpercent, na.rm = TRUE))

#   group_by(chocolate, fruity, caramel, peanutyalmondy, nougat, crispedricewafer, hard, bar, pluribus)


######################################################
############# College admissions dataset #############
######################################################

colAdm = read.csv("http://vicpena.github.io/admin.csv")
str(colAdm)

# 1. Find the percentage of men who applied and got in and 
# the percentage of women who applied and got in. What do you see? 
install.packages("tidyr")
library(tidyr)
library(dplyr)
colAdm2 = colAdm %>% uncount(Freq)
100*round(prop.table(table(colAdm2$Gender, colAdm2$Admit)),4)

# men applied and got in
maleIn = colAdm %>% filter(Admit == "Admitted" & Gender == "Male")
male = colAdm %>% filter(Gender == "Male")
100*round(sum(maleIn$Freq)/sum(male$Freq),4)

# women applied and got in
femaleIn = colAdm %>% filter(Admit == "Admitted" & Gender == "Female")
female = colAdm %>% filter(Gender == "Female")
100*round(sum(femaleIn$Freq)/sum(female$Freq),4)

# 2. Now, find the percentage of men who applied and got in by department. 
# Do the same with women. Compare the results with what you found in part 1. 
colAdm2 = colAdm %>% uncount(Freq)
100*round(prop.table(table(colAdm2$Gender, colAdm2$Dept, colAdm2$Admit),2),4)

male2 = male %>% uncount(Freq)
100*round(prop.table(table(male2$Dept, male2$Admit),1),4)

female2 = female %>% uncount(Freq)
100*round(prop.table(table(female2$Dept, female2$Admit),1),4)

# 3. Explain what is going on in this dataset. 
# Do you see any evidence of gender discrimination?  

male2 %>% group_by(male2$Dept) %>% summarise(malePerc = 100*n()/sum(male$Freq))
female2 %>% group_by(female2$Dept) %>% summarise(femalePerc = 100*n()/sum(female$Freq))


######################################################
################ Fandango movie rating ###############
######################################################
library(ggplot2)
install.packages('fivethirtyeight')
library(fivethirtyeight)
data(fandango)

?fandango
str(fandango)

# 1. Identify the Top 5 best rated and Top 5 worst rated movies in the dataset. 
# Average over different platforms. 

# create a new variable that represents the average score of a film 
# over different platforms
fandango$avgScore <- (fandango$fandango_ratingvalue + 
  fandango$rt_norm + 
  fandango$metacritic_norm + 
  fandango$imdb_norm)/4

# top 5 worst rated movies
fandango %>% 
  group_by(film) %>%
  summarize(avgScore) %>%
  select(film, avgScore) %>%
  arrange(avgScore) %>%
  top_n(-5)

# top 5 best rated movies
fandango %>% 
  group_by(film) %>%
  summarize(avgScore) %>%
  select(film, avgScore) %>%
  arrange(desc(avgScore)) %>%
  top_n(5)

# 2. Visualize the difference between Fandango stars and 
# actual Fandango ratings. Comment on what you see

qplot(x = fandango_stars, y = fandango_ratingvalue, data = fandango) + 
  geom_smooth() +
  xlab("fandango stars") +
  ylab("actual fandango rating") +
  ggtitle("Fandango Stars vs. Actual Fandango Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=12))

# another plot  
# convert fandango stars to a categorical variable
quantile(fandango$fandango_stars)
fandango$fandango_stars = cut(fandango$fandango_stars,
                        breaks=quantile(fandango$fandango_stars), 
                        include.lowest = TRUE)

qplot(x = fandango_stars, y = fandango_ratingvalue, data = fandango) + 
  geom_boxplot() +
  xlab("fandango stars") +
  ylab("actual fandango rating") +
  ggtitle("Fandango Stars vs. Actual Fandango Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=12))

# Given the data you have, create a metric to measure discrepancies between 
# user and critic ratings. Create a table that contains the Top 5 movies that 
# seem to appeal to critics but not the audience, and another table with the 
# Top 5 movies that users seem to like more than critics do

# Note: 
# IMDb doesn't have critic rating
# I didn't use fandango rating here because it doesn't seperate critics' rating and users' rating
# Also didn't use rottentomatoes' user rating because the dataset doesn't provide the number of rottentomatoes users, so it's hard to normalize
fandango$criticScore <- (fandango$rt_norm + fandango$metacritic_norm)/2
fandango$userScore <- (fandango$metacritic_user_nom * fandango$metacritic_user_vote_count + 
                         fandango$imdb_norm * fandango$imdb_user_vote_count)/(fandango$metacritic_user_vote_count + fandango$imdb_user_vote_count)
fandango$diff <- fandango$criticScore - fandango$userScore

# top 5 movies users like more than critics
fandango %>% 
  group_by(film) %>%
  summarize(diff) %>%
  select(film, diff) %>%
  arrange(diff) %>%
  top_n(-5)

# top 5 movies critics like more than users
fandango %>% 
  group_by(film) %>%
  summarize(diff) %>%
  select(film, diff) %>%
  arrange(desc(diff)) %>%
  top_n(5)


########################################################
################ Lahman Baseball Dataset ###############
########################################################

lb = read.csv("http://vicpena.github.io/sta9750/Teams1719.csv")
str(lb)

# 1. Create a statistic that quantifies "home advantage"
lb$AdvW <- lb$HomeW - lb$AwayW
lb$AdvL <- lb$HomeL - lb$AwayL

# 2. Find home advantage statistics for the American League (AL) 
# and National League (NL) in the 2017-2019 period. 
# Comment on the results
str(lb)

al = lb %>% filter(League == "AL" & Year <= 2019 & Year >= 2017) %>% select(Team, Year, AdvW)
al %>% group_by(Team) %>% summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv))
al %>% group_by(Year) %>% summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(Year)

nl = lb %>% filter(League == "NL" & Year <= 2019 & Year >= 2017) %>% select(Team, Year, AdvW)
nl %>% group_by(Team) %>% summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv))
nl %>% group_by(Year) %>% summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(Year)

lb %>% group_by(League) %>% summarize(HomeAdv = mean(AdvW, na.rm = TRUE))%>% arrange(desc(HomeAdv))
lb %>% group_by(Year) %>% summarize(HomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(Year)

# 3. Find the teams that had the highest and lowest home advantage effect by league 
# in 2017, 2018, and 2019 separately

# the teams that has the highest home advantage:
# AL:
al %>% filter(Year == 2017) %>% group_by(Team) %>% 
  summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv)) %>% top_n(1)
al %>% filter(Year == 2018) %>% group_by(Team) %>% 
  summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv)) %>% top_n(1)
al %>% filter(Year == 2019) %>% group_by(Team) %>% 
  summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv)) %>% top_n(1)

# NL:
nl %>% filter(Year == 2017) %>% group_by(Team) %>% 
  summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv)) %>% top_n(1)
nl %>% filter(Year == 2018) %>% group_by(Team) %>% 
  summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv)) %>% top_n(1)
nl %>% filter(Year == 2019) %>% group_by(Team) %>% 
  summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv)) %>% top_n(1)

# the teams that has the lowest home advantage:
# AL:
al %>% filter(Year == 2017) %>% group_by(Team) %>% 
  summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv)) %>% top_n(-1)
al %>% filter(Year == 2018) %>% group_by(Team) %>% 
  summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv)) %>% top_n(-1)
al %>% filter(Year == 2019) %>% group_by(Team) %>% 
  summarize(alHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(alHomeAdv)) %>% top_n(-1)

# NL:
nl %>% filter(Year == 2017) %>% group_by(Team) %>% 
  summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv)) %>% top_n(-1)
nl %>% filter(Year == 2018) %>% group_by(Team) %>% 
  summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv)) %>% top_n(-1)
nl %>% filter(Year == 2019) %>% group_by(Team) %>% 
  summarize(nlHomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(nlHomeAdv)) %>% top_n(-1)

# 4. Which franchise had the highest average home advantage in the 2017-2019 period? 
# Which one had the lowest average home advantage effect? 
lb %>% group_by(Team) %>% summarize(HomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(HomeAdv)) %>% top_n(1)
lb %>% group_by(Team) %>% summarize(HomeAdv = mean(AdvW, na.rm = TRUE)) %>% arrange(desc(HomeAdv)) %>% top_n(-1)

# 5. After completing these exercises, what did you learn about 
# home advantage effect in the MLB? 

qplot(x = Team, y = AdvW, data = lb) +
  geom_boxplot()

#################################################
#################################################
#################################################

install.packages('Lahman')
library(Lahman)

?Lahman

# 1. Let's consider data from 2018 only and look at the subset of pitchers who 
# pitched more than 250 outs. Plot the earned run average 
# (ERA; small values are good and big ones are bad) of the pitchers against their age. 
# Now, find a table with the average ERAs by age. Do you see any patterns? 

data(Pitching)
str(Pitching)

pitching <- Pitching %>% 
  filter(IPouts > 250 & yearID == 2018) %>% 
  left_join(People, by = "playerID") %>% 
  select(nameFirst, nameLast, birthYear, ERA)

pitching$Age = 2019 - pitching$birthYear

qplot(x = Age, y = ERA, data = pitching) +
  geom_point() +
  geom_smooth() + 
  ggtitle("Age vs. ERA") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10))

# another plot
# categorize age variable
pitching$Age = cut(pitching$Age,
                        breaks=quantile(pitching$Age), 
                        include.lowest = TRUE)

pitching %>% group_by(Age) %>% summarise(avgERA = mean(ERA, na.rm = TRUE))

qplot(x = Age, y = ERA, data = pitching) +
  geom_boxplot() + 
  ggtitle("Age vs. ERA") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10))

# table
pitching %>% group_by(Age) %>% summarise(avgERA = mean(ERA, na.rm = TRUE))

# 2. Again, let's look at pitchers who pitched more than 250 outs in 2018. 
# Identify the top 5 best and worst pitchers, in terms of ERA.  
pitching <- Pitching %>% 
  filter(IPouts > 250 & yearID == 2018) %>% 
  left_join(People, by = "playerID") %>% 
  select(playerID, nameFirst, nameLast, ERA)

# top 5 best pitchers
pitching %>% group_by(playerID) %>% 
  summarise(avgERA = mean(ERA, na.rm = TRUE)) %>% arrange(avgERA) %>% 
  top_n(-5) %>% left_join(People, by = "playerID") %>%
  select(nameFirst, nameLast, avgERA)

# top 5 worst pitchers
pitching %>% group_by(playerID) %>% 
  summarise(avgERA = mean(ERA, na.rm = TRUE)) %>% arrange(desc(avgERA)) %>% 
  top_n(5) %>% left_join(People, by = "playerID") %>%
  select(nameFirst, nameLast, avgERA)

# 3. Consider the best pitcher (in terms of ERA) that you found in part 2. 
# Find his ERA by season throughout his career. Based on this alone, 
# do you think he's already "peaked"? 

bestPitcher = pitching %>% group_by(playerID) %>% 
  summarise(avgERA = mean(ERA, na.rm = TRUE)) %>% arrange(avgERA) %>% 
  top_n(-1) %>% select(playerID) %>%
  inner_join(PitchingPost, by = "playerID") %>% select(playerID, yearID, ERA)

bestPitcher

pitching %>% group_by(playerID) %>% 
  summarise(avgERA = mean(ERA, na.rm = TRUE)) %>% 
  arrange(avgERA) %>% 
  top_n(-1) %>% select(playerID) %>%
  inner_join(Pitching, by = "playerID") %>% 
  select(playerID, yearID, ERA) %>% 
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, yearID, ERA)

# 4. Let's do a similar exercise, but now with batting average (BA; more is better). 
# Use the battingStats function in Lahman to find BAs. 
# Consider data from 2018 only and look at players that have more than 200 at bats (AB). 
# Plot BA against age. Do you see any patterns? Find a table with average BAs by age.

data(Batting)

batting <- Batting %>% 
  filter(AB > 200 & yearID == 2018) %>% 
  left_join(People, by = "playerID")

BAs = battingStats(data = batting, 
             idvars = c("playerID", "yearID", "stint", "teamID", "lgID"), 
             cbind = TRUE)
BAs$Age = 2019 - batting$birthYear

qplot(x = Age, y = BA, data = BAs) +
  geom_point() +
  geom_smooth() + 
  ggtitle("Age vs. BA") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10))

# table
BAs %>% group_by(Age) %>% summarise(avgBA = mean(BA, na.rm = TRUE))

# 5. Find the top 5 best and worst players in terms of BA. 

# top 5 best players
BAs %>% group_by(playerID) %>% 
  summarise(avgBA = mean(BA, na.rm = TRUE)) %>% arrange(desc(avgBA)) %>% 
  top_n(5) %>% left_join(People, by = "playerID") %>%
  select(nameFirst, nameLast, avgBA)

# top 5 worst players
BAs %>% group_by(playerID) %>% 
  summarise(avgBA = mean(BA, na.rm = TRUE)) %>% arrange(avgBA) %>% 
  top_n(-5) %>% left_join(People, by = "playerID") %>%
  select(nameFirst, nameLast, avgBA)

# 6. Consider the best player (in terms of BA) that you found in part 5. 
# Find his BA by season throughout his career. 
# Based on this alone, do you think he's already "peaked"?

BattingAllYear = battingStats(data = Batting, 
             idvars = c("playerID", "yearID", "stint", "teamID", "lgID"), 
             cbind = TRUE)

BAs %>% group_by(playerID) %>% 
  summarise(avgBA = mean(BA, na.rm = TRUE)) %>% 
  arrange(avgBA) %>% 
  top_n(1) %>% select(playerID) %>%
  inner_join(BattingAllYear, by = "playerID") %>% 
  select(playerID, yearID, BA) %>% 
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, yearID, BA)
