library(dplyr)
library(tidyr)

##
## Joining data
##
# Examples from:
# https://dplyr.tidyverse.org/reference/join.html

band_members
band_instruments


# mutating joins
# --------------

# inner_join: merge and keep rows that appear in both datasets
band_members %>% inner_join(band_instruments, by = "name")
# left_join: merge and keep rows in "left" dataset 
band_members %>% left_join(band_instruments, by = "name")

# right_join: merge and keep rows in "right" dataset
band_members %>% right_join(band_instruments, by = "name")

# full_join: merge and keep all rows!
band_members %>% full_join(band_instruments, by = "name")

band_members %>% full_join(band_instruments)


# filtering joins
# ---------------

# no attempt at merging information
# band_instruments used for filtering band_members

# semi_join: keep rows in band_members 
#            that have a match in band_instruments
band_members
band_instruments
band_members %>% semi_join(band_instruments, by = "name")

# anti_join: keep rows in band_members 
#            that DON'T have a match in band_instruments
band_members %>% anti_join(band_instruments, by = "name")



# what if join variables have different names?
band_members
band_instruments2

band_members %>%
  left_join(band_instruments2, by = c("name" = "artist"))

# indicate it in by statement

# Duplicate rows
# --------------

df1 = data.frame(x = c("a", "a", "b"), y = 1:3)
df2 = data.frame(x = c("a", "b", "c"), z = 4:6)
df1 %>% left_join(df2, by = "x")
df2 %>% left_join(df1, by = "x")

df2
df1
# what happens if we left_join?

# another example


# NYC flights data
# https://r4ds.had.co.nz/relational-data.html
install.packages("nycflights13")
library(nycflights13)
library(dplyr)
library(tidyr)

glimpse(airlines)
glimpse(airports)
glimpse(planes)
glimpse(weather)
glimpse(flights)

# key: variable(s) that uniquely identify a row 

# key of airline?
airlines
# key of airports?
airports
# key of planes?


# add weather information to flights data
glimpse(flights)
glimpse(weather)
fl2 = flights %>% left_join(weather)
glimpse(fl2)

# add airport name info to flights
glimpse(flights)
df2 = flights %>% left_join(airports, by = c("dest" = "faa"))
glimpse(df2)

# add plane info to flights
glimpse(flights)
glimpse(flights %>% left_join(planes, by = "tailnum"))

# compute average arrival delay by destination
glimpse(flights)
# sort in descending order by average delay 
flights %>% group_by(dest) %>%
  summarize(avgDelay = mean(arr_delay, na.rm = T)) %>%
    arrange(desc(avgDelay)) %>%
      left_join(airports, by = c("dest" = "faa")) %>%
        select(dest, name, avgDelay)

glimpse(airports)

# force R to compute mean with NAs
x = c(1, 2, 3, 4, 5, NA)
mean(x, na.rm = T)