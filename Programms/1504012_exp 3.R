# exp no 2 
library(datasets)
library(dplyr)

# Step 1
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/lahman-batting.csv") % > %
  
  # Step 2
  select(c(playerID, yearID, AB, teamID, lgID, G, R, HR, SH))  % > % 
  
  # Step 3
  arrange(playerID, teamID, yearID)

# Structure of the data
glimpse(data)


filter(airquality, Temp > 70)

library(dplyr)
filter(airquality,Day==25 & Temp>70)
filter(airquality,Day>25 | Temp>70)

mutate(airquality, TempInC = (Temp - 32) * 5 / 9)

summarise(airquality, mean(Temp, na.rm = TRUE))

summarise(airquality, mean(Wind, na.rm = TRUE))

summarise(group_by(airquality, Month), mean(Temp, na.rm = TRUE))

summarise(group_by(airquality,Month),mean(Temp))

summarise(group_by(airquality,Month),var(Temp))

sample_n(airquality, size = 10)
sample_frac(airquality, size = 0.1)

sample_n(airquality,size = 5)
count(airquality, Month)
arrange(airquality, desc(Month), Day)
arrange(airquality,order(Month),Day)

airquality %>% 
  +     filter(Month != 5) %>% 
  +     group_by(Month) %>% 
  +     summarise(mean(Temp, na.rm = TRUE))

select(flights, year)

# --------------------------------------------------------------------------



data1 <- read.csv("acme.csv")

print(data1)

head(data1)

tail(data1)














