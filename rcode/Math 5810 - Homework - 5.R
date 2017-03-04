#
# Using Hadley's Bigvis for the airline data 2013 to 2016
#
# to use this code, first run the code in install-bigvis.R
# then unzip the file Airline.7z (I use 7-zip)
# into the folder ../Data/Airline
# You can adjust the folder relative to your desired working directory
# Usually I assume the working directory is the home of bigvis-06.R

library(ggplot2)
library(dplyr)
library(bigvis)
#library(reshape2)
#library(scales)
#library(grid)

# ../Data/Airline contains lots of data files
# we can read the file names
temp = list.files(path = "C:/Users/Ian/Google Drive/Data/Airline", full.names=TRUE)
# you should check that temp contains the file names - otherwise you have a 
# problem with where you stored your data or the path name above
temp

# read in the contents of each file
all = lapply(temp, read.csv, header=TRUE)

# convert to tibbles
all = lapply(all, tbl_df)

# and combine them:
flights = bind_rows(all)

# the variables we will use are the following:
flights = select(flights, 
                 DAY_OF_WEEK,
                 DEP_TIME,
                 ARR_TIME,
                 ARR_DELAY,
                 DEP_DELAY,
                 AIR_TIME,
                 DISTANCE)


# notice that some of the values are missing
sum(is.na(flights))

# you can remove them if you like, using
# flights = na.omit(flights)
# I'm using the original with missing values included

# More convenient to extract the variables we want to use
delay = flights$ARR_DELAY
dist = flights$DISTANCE
airtime = flights$AIR_TIME
depdelay = flights$DEP_DELAY
# time is in minutes, speed in miles per hour
speed = dist / (airtime / 60 ) 

time = flights$ARR_TIME
hours = floor(time/100)
mins = time-100*hours
time = hours + (mins/60)



#This is where I was going to find out if there was a day of the week that had more flights
dayofweek = flights$DAY_OF_WEEK
dayofweek_sum = condense(bin(dayofweek, width=1))
autoplot(dayofweek_sum)
autoplot(smooth(dayofweek_sum, 10))
#If I did this right there are more flights on Monday than the rest of the week


#This one is trying to show how many departure delays across the data and 
#It seems like most made it out on time or earlier
depdelay_sum = condense(bin(depdelay, 100))
autoplot(depdelay_sum)
autoplot(smooth(depdelay_sum, 1000))


#I tried to see if there day of week made a difference of departing on time.
#I think it also shows that most flights leave in the morning.
#and all the later flights happen during midweek.
daydelay = condense(bin(dayofweek, 1), bin(depdelay, 60))
autoplot(daydelay)
autoplot(peel(daydelay))

teaser = list(
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(0, 0.5, 0, 0.5), "lines"),
    legend.key.width = unit(1.45, "inches"),
    text = element_text(size = 24)
  ),
  labs(x = NULL, y = NULL, fill = NULL)
)

daydeld = condense(bin(dayofweek, 1), bin(depdelay, 120), z = airtime)
daydeld = subset(daydeld, dayofweek > 0)
autoplot(daydeld) + teaser

airtime_sum = condense(bin(airtime, 100))
autoplot(airtime_sum)
autoplot(smooth(airtime_sum, 1000))

#This one was comparing airtime and departure delay.
#It looks to me like the more airtime you have on a flight,
#the less likely you are to depart late.
airdelay = condense(bin(airtime, 10), bin(depdelay, 10))
autoplot(airdelay)
autoplot(peel(airdelay))

teaser = list(
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(0, 0.5, 0, 0.5), "lines"),
    legend.key.width = unit(1.45, "inches"),
    text = element_text(size = 24)
  ),
  labs(x = NULL, y = NULL, fill = NULL)
)

airdelayd = condense(bin(airtime, 10), bin(depdelay, 10), z = dist)
airdelayd = subset(airdelayd, airtime > 100)
autoplot(airdelayd) + teaser