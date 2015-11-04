#Obtained data from NFL Savant
#http://nflsavant.com/about.php

nfl2013 <- read.csv(file.choose(), header=TRUE) #import CSV file
nfl2014 <- read.csv(file.choose(), header=TRUE) #import CSV file

nfldata <- rbind(nfl2013,nfl2014) #combine two years of data into one dataset
attach(nfldata)

#See number of plays in dataset
dim(nfldata)

#Take a look at data to assess
head(nfldata)
