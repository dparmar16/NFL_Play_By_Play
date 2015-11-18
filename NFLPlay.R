#Get 2015 Weeks 1-8 data for free from 
#http://www.armchairanalysis.com/data.php
#Load in Play.csv
nfldata <- read.csv(file.choose(), header = TRUE)

#Attack and take a look at data
attach(nfldata)
head(nfldata)

#
library(ggplot2)
library(dplyr)
library(plyr)
remove.packages("dplyr")

#Look at all rush and pass plays
nfl_off <- nfldata[type=="RUSH" | type=="PASS",]

#Can use either dplyr or plyr
#If you want to use dplyr, then can do this:
nfl_off %>% group_by(yfog) %>% select(type) %>% summarise(lengthtype = length(type))

#Choose to use plyr package and ddply command
#Get number of plays by yardline (relative to goaline), pass or run, and 1st/2nd/3rd/4th down
playcomp <- ddply(nfl_off, .(yfog,type,dwn),summarise, playsran = length(pid))

#Use qplot function from ggplot2
#Get number of plays on y axis, yardline on x axis, colored by down, shaped by pass/run
qplot(x=playcomp$yfog,xlab="Yardline",ylim=c(0,100),y=playcomp$playsran,ylab="Number of Plays",
      color=as.factor(playcomp$dwn),
      shape=playcomp$type) + scale_shape_manual(values=c(79,16)) + theme(legend.position="bottom")

