#Get 2015 Weeks 1-8 data for free from 
#http://www.armchairanalysis.com/data.php
#Load in Play.csv
nfldata <- read.csv(file.choose(), header = TRUE)

#Attack and take a look at data
attach(nfldata)
head(nfldata)


library(ggplot2)
library(dplyr)


#Look at all rush and pass plays
nfl_off <- nfldata[type=="RUSH" | type=="PASS",]

#Can use either dplyr or plyr
#If you want to use dplyr, then can do this:
#library(plyr)
#nfl_off %>% group_by(yfog) %>% select(type) %>% summarise(lengthtype = length(type))
#Need to remove dplyr to use plyr and ddply
#remove.packages("dplyr")

#Choose to use plyr package and ddply command
#Get number of plays by yardline (relative to goaline), pass or run, and 1st/2nd/3rd/4th down
playcomp <- ddply(nfl_off, .(yfog,type,dwn),summarise, playsran = length(pid))
head(playcomp)

#Use qplot function from ggplot2
#Get number of plays on y axis, yardline on x axis, colored by down, shaped by pass/run
qplot(x=playcomp$yfog,xlab="Yardline",ylim=c(0,100),y=playcomp$playsran,ylab="Number of Plays",
      color=as.factor(playcomp$dwn),
      shape=playcomp$type) + scale_shape_manual(values=c(79,16)) + theme(legend.position="bottom")

#Reduce print output by not printing warnings
```{r warning=FALSE}
#Use facet_wrap to have multiple graphs on one plot
plot_one <- qplot(data=playcomp, x=yfog,xlab="Yardline",y=playsran,ylim=c(0,100),
                  ylab="Number of Plays",color=as.factor(type))#,color=as.factor(type))#+ geom_point() 
plot_one + facet_wrap(~dwn) + scale_color_discrete(name="Play Type", labels=c("Pass","Rush"))
```