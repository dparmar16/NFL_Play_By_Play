#Data already loaded in NFLPlay.R

#Generate drive numbers based on the data
install.packages("ggplot2")
library(ggplot2)
library(sqldf)

#First look at the data already loaded
head(nfldata)

#Create binary variable ind
#ind equals true if dseq number increases or stays the same (drive continues)
#ind equals false if dseq number decreases (to 0 or 1 signalling new drive)
ind <- with(nfldata, dseq[-1L] >= dseq[-length(dseq)])
head(ind)

#Create drive number vector to store values
drivenumber = c()
i = 1

#For loop to assign drive number
for (x in 1:(nrow(nfldata))) {
  if (ind[x] == FALSE) {
    i <- i + 1
  }
  drivenumber[x] <- i
}

#Look at drivenumber vector
drivenumber[1:25]
nfldata[1:25,]

#Get counts data frame
drive_tab <- rle(sort(drivenumber))
drive_table <- data.frame(number=drive_tab$values,count=drive_tab$lengths)
head(drive_table)

#hist(drive_table)
qplot(drive_table$count,data=drive_table, geom="histogram", binwidth = 1, 
      xlab="Plays in Drive",ylab="Number of Drives",main = "Number of Drives with Amount of Plays ")

#Make sure drive number lines up
cbind(nfldata[1:50,6],c(1,drivenumber[1:49]))
dim(nfldata)
nfl_updated <- cbind(nfldata,c(1,drivenumber)) 
names(nfl_updated)[31] <- "driveid"
head(nfl_updated)

#Do some spot checking to double check numbers match up
sqldf("select driveid from nfl_updated group by driveid having count(driveid) = 4 limit 5")
sqldf("select gid, pid, off,def,type,dseq,len,dwn,fd,driveid from nfl_updated where driveid in (10,11,12,36,37,38)")
