#Load NFL data (already done in previous script)
#Take a look at data

head(nfldata)
table(nfldata[nfldata$dwn==3,]$yfog,nfldata[nfldata$dwn==3,]$ytg)
table(nfldata[nfldata$dwn==3,]$ytg)

library(sqldf)
library(plyr)
library(ggplot2)

#Step 1: get third down data
nfl3 <- nfldata[nfldata$dwn==3,]
nfl3 <- nfl3[nfl3$type=="PASS"| nfl3$type == "RUSH",]
nfl3[1:20,]
attach(nfl3)

#Step 2: create buckets with ifthenelse (1-3 short 4-6 medium 7-10 long 11+ distant)
x = 1
distance = c()
for (x in 1:nrow(nfl3)) {
  if (ytg[x] >= 0 & ytg[x] <= 3) distance[x] <- "Short"
  else if (ytg[x] >= 4 & ytg[x] <= 6) distance[x] <-"Medium"
  else if (ytg[x] >= 7 & ytg[x] <= 10) distance[x] <-"Long"
  else if (ytg[x] >= 11) distance[x] <- "Distant"}
nfldis <- cbind(nfl3,distance)
head(nfldis)

#Step 3: get percentage conversion then test for stat sig with two sample binomial 
conv_rate <- ddply(nfldis, .(distance, type),summarise, conversionrate = sum(fd)/length(pid), numberattempts = length(pid),converts = sum(fd),fails=length(fd[fd==0]))
conv_rate$se <- sqrt((conv_rate$conversionrate)*(1-conv_rate$conversionrate)/(conv_rate$numberattempts))
conv_rate

#Plot Conversion Rate vs. Number of Attempts
qplot(conv_rate[,4],conv_rate[,3],xlab="Number of attempts",ylab="Conversion Rate",xlim=c(0,1000)) + 
  geom_text(aes(label=paste(conv_rate[,1],conv_rate[,2]),parse=TRUE),cex=3)

#Plot Conversion Rate by Type/Distance
qplot(conv_rate[,4],conv_rate[,3],xlab="Number of Attempts",ylab="Conversion Rate (as Decimal)", ylim=c(0,.8),
      main="Conversion Rate vs. Number of Attempts \n by Play Type and Yards to Go \n with 95 Percent Confidence Bands",
      xlim=c(0,1000)) + geom_text(aes(label=paste(conv_rate[,1],conv_rate[,2]),parse=TRUE),
                                  cex=3) + geom_errorbar(ymin=conv_rate[,3]-1.96*conv_rate[,7],
                                                          ymax=conv_rate[,3]+1.96*conv_rate[,7])#,colour=type)

#stat sig
#Distant
prop.test(conv_rate$converts[1:2],conv_rate$numberattempts[1:2],alternative="greater")
#Long
prop.test(conv_rate$converts[3:4],conv_rate$numberattempts[3:4],alternative="two.sided")
#Medium
prop.test(conv_rate$converts[5:6],conv_rate$numberattempts[5:6],alternative="two.sided")
#Short
prop.test(conv_rate$converts[7:8],conv_rate$numberattempts[7:8],alternative="less")

#Manipulate data to get conversion curve by yards to go
conv_yards <- ddply(nfldis[ytg < 16,], .(ytg, type),summarise, conversionrate = sum(fd)/length(pid), numberattempts = length(pid),converts = sum(fd),fails=length(fd[fd==0]))
conv_yards

#Plot conversion rate by yards to go
qplot(data=conv_yards, x=ytg, y=conversionrate, color=type,
      xlab="Yards to Get First Down",ylab="Conversion Rate",
      main="Conversion Rate by Play Type and Yards-to-Go \n with 95 Percent Confidence Interval") + geom_line() + stat_smooth(level=0.95,aes(fill=type))

#Step 4: create buckets of 10 yard intervals
conv_loc <- ddply(nfldis[ytg < 11,], .(yfog, ytg,distance),summarise, conversionrate = sum(fd)/length(pid), numberattempts = length(pid),converts = sum(fd),fails=length(fd[fd==0]))
conv_loc
#Group by 10 yard increments by generating new variable
conv_loc$zone <- cut(conv_loc$yfog, seq(0,100,by=10),labels=seq(0,90,by=10))

#Create new dataframe with conversion rate by zone of field
conv_range <- ddply(conv_loc,.(zone,distance), summarise, convrate = sum(converts)/sum(numberattempts),attempts=sum(numberattempts))
conv_range$stan_error = sqrt((conv_range$convrate)*(1-conv_range$convrate)/(conv_range$attempts))
conv_range

#Create confidence bands for graph
limits = aes(min=conv_range[,3]-1.96*conv_range[,5], max=conv_range[,3]+1.96*conv_range[,5])

#Plot conversion rate by zone on field
qplot(data=conv_range,x=zone,y=convrate,xlab="Yardline of Field",ylab="Third Down Conversion Rate",
      main="Third Down Conversion Rate by Field Location",ylim=c(0.2,0.8),aes(colour=distance,x=zone,y=convrate)) + 
  geom_point(aes(colour=distance)) +
  geom_line(aes(x=conv_range$zone, y=conv_range$convrate,group=conv_range$distance,colour=conv_range$distance)) + 
  geom_errorbar(limits,color=rep(c("red","dark green","blue"),10)) #width=0.2,
