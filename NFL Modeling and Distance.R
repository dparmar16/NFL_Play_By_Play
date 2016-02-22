#IN REPORT:
#Who is this problem important to
#Why is this important
#What is Markov relevance to NFL
#Tell a story: background context, problem, and result

#Change distances
dist2 = c()
head(nfl3)
attach(nfl3)
for (x in 1:nrow(nfl3)) {
  if (ytg[x] >= 0 & ytg[x] <= 4) dist2[x] <- "Short"
  else if (ytg[x] >= 5 & ytg[x] <= 8) dist2[x] <-"Medium"
  else if (ytg[x] >= 9 & ytg[x] <= 12) dist2[x] <-"Long"
  else if (ytg[x] >= 13) dist2[x] <- "Distant"}
nfldis2 <- cbind(nfl3,dist2)

dist3 = c()
for (x in 1:nrow(nfl3)) {
  if (ytg[x] >= 0 & ytg[x] <= 2) dist3[x] <- "Short"
  else if (ytg[x] >= 3 & ytg[x] <= 4) dist3[x] <-"Medium"
  else if (ytg[x] >= 5 & ytg[x] <= 6) dist3[x] <-"Long"
  else if (ytg[x] >= 7) dist3[x] <- "Distant"}
nfldis3 <- cbind(nfl3,dist3)


#Make model to predict third down conversion and see what matters (and if difference distnace yards matter)
head(nfldis)
nfldis_level <- within(nfldis, distance <- relevel(distance, ref = "Short"))
??glm
thirddownmod <- glm(data=nfldis_level, fd~qtr+yfog+as.factor(distance)+dseq+type, family="binomial")
summary(thirddownmod)
head(nfldis2)
thirddownmod2 <- glm(data=nfldis2, fd~qtr+yfog+as.factor(distance)+dseq+type, family="binomial")
summary(thirddownmod2)
thirddownmod3 <- glm(data=nfldis3, fd~qtr+yfog+as.factor(distance)+dseq+type, family="binomial")
summary(thirddownmod3)

#DO SAME GRAPHS WITH DIFFERENT YARD GROUPS
#Yardage grouping 2
library(plyr)
conv_rate2 <- ddply(nfldis2, .(dist2, type),summarise, conversionrate = sum(fd)/length(pid), numberattempts = length(pid),converts = sum(fd),fails=length(fd[fd==0]))
conv_rate2$se <- sqrt((conv_rate2$conversionrate)*(1-conv_rate2$conversionrate)/(conv_rate2$numberattempts))
conv_rate2
qplot(conv_rate2[,4],conv_rate2[,3],xlab="Number of Attempts",ylab="Conversion Rate (as Decimal)", ylim=c(0,.8),
      main="Conversion Rate vs. Number of Attempts \n by Play Type and Yards to Go \n with 95 Percent Confidence Bands",
      xlim=c(0,1000)) + geom_text(aes(label=paste(conv_rate2[,1],conv_rate2[,2]),parse=TRUE),
                                  cex=3) + geom_errorbar(ymin=conv_rate2[,3]-1.96*conv_rate2[,7],
                                                         ymax=conv_rate2[,3]+1.96*conv_rate2[,7])#,colour=type)

#Yardage grouping 3
conv_rate3 <- ddply(nfldis3, .(dist3, type),summarise, conversionrate = sum(fd)/length(pid), numberattempts = length(pid),converts = sum(fd),fails=length(fd[fd==0]))
conv_rate3$se <- sqrt((conv_rate3$conversionrate)*(1-conv_rate3$conversionrate)/(conv_rate3$numberattempts))
conv_rate3
qplot(conv_rate3[,4],conv_rate3[,3],xlab="Number of Attempts",ylab="Conversion Rate (as Decimal)", ylim=c(0,.8),
      main="Conversion Rate vs. Number of Attempts \n by Play Type and Yards to Go \n with 95 Percent Confidence Bands",
      ) + geom_text(aes(label=paste(conv_rate3[,1],conv_rate3[,2]),parse=TRUE),
                                  cex=3) + geom_errorbar(ymin=conv_rate3[,3]-1.96*conv_rate3[,7],
                                                         ymax=conv_rate3[,3]+1.96*conv_rate3[,7])#,colour=type)

