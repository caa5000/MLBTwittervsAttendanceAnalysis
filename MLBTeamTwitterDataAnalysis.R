#ABOUT: #Use datasheet (built in python) to test hypothesis that mlb team twitter activity has an impact on team attendance.
#Expect a lot of 'noise' in the data - there will be a lot of arguments to contend with. 
#1. Is there a correlation between attendance and twitter activity? i.e. Does different forms of activity on twitter impact attendance?

#Data available on github account. 
teamsData=read.csv(file="C:/Data Science Masters Program/DS 710 - Programming for Data Science/teamingDataForAnalysis.csv")
summary(teamsData)
head(teamsData)
attach(teamsData)
data(teamsData)
library(plyr)
#install.packages("ggplot2")
require(ggplot2)
library(reshape2)

#x:avg twitter post rate since 2013 season vs. y:average attendance by team since 2013 season.
ncol(teamsData) #28 columns at inception.
teamsData[,29] <- (X2013Attendper+X2014Attendper+X2015Attendper+X2016Attendper)/4 #Build column to compare average attendance over a 4 year period.
names(teamsData)[29] <- "AvgAttendancePer" #rename new column - this could have been tacked on directly to data frame.
today <- '2016/5/5'
todaysdate <- as.Date(today, '%Y/%m/%d')
teamsData$NumDaysSinceInception <- difftime(todaysdate, CreatedAt, units=c("days")) #Calculate # of days since inception of account.
teamsData$TeamPostRateSinceInception <- (teamsData$NumDaysSinceInception*24*60)/StatusesCount #Approx. how many minutes in between updates?
teamsData$TeamPostRateSinceInception <- as.integer(teamsData$TeamPostRateSinceInception)
teamsData
plot(teamsData$TeamPostRateSinceInception, teamsData$AvgAttendancePer, main="Is Post Rate Correlated with Attendance?", xlab="Average Minutes Between Posts Since Inception", ylab="Attendance Per Game") #Plot rates of attendance 
#Build a linear model to test if there is an association with post rate since inception vs. attendance.
PostRateInceptionVSAvgAttendance = lm(teamsData$AvgAttendancePer ~ teamsData$TeamPostRateSinceInception)
PostRateInceptionVSAvgAttendance # y = -3.851x + 30377.13
summary(PostRateInceptionVSAvgAttendance) #p-value of 0.925 - High As expected. 
legend("topright", legend = "Data is inconclusive", col = "blue", lwd=1)
abline(PostRateInceptionVSAvgAttendance, col="blue", lwd=1)

#x:# of likes vs. y:percent of stadium filled in 2016 - Is cooperation with fanbase a possible predictor of stadium fill?
ncol(teamsData) #31 columns after new ones created
plot(NumLikes, PercStadiumFill2016, main="Is team connecting with Twitter Fans?", xlab="# of likes by team (in 000's)", ylab="Percentage of Stadium Filled on Average, 2016")
#Build a linear model to test if there's association.
NumLikesVSPercStadiumFill2016 = lm(PercStadiumFill2016 ~ NumLikes)
NumLikesVSPercStadiumFill2016 # y = -.003x + .6815
summary(NumLikesVSPercStadiumFill2016) #p-value of 0.36 - Low amount of data points. 
legend("topright", legend = "Data is inconclusive", col = "blue", lwd=1)
abline(NumLikesVSPercStadiumFill2016, col="blue", lwd=1)
#Perform a log transformation on # of likes to see if any difference - data is skewed since some teams seem to go 'like-crazy'
logNumLikes <- log(NumLikes) #transform to log.
logNumLikes
plot(logNumLikes, PercStadiumFill2016, main="Is team connecting with Twitter Fans?", xlab="Log of # of Likes by team", ylab="Percentage of Stadium Filled on Average, 2016")
logNumLikesVSPercStadiumFill2016 = lm(PercStadiumFill2016 ~ logNumLikes) #Linear Model
logNumLikesVSPercStadiumFill2016 # y= -0.03526x+0.71406
summary(logNumLikesVSPercStadiumFill2016) #p-value of 0.222 - Not significant.
legend("topright", legend = "Data is inconclusive", col = "blue", lwd=1)
abline(logNumLikesVSPercStadiumFill2016, col="blue", lwd=1)

#ratio of twitter followers to tv market size viewing area? Which teams have the highest?
teamsData$FollowerToTVMarketRatio <- Followers/(MarketSize*1000000) #Market size was NOT in millions before.. 
teamsData$FollowerToTVMarketRatio
hist(teamsData$FollowerToTVMarketRatio) #Not quite a bell curve. 
#Determine if there is a relationship between follower/market ratio to avg % of stadium filled.
plot(teamsData$FollowerToTVMarketRatio, PercStadiumFill2016, main="Does a ratio of high twitter followers in TV Market correlate with a full stadium?", ylab="% of stadium filled - home games 2016", xlab="Ratio of Twitter Followers to size of TV Market")
FollowersMarketRatioVSPercStadiumFill2016 = lm(PercStadiumFill2016 ~ teamsData$FollowerToTVMarketRatio)
FollowersMarketRatioVSPercStadiumFill2016m # y=1.3322x+0.4748
abline(FollowersMarketRatioVSPercStadiumFill2016, col="blue", lwd=1)
summary(FollowersMarketRatioVSPercStadiumFill2016) #p-value of .00194!
legend("bottomright", legend = "Data shows a relationship (p-value .00194)", col = "blue", lwd=1)
#We can reject the null hypothesis that the ratio of twitter followers in a TV market has no relationship with how filled a stadium gets.
#Review data against 2015 % stadium filled.
plot(teamsData$FollowerToTVMarketRatio, PercStadiumFill2015, main="Does a ratio of high twitter followers in TV Market correlate with a full stadium?", ylab="% of stadium filled - home games 2015", xlab="Ratio of Twitter Followers to size of TV Market")
FollowersMarketRatioVSPercStadiumFill2015 = lm(PercStadiumFill2015 ~ teamsData$FollowerToTVMarketRatio)
FollowersMarketRatioVSPercStadiumFill2015 # y=1.4187x+0.5023 - appears to be more of a relationship.
abline(FollowersMarketRatioVSPercStadiumFill2015, col="blue", lwd=1)
summary(FollowersMarketRatioVSPercStadiumFill2015) #P value of .000344 - very much significant.
legend("bottomright", legend = "Data shows a relationship (p-value .000344)", col = "blue", lwd=1)
#install.packages("calibrate")
library("calibrate")
textxy(teamsData$FollowerToTVMarketRatio, PercStadiumFill2015, TeamName, cex=.6)
?textxy
teamsData$FollowerToTVMarketRatio
#2. Scatterplot mapping carats on x-axis and price on y-axis. Use color asthetics argument. Explain Findings.
#attach(teamsData$FollowerToTVM)
library(grid)
#Create generic text to add to plots via grobTree.
myText <- grobTree(textGrob("Data shows positive relationship. Win % (size and color) illustrated as a potential factor.", x=.27, y=.12, hjust=0, gp=gpar(col="red", fontsize=10, fontface="italic")))
slopeData <- grobTree(textGrob("y=1.4187x+0.5023 , p value : 0.000344", x=.6, y=.66, hjust=0, rot = 29, gp=gpar(col="blue", fontsize=10, fontface="italic")))
#What does team's win percentage have to do with the 'significant' ratio we've discovered?
install.packages(ggrepel)
library(ggrepel) #Allows you to shift text over on the chart. 
ggplot(teamsData, aes(x = teamsData$FollowerToTVMarketRatio, y = PercStadiumFill2015, colour = X2015WP)) + geom_text_repel(label=TeamName, box.padding = unit(.5, "lines")) + geom_point(aes(size = X2015WP), show.legend = FALSE) + scale_colour_gradient("2015 Win %",low = "red", high = "green") + theme(legend.title=element_text(colour="red", size = 14, face="bold"),legend.position="right") + labs(x = "Ratio of Twitter Followers to size of TV Market (markets with 2 teams halved)", y = "% of stadium fill for 2015 Home Games", title = "Does ratio of twitter followers in a TV Market correlate with MLB attendance?", size="2015 Win %", colour="2015 Win %") + geom_abline(intercept=0.5023, slope=1.4187, color="blue", size=.5, linetype=2) + annotation_custom(myText) + annotation_custom(slopeData)
?geom_text_repel
#Plot with the team names for further looks.
ggplot(teamsData, aes(x = teamsData$FollowerToTVMarketRatio, y = PercStadiumFill2015, label = TeamName, colour = X2015WP)) + geom_text(aes(fill = X2015WP), color="blue", check_overlap=TRUE, fontface = "bold", show.legend=FALSE) + labs(x = "Ratio of Twitter Followers in each TV Market", y = "% of stadium fill for 2015 Home Games", title = "Does ratio of twitter followers in a TV Market correlate with attendance?", label="2015 Win %", colour="2015 Win %", fontface="bold")
#Does payroll have an affect? Start by tiering the teams.
?position_dodge
teamsData$PayrollRanking <- c('bottom third','middle third','bottom third','bottom third','bottom third','top third','bottom third','bottom third','top third','top third','middle third','middle third','top third','top third','top third','bottom third','top third','middle third','top third','middle third', 'bottom third','middle third','middle third','middle third','bottom third', 'top third','middle third','top third','middle third','bottom third')
teamsData
payrollInfo <- grobTree(textGrob("Payroll could play a factor in ratio of twitter followers.", x=.53, y=.12, hjust=0, gp=gpar(col="red", fontsize=10, fontface="italic")))
ggplot(teamsData, aes(x = teamsData$FollowerToTVMarketRatio, y = PercStadiumFill2015, size=3)) + geom_point(aes(color = factor(teamsData$PayrollRanking))) + scale_color_manual(values = c("red", "yellow", "green")) + scale_fill_discrete(name="MLB Payroll Rankings in Thirds", breaks="Bottom Third (42M - 88M)") + theme(legend.title=element_text(colour="blue", size = 14, face="bold"),legend.position="right") + labs(x = "Ratio of Twitter Followers to size of TV Market (markets with 2 teams halved)", y = "% of stadium fill for 2015 Home Games", title = "How is payroll, twitter ratio and stadium fill associated?", colour="2015 Payroll Ranking", size="") + geom_abline(intercept=0.5023, slope=1.4187, color="blue", size=.5, linetype=2) + annotation_custom(payrollInfo) + annotation_custom(slopeData)
#Does ticket price play a roll? 
ggplot(teamsData, aes(x = teamsData$FollowerToTVMarketRatio, y = PercStadiumFill2015, colour = X2016AvgPrice, size = 3)) + geom_point(aes(color = X2016AvgPrice)) + scale_colour_gradient("Ticket Cost",low = "red", high = "green") + theme(legend.title=element_text(colour="blue", size = 14, face="bold"),legend.position="right") + labs(x = "Ratio of Twitter Followers to size of TV Market (markets with 2 teams halved)", y = "% of stadium fill for 2015 Home Games", title = "Is stadium fill a matter of ticket price more than anything?", colour="Avg. Ticket Price") + geom_abline(intercept=0.5023, slope=1.4187, color="blue", size=.5, linetype=2) + annotation_custom(myText) + annotation_custom(slopeData)
#How big of a role does win percentage play?
ggplot(teamsData, aes(x = teamsData$FollowerToTVMarketRatio, y = PercStadiumFill2015, colour = x0, size = 3)) + geom_point(aes(color = X2016AvgPrice)) + scale_colour_gradient("Ticket Cost",low = "red", high = "green") + theme(legend.title=element_text(colour="blue", size = 14, face="bold"),legend.position="right") + labs(x = "Ratio of Twitter Followers to size of TV Market (markets with 2 teams halved)", y = "% of stadium fill for 2015 Home Games", title = "Is stadium fill a matter of ticket price more than anything?", colour="Avg. Ticket Price") + geom_abline(intercept=0.5023, slope=1.4187, color="blue", size=.5, linetype=2) + annotation_custom(myText) + annotation_custom(slopeData)
#Check Q-Q plot for any gross anamolies. 
qqplot(teamsData$FollowerToTVMarketRatio, PercStadiumFill2015) #Data appears 'left-skewed' - does not require attention. More data points would help solidify.
qqline(teamsData$FollowerToTVMarketRatio)

#How does current twitter accounts to TV Market ratio stack up against attendance data to 2013? (To help solidify relationship).
teamsData$AvgAttendance2013thru2016 <- (PercStadiumFill2013+PercStadiumFill2014+PercStadiumFill2015+PercStadiumFill2016)/4 #accidently duplicated.
plot(teamsData$FollowerToTVMarketRatio, teamsData$AvgAttendance2013thru2016) #raw plot for eyes.
FollowerRatioAvgAttendanceModel <- lm(teamsData$AvgAttendance2013thru2016 ~ teamsData$FollowerToTVMarketRatio)
FollowerRatioAvgAttendanceModel # y = 1.3385x + 0.5022 - Seem similar to others.
summary(FollowerRatioAvgAttendanceModel) #p value of .000251 - definitely still a relationship. Helps verify the correlation. 
abline(FollowerRatioAvgAttendanceModel)

?geom_abline
?geom_label
?theme
?geom_point
?geom_text

#Does a higher % of twitter followers in a TV market (ratio) correlate with stadium fill? What about other variables? 
#Segment teams into higher and lower compared to this point last year. Build data frame. 
teamsData$AttendanceUpDownCompared2016to2015 <- c('UP','UP','DOWN','DOWN','UP','DOWN','DOWN','DOWN','DOWN','DOWN','UP','UP','UP','DOWN','UP','DOWN', 'UP','UP','DOWN','DOWN','DOWN','UP','DOWN','DOWN','DOWN','UP','UP','UP','UP','DOWN')
teamsData$AttendanceUpDownCompared2016to2015nums <- c(108, 1853, -8181, -729, 7320, -1461, -8291, -1350, -330, -2739, 3525, 2654, 2401, -1129, 1096, -3163, 905, 1316, -2913, -4004, -1206, 2692, -5083, -2398, -3, 713, 61, 3574, 7938, -3172)
?scale_color_manual
teamsData #Raw numbers may not be used.
#Calculate the mean of the two separate groups and run a t-test.
t.test(teamsData$FollowerToTVMarketRatio ~ teamsData$AttendanceUpDownCompared2016to2015) #Inconclusive. Up-.137, Down-.127
#Is there a difference in means for average number of followers?
t.test(Followers ~ teamsData$AttendanceUpDownCompared2016to2015) #Inconclusive. Up-686k, Down-558k.
#Is there a difference in means for number of likes?
t.test(NumLikes ~ teamsData$AttendanceUpDownCompared2016to2015) #Inconclusive. Up-9.8, Down-10.33
#Is there a difference in means for statuses count?
t.test(StatusesCount ~ teamsData$AttendanceUpDownCompared2016to2015) #Inconclusive. Up-37k, Down-38k. 
#Is there a difference in means for recent post rate? 
t.test(MinsBetweenTweetsLast150 ~ teamsData$AttendanceUpDownCompared2016to2015) #Inconclusive (p:.3232). Up-50.37, Down-42.35
t.test(MinsBetweenTweetsLast150 ~ teamsData$AttendanceUpDownCompared2016to2015, alternative="l") #Inconclusive.


