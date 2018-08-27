#############MANY LABS 3 COMPUTER DATE SCRIPT##################
#######January 19, 2015###########

setwd("/Users/Charlie/Desktop")
DateTest<-read.csv(file="sessionsTest.csv",header=TRUE)
head(DateTest)
JustDate<-DateTest[,c("session_id","session_date")]
head(JustDate)

require(reshape)

DateTimeSplit<-colsplit(JustDate$session_date, split=" ", c("Date","Time"))
head(JustDate)
DateAll<-cbind(JustDate,DateTimeSplit)
head(DateAll)
tail(DateAll)

DateSplit<-colsplit(DateAll$Date,split="/",c("Month","Day","Year"))
head(DateSplit)
DateAll<-cbind(DateAll,DateSplit)
head(DateAll)
tail(DateAll)

###Calculating number of days since August 1

head(DateAll)
DateAll$DaysSinceMonth[DateAll$Month=="8"]<-0
DateAll$DaysSinceMonth[DateAll$Month=="9"]<-31
DateAll$DaysSinceMonth[DateAll$Month=="10"]<-61
DateAll$DaysSinceMonth[DateAll$Month=="11"]<-92
DateAll$DaysSinceMonth[DateAll$Month=="12"]<-122

#Adding Days
DateAll$DaysSinceAug<-DateAll$Day+DateAll$DaysSinceMonth
head(DateAll)
DateAll

################Now cleaning all data#########################

setwd("/Users/Charlie/Desktop/ML3 All Sessions Files")

One<-read.csv(file="sessions.csv",header=TRUE)
head(One)
Two<-read.csv(file="sessions 2.csv",header=TRUE)
head(Two)
All<-rbind(One,Two)
head(All)
tail(All)

Two<-read.csv(file="sessions 3.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 4.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 5.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 6.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 7.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 8.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 9.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 10.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 11.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 12.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 13.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 14.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 15.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 16.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 17.csv",header=TRUE)
All<-rbind(All,Two)
###18 is OSU
Two<-read.csv(file="sessions 19.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 20.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="sessions 21.csv",header=TRUE)
All<-rbind(All,Two)

head(All)
tail(All)

JustDate<-All[,c("session_id","session_date")]
head(JustDate)

str(JustDate$session_date)
JustDate$session_date<-as.character(JustDate$session_date)

DateTimeSplit<-colsplit(JustDate$session_date, split=" ", c("Date","Time"))
head(JustDate)
DateAll<-cbind(JustDate,DateTimeSplit)
head(DateAll)
tail(DateAll)

DateSplit<-colsplit(DateAll$Date,split="/",c("Month","Day","Year"))
head(DateSplit)
DateAll<-cbind(DateAll,DateSplit)
head(DateAll)
tail(DateAll)

###Calculating number of days since August 1

head(DateAll)
DateAll$DaysSinceMonth[DateAll$Month=="8"]<-0
DateAll$DaysSinceMonth[DateAll$Month=="9"]<-31
DateAll$DaysSinceMonth[DateAll$Month=="10"]<-61
DateAll$DaysSinceMonth[DateAll$Month=="11"]<-92
DateAll$DaysSinceMonth[DateAll$Month=="12"]<-122

#Adding Days
DateAll$DaysSinceAug<-DateAll$Day+DateAll$DaysSinceMonth
head(DateAll)
tail(DateAll)

###Dealing with OSU data (different format due to error)###

OSU<-read.csv(file="sessions 18(OSU,altered).csv",header=TRUE)
head(OSU)

JustDateOSU<-OSU[,c("session_id","session_date")]
head(JustDateOSU)

str(JustDateOSU$session_date)
JustDateOSU$session_date<-as.character(JustDateOSU$session_date)

DateTimeSplitOSU<-colsplit(JustDateOSU$session_date, split=" ", c("Date","Time"))
head(JustDateOSU)
DateAllOSU<-cbind(JustDateOSU,DateTimeSplitOSU)
head(DateAllOSU)
tail(DateAllOSU)

DateSplitOSU<-colsplit(DateAllOSU$Date,split="/",c("Month","Day","Year"))
head(DateSplitOSU)
DateAllOSU<-cbind(DateAllOSU,DateSplitOSU)
head(DateAllOSU)
tail(DateAllOSU)

###Calculating number of days since August 1

head(DateAll)
DateAllOSU$DaysSinceMonth[DateAllOSU$Month=="8"]<-0
DateAllOSU$DaysSinceMonth[DateAllOSU$Month=="9"]<-31
DateAllOSU$DaysSinceMonth[DateAllOSU$Month=="10"]<-61
DateAllOSU$DaysSinceMonth[DateAllOSU$Month=="11"]<-92
DateAllOSU$DaysSinceMonth[DateAllOSU$Month=="12"]<-122

#Adding Days
DateAllOSU$DaysSinceAug<-DateAllOSU$Day+DateAllOSU$DaysSinceMonth
head(DateAllOSU)
tail(DateAllOSU)

######Merging it all########
head(DateAll)
head(DateAllOSU)

DateAllFinal<-rbind(DateAll,DateAllOSU)
head(DateAllFinal)

write.csv(DateAllFinal,file="ML3 Computer Dates.csv",row.names=FALSE)
