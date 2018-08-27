#####MANY LABS 3 STROOP ANALYSIS SCRIPT####

#Note: the beginning of this script shows how the data were cleaned.  For analyses, skip to line 162

setwd("/Users/Charlie/Desktop/ML3 Data Analysis/")

####Packages####
require(car)
require(effects)
require(lsr)
require(data.table)
require(dplyr)
require(reshape2)
require(MBESS)

############Stroop##############

Stroop<-read.csv(file="ML3StroopRaw.csv",header=TRUE,stringsAsFactors=FALSE)
head(Stroop)
str(Stroop)

###Cleaning Stroop Data###

###Isolating trials from the Test Block (there's only 1 test block)
JustTest<-subset(Stroop,Stroop$task_name=='stroop')
head(JustTest)
list(JustTest$task_name)
str(JustTest)

###Isolating the word in a given trial###
trialword<-JustTest$trial_name
trialword2<-sub('c.*','',trialword)
trialword2<-substr(trialword2,1,nchar(trialword2)-1)
trialword2
trialword2<-tolower(trialword2)
JustTest$trial_word<-trialword2
head(JustTest)

###coding colors as numbers (1=red,2=blue,3=green)
JustTest[JustTest$trial_word=="red",]$trial_word=1
list(JustTest$trial_word)
JustTest[JustTest$trial_word=="blue",]$trial_word=2
JustTest[JustTest$trial_word=="green",]$trial_word=3
list(JustTest$trial_word)

JustTest[JustTest$block_pairing_definition=="red",]$block_pairing_definition=1
list(JustTest$block_pairing_definition)
JustTest[JustTest$block_pairing_definition=="blue",]$block_pairing_definition=2
JustTest[JustTest$block_pairing_definition=="green",]$block_pairing_definition=3
list(JustTest$block_pairing_definition)
###Now the color and meaning of each word are coded as numbers

###making variable for congruent vs. incongruent trials
JustTest$congruent<-JustTest$block_pairing_definition==JustTest$trial_word
str(JustTest)
JustTest[JustTest$congruent=="TRUE",]$congruent="Congruent"
JustTest[JustTest$congruent=="FALSE",]$congruent="Incongruent"
list(JustTest$congruent)
str(JustTest$congruent)
JustTest$congruent<-as.factor(JustTest$congruent)


####Cleaning Latency Data

###Cleaning Latency Data###
Latency<-JustTest
head(Latency)
str(Latency)


###Cleaning data with Dan Martin's dplyr IAT functions (adapted)
###Reading in data from fixing the bug

Raw<-read.csv(file="StroopCleanSet.csv",header=TRUE)
Latency<-read.csv(file="StroopCleanSet.csv",header=TRUE)
head(Latency)

names(Latency)[names(Latency)=="session_id"]<-"SESSION_ID"
names(Latency)[names(Latency)=="trial_latency"]<-"TRIAL_LATENCY"
names(Latency)[names(Latency)=="trial_error"]<-"TRIAL_ERROR"
names(Latency)[names(Latency)=="congruent"]<-"CONGRUENT"

myTbl<-group_by(tbl_df(Latency),SESSION_ID)
myTbl$SUBEXCL<-0
myTblNoLong<-filter(myTbl,TRIAL_LATENCY<10000,TRIAL_LATENCY>=0)

myFastTbl<-filter(myTbl) %>%
summarise(FASTM=sum(TRIAL_LATENCY<300)/length(TRIAL_LATENCY))

isTooFast<-filter(myFastTbl,FASTM>.10)%>%
select(SESSION_ID)
if(nrow(isTooFast)>0){
	myTbl[myTbl$SESSION_ID %in% isTooFast, ]$SUBEXCL<-1
}

myTblNotFast<-group_by(myTblNoLong,SESSION_ID,CONGRUENT)

###Replacing error trials with mean of trial type + 600ms
#NOTE: in TRIAL_ERROR, 0=error, 1=correct

meanReplace<-filter(myTblNotFast,TRIAL_ERROR==1) %>%
summarise(blockMean=mean(TRIAL_LATENCY)+600)
meanReplace
head(meanReplace)
str(meanReplace)

mergeTbl<-merge(myTblNotFast,meanReplace,by=c("SESSION_ID","CONGRUENT"),all=TRUE)
head(mergeTbl)
head(mergeTbl,n=50)
if(mergeTbl$TRIAL_ERROR==0)mergeTbl$TRIAL_LATENCY=mergeTbl$blockMean

Correct<-subset(mergeTbl,mergeTbl$TRIAL_ERROR==1)
Incorrect<-subset(mergeTbl,mergeTbl$TRIAL_ERROR==0)
Incorrect$TRIAL_LATENCY<-Incorrect$blockMean
head(Incorrect)
Corrected<-rbind(Correct,Incorrect)
head(Corrected,n=50)

Congruent<-subset(Corrected,Corrected$CONGRUENT=="Congruent")
Incongruent<-subset(Corrected,Corrected$CONGRUENT=="Incongruent")
head(Congruent)
head(Incongruent)

blockMeans1<-summarise(group_by(Congruent,SESSION_ID),MC=mean(TRIAL_LATENCY),NC=length(TRIAL_LATENCY))
tblM1<-blockMeans1

blockMeans2<-summarise(group_by(Incongruent,SESSION_ID),MI=mean(TRIAL_LATENCY),NI=length(TRIAL_LATENCY))
tblM2<-blockMeans2

Means<-merge(tblM1,tblM2,by="SESSION_ID")
head(Means)
list(Means$NC)
list(Means$NI)

myTblNotFastCorrect<-filter(Corrected, TRIAL_ERROR==1)
SD<-summarise(group_by(myTblNotFastCorrect,SESSION_ID),S=sd(TRIAL_LATENCY))

Stroop<-merge(Means,SD,by="SESSION_ID")
head(Stroop)
Stroop$StroopEffect<-(Stroop$MI-Stroop$MC)/Stroop$S
mean(Stroop$StroopEffect)
sd(Stroop$StroopEffect)
Stroop
head(Stroop)


#############Checking Data################
setwd("/Users/Charlie/Desktop")
Test1<-subset(Raw,Raw$session_id==667736)
Test1
write.csv(Test1,file="StroopTest1.csv",row.names=FALSE)

Test2<-subset(Raw,Raw$session_id==668587)
Test2
write.csv(Test2,file="StroopTest2.csv",row.names=FALSE)
###Scores match up


setwd("/Users/Charlie/Desktop/ML3 Final Data")
write.csv(Stroop,file="ML3StroopData.csv",row.names=FALSE)

##########################Replicating Previous Effect####################

setwd("/Users/Charlie/Desktop/ML3 Final Data")
Stroop<-read.csv(file="ML3StroopData.csv",header=TRUE)
head(Stroop)
names(Stroop)[1]<-"session_id"

ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

ML3<-merge(ML3,Stroop,by="session_id",all.x=TRUE)


mean(ML3$StroopEffect,na.rm=TRUE)
t.test(ML3$StroopEffect,y=NULL,alternative="two.sided",mu=0,paired=FALSE,var.equal=FALSE,conf.level=.95)
#Cohen's d for one sample
d<-mean(ML3$StroopEffect,na.rm=TRUE)/sd(ML3$StroopEffect,na.rm=TRUE)
d


###Follow Up Analyses-Analyzing Error Rate###
Error1<-summarise(group_by(Congruent,SESSION_ID),EC=sum(TRIAL_ERROR==0),NCE=length(TRIAL_ERROR))
Error1

Error2<-summarise(group_by(Incongruent,SESSION_ID),EI=sum(TRIAL_ERROR==0),NIE=length(TRIAL_ERROR))
Error2

#Checking Data
setwd("/Users/Charlie/Desktop")
Test3<-subset(Raw,Raw$session_id== 673719)
Test3
write.csv(Test3,file="StroopTest3.csv",row.names=FALSE)
#Checks out

#Error Ratio
ErrorTable<-merge(Error1,Error2,by="SESSION_ID")
head(ErrorTable)

ErrorTable$CER<-ErrorTable$EC/ErrorTable$NCE
ErrorTable$IER<-ErrorTable$EI/ErrorTable$NIE
head(ErrorTable)

ErrorTable$ErrorDifference<-ErrorTable$IER-ErrorTable$CER
head(ErrorTable)

mean(ErrorTable$ErrorDifference)
t.test(ErrorTable$ErrorDifference,y=NULL,alternative="two.sided",mu=0,paired=FALSE,var.equal=FALSE,conf.level=.95)
#Cohen's d for one sample
d<-mean(ErrorTable$ErrorDifference)/sd(ErrorTable$ErrorDifference)
d

#Overall error rates
mean((ErrorTable$EC+ErrorTable$EI))
sd((ErrorTable$EC+ErrorTable$EI))

###############Time of Semester################
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

ML3<-merge(ML3,Stroop,by="session_id",all.x=TRUE)

###80 vs. 20 Analyses
Pool<-subset(ML3,ML3$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)

#First 80
mean(First80$StroopEffect,na.rm=TRUE)
t.test(First80$StroopEffect,y=NULL,alternative="two.sided",mu=0,paired=FALSE,var.equal=FALSE,conf.level=.95)
#Cohen's d for one sample
d<-mean(First80$StroopEffect,na.rm=TRUE)/sd(First80$StroopEffect,na.rm=TRUE)
d

#Last 20
mean(Last20$StroopEffect,na.rm=TRUE)
t.test(Last20$StroopEffect,y=NULL,alternative="two.sided",mu=0,paired=FALSE,var.equal=FALSE,conf.level=.95)
#Cohen's d for one sample
d<-mean(Last20$StroopEffect,na.rm=TRUE)/sd(Last20$StroopEffect,na.rm=TRUE)
d


?t.test
t.test(First80$StroopEffect,Last20$StroopEffect)
#No reliable difference between these time points

###Mixed Effects Model
#Unconditional Model
str(Pool$DaysInComp)
Stroop.Uncond<-lmer(StroopEffect~1+(1|Site),data=Pool)
summary(Stroop.Uncond)
0.0005677/(0.0005677 + 0.0950123)

#Constructing Model
Stroop.MEmodel<-lmer(StroopEffect~DaysInComp+(1|Site),data=Pool)
summary(Stroop.MEmodel)

#Comparing Model against Null Model
Stroop.MEmodel.null<-lmer(StroopEffect~1+(1|Site),data=Pool,REML=FALSE)
Stroop.MEmodel.test<-lmer(StroopEffect~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Stroop.MEmodel.null,Stroop.MEmodel.test)
coef(Stroop.MEmodel.test)

Stroop.lm<-lm(StroopEffect~DaysInComp,data=Pool)
Anova(Stroop.lm,type="II")
ci.pvaf(F.value= 5.0083,df.1=1,df.2=2658,N=2660,conf.level=.95)
plot(allEffects(Stroop.lm))

###############Moderator and Order Analyses#################

###Moderators
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(StroopEffect~AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(StroopEffect~ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(StroopEffect~ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(StroopEffect~Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(StroopEffect~Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(StroopEffect~Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-lm(StroopEffect~Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)

#####mTurk Comparison#####

mTurk<-subset(ML3,Site=="mTurk")
#Comparing Model against Null Model
mTurk.Stroop<-lm(StroopEffect~DaysInComp,data=mTurk)
Anova(mTurk.Stroop,type="II")
ci.pvaf(F.value= 2.2531,df.1=1,df.2=618,N=620,conf.level=.95)



#Summary Data

StroopSum<-summarise(group_by(ML3,Site),N=sum(StroopEffect!="NA",na.rm=TRUE),StroopMean=mean(StroopEffect,na.rm=TRUE),StroopSD=sd(StroopEffect,na.rm=TRUE))
StroopSum
setwd("/Users/Charlie/Desktop")
write.csv(StroopSum,file="StroopSum.csv",row.names=FALSE)

StroopSum$t<-StroopSum$StroopMean/(StroopSum$StroopSD/sqrt(StroopSum$N))
StroopSum
StroopSum$df<-StroopSum$N-1
StroopSum$p<-2*pt(-abs(StroopSum$t),StroopSum$df)
StroopSum$d<-StroopSum$StroopMean/StroopSum$StroopSD
setwd("/Users/Charlie/Desktop")
write.csv(StroopSum,file="StroopSum.csv",row.names=FALSE)


#####Task Order Effects####
head(ML3)
str(ML3$stroop_order)
Order.lm<-lm(StroopEffect~stroop_order,data=ML3)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.844,df.1=1,df.2= 3278,N=3280,conf.level=.95)

ML3$stroop_ordersquare<-ML3$stroop_order^2
OrderQuad.lm<-lm(StroopEffect~stroop_ordersquare,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 1.6184,df.1=1,df.2= 3278,N=3280,conf.level=.95)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
ML3<-merge(ML3,Order2,by="session_id",all=TRUE)
head(ML3)

str(ML3$StroopOrder10)
Order.lm<-lm(StroopEffect~StroopOrder10,data=ML3)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 1.6043,df.1=1,df.2= 3278,N=3280,conf.level=.95)

ML3$StroopOrder10square<-ML3$StroopOrder10^2
OrderQuad.lm<-lm(StroopEffect~StroopOrder10square,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 2.3817,df.1=1,df.2= 3278,N=3280,conf.level=.95)

StroopFirst<-subset(ML3,StroopOrder10==1)
t.test(StroopFirst$StroopEffect,y=NULL,alternative="two.sided",mu=0,paired=FALSE,var.equal=FALSE,conf.level=.95)
d<-mean(StroopFirst$StroopEffect,na.rm=TRUE)/sd(StroopFirst$StroopEffect,na.rm=TRUE)
d
