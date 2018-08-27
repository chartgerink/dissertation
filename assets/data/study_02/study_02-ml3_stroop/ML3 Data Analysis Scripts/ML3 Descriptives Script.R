######################ML3 Descriptives#########################

###Getting Started###
#First, go to this project's OSF page (https://osf.io/ct89g/) and download the file ML3AllSites.csv from the files section.  Place it on your desktop and run the following line, filling in your username.

setwd("/Users/YourName/Desktop") #Or the alternative path to your desktop depending on your computer (this will work for Mac)


#Loading the data#
##setting my working directory, you don't need to run the next line
setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
#run this one
ML3<-read.csv(file="ML3AllSites.csv",header=TRUE,stringsAsFactors=FALSE)

###Installing necessary packages
#If you haven't downloaded these packages, do so, then require them.  This will require you to select a CRAN mirror.  Choose the location nearest you.
install.packages("doBy")
require(doBy)
install.packages("ggplot2")
require(ggplot2)


###Descriptives are programmed to run as you run each line.  Graphs will appear in a second window.  Warning messages about NAs being coerced when converting data are fine.

###################Descriptives###################

########################################################################
###Power and Perspective Taking###
########################################################################
ML3$sarcasm<-as.integer(ML3$sarcasm)
###Power and perspective taking DV
summaryBy(sarcasm~PowerCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
summaryBy(sarcasm~PowerCond,data=ML3,FUN=list(length))
#Note-those in NA row did not write anything for the manipulated prompt
str(ML3$PowerCond)
?summaryBy

ggplot(ML3,aes(x=sarcasm,fill=PowerCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

###Length of response for power prompt
###Splitting by condition
LowPower<-subset(ML3,ML3$PowerCond=="LowPower")
HighPower<-subset(ML3,ML3$PowerCond=="HighPower")

LowPower$PowerText<-LowPower$lowpower
HighPower$PowerText<-HighPower$highpower
PowerData<-rbind(LowPower,HighPower)
head(PowerData)
tail(PowerData)
str(PowerData$PowerText)
PowerData$TextLength<-nchar(PowerData$PowerText,allowNA=TRUE)
list(PowerData$TextLength)

str(PowerData$TextLength)
summaryBy(TextLength~PowerCond,data=PowerData,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

ggplot(PowerData,aes(x=TextLength,fill=PowerCond))+geom_histogram(binwidth=50,alpha=.5,position="identity")

########################################################################
###Moral Credentials DVs###
########################################################################
ML3$mcdv1<-as.integer(ML3$mcdv1)
summaryBy(mcdv1~CredCond*Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
summaryBy(mcdv1~CredCond*Genderfactor,data=ML3,FUN=list(length))

ggplot(ML3,aes(x=mcdv1,fill=CredCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

ML3$mcdv2<-as.integer(ML3$mcdv2)
summaryBy(mcdv2~CredCond*Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

ggplot(ML3,aes(x=mcdv2,fill=CredCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

########################################################################
###Self esteem and subjective distance DV (subjective distance from class)
########################################################################
BestGrade<-subset(ML3,ML3$SubDistCond=="BestGrade")
WorstGrade<-subset(ML3,ML3$SubDistCond=="WorstGrade")
head(BestGrade)
head(WorstGrade)
tail(WorstGrade)

BestGrade$SubDist<-BestGrade$bestgrade3
WorstGrade$SubDist<-WorstGrade$worstgrade3
BestGrade$TimeSince<-BestGrade$bestgrade1
WorstGrade$TimeSince<-WorstGrade$worstgrade1
SESDdata<-rbind(BestGrade,WorstGrade)
head(SESDdata)
tail(SESDdata)

SESDdata$SubDist<-as.integer(SESDdata$SubDist)
summaryBy(SubDist~SubDistCond,data=SESDdata,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
summaryBy(SubDist~SubDistCond,data=SESDdata,FUN=list(length))

ggplot(SESDdata,aes(x=SubDist,fill=SubDistCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

########################################################################
###Warmer hearts warmer rooms DV (temperature estimate)
########################################################################
ML3$tempest1<-as.integer(ML3$tempest1)
str(ML3$tempest1)
tempclean1<-subset(ML3,ML3$tempest1<95)
TempClean<-subset(tempclean1,tempclean1$tempest1>50)
head(TempClean)

summaryBy(tempest1~TempCond,data=TempClean,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

ggplot(TempClean,aes(x=tempest1,fill=TempCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

########################################################################
###ELM DV (argument quality rating)
########################################################################
summaryBy(ArgumentQuality~ELMCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

ggplot(ML3,aes(x=ArgumentQuality,fill=ELMCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

########################################################################
###Persistence
########################################################################
head(ML3)
summary(ML3$Persistence)
sd(ML3$Persistence,na.rm=TRUE)
hist(ML3$Persistence)

########################################################################
###Weight as importance DV
########################################################################
str(ML3$IIResponse)
ML3$IIResponse<-as.integer(ML3$IIResponse)
summaryBy(IIResponse~ClipboardWeight*ClipBoardMaterial,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

ggplot(ML3,aes(x=IIResponse,fill=ClipboardWeight))+geom_histogram(binwidth=1,alpha=.5,position="identity")

########################################################################
###Metaphoric Restructuring
########################################################################

#All categorical

##########################Individual Differences##########################

head(ML3)

###Attention###
table(ML3$AttentionCheck)

#############BIG FIVE###############
###Openness
summary(ML3$Openness)
sd(ML3$Openness,na.rm=TRUE)
hist(ML3$Openness)

###Conscientiousness
summary(ML3$Conscientiousness)
sd(ML3$Conscientiousness,na.rm=TRUE)
hist(ML3$Conscientiousness)

###Extraversion
summary(ML3$Extraversion)
sd(ML3$Extraversion,na.rm=TRUE)
hist(ML3$Extraversion)

###Agreeableness
summary(ML3$Agreeableness)
sd(ML3$Agreeableness,na.rm=TRUE)
hist(ML3$Agreeableness)

###Neuroticism
summary(ML3$Neuroticism)
sd(ML3$Neuroticism,na.rm=TRUE)
hist(ML3$Neuroticism)

#############INTRINSIC MOTIVATION###############
summary(ML3$Intrinsic)
sd(ML3$Intrinsic,na.rm=TRUE)
hist(ML3$Intrinsic)

#############MOOD###############
summary(ML3$Mood)
sd(ML3$Mood,na.rm=TRUE)
hist(ML3$Mood)

#############NEED FOR COGNITION###############
summary(ML3$NFC)
sd(ML3$NFC,na.rm=TRUE)
hist(ML3$NFC)

#############REPORTED EFFORT###############
#1(no effort)-5(tried my hardest) scale
summary(ML3$ReportedEffort)
sd(ML3$ReportedEffort,na.rm=TRUE)
hist(ML3$ReportedEffort)

#############REPORTED ATTENTION###############
#1(none)-5(I have my undivided attention) scale
summary(ML3$ReportedAttention)
sd(ML3$ReportedAttention,na.rm=TRUE)
hist(ML3$ReportedAttention)

#############SELF-ESTEEM###############
#1(not very true of me)-7(very true of me) scale
#statement is: I have high self-esteem
summary(ML3$SelfEsteem)
sd(ML3$SelfEsteem,na.rm=TRUE)
hist(ML3$SelfEsteem)

#############STRESS###############
#Higher scores indicate more stress
summary(ML3$Stress)
sd(ML3$Stress,na.rm=TRUE)
hist(ML3$Stress)

##########################Demographics#################################
###See Variable Codebook###