#############MANY LABS 3 Stroop Basic Cleaning SCRIPT##################
#######January 19, 2015###########

################Now cleaning all data#########################

setwd("/Users/Charlie/Desktop/ML3 Raw Data/JustStroop")
One<-read.csv(file="iat.csv",header=TRUE)
head(One)
Two<-read.csv(file="iat 2.csv",header=TRUE)
head(Two)
All<-rbind(One,Two)
head(All)
tail(All)

Two<-read.csv(file="iat 3.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 4.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 5.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 6.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 7.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 8.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 9.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 10.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 11.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 12.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 13.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 14.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 15.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 16.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 17.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 18.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 19.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 20.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 21.csv",header=TRUE)
All<-rbind(All,Two)
Two<-read.csv(file="iat 22.csv",header=TRUE)
All<-rbind(All,Two)


head(All)
tail(All)
Stroop<-All
head(Stroop)

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
JustTest$block_pairing_definition==JustTest$trial_word
JustTest$congruent<-JustTest$block_pairing_definition==JustTest$trial_word
str(JustTest)
JustTest[JustTest$congruent=="TRUE",]$congruent="Congruent"
JustTest[JustTest$congruent=="FALSE",]$congruent="Incongruent"
list(JustTest$congruent)
str(JustTest$congruent)
JustTest$congruent<-as.factor(JustTest$congruent)

head(JustTest)

write.csv(JustTest,file="ML3Stroop.csv",row.names=FALSE)
#Sending out raw data
write.csv(Stroop,file="ML3StroopRaw.csv",row.names=FALSE)

