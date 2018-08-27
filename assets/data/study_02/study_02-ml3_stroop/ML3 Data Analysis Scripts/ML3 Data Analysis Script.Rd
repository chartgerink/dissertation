#############MANY LABS 3 DATA ANALYSIS SCRIPT##################
#######Charlie Ebersole, Begin November 13, 2014###########

#####################READING AND CLEANING DATA###########################

###Using PLU's preliminary data for checking the script
getwd()
setwd("/Users/Charlie/Desktop/ML3 Data Analysis/Test Data")
ML3<-read.csv(file="flippedexplicitPLU.csv",header=TRUE,na.strings=".",stringsAsFactors=FALSE)
head(ML3)
str(ML3)


####Packages####
require(car)
require(effects)
require(lsr)
require(data.table)
require(dplyr)
require(doBy)



#################
## Add descriptive statistics for the entire data file - means, ranges, SDs, distributions; make it easy to discover coding errors, ceiling/floor effects and violations of distributional assumptions.  Circulate those for multiple eyes to review. ##
#################


###Adding the order variables for the final condition assignments###
setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
Order<-read.csv(file="ML3Order.csv",header=TRUE,stringsAsFactors=FALSE)
#Merging order data
ML3<-merge(ML3,Order,by="session_id")
head(ML3)
length(ML3$session_id)

##########################Individual Differences##########################

head(ML3)

###Attention###
list(ML3$atttention)
ML3$AttentionCheck[ML3$attentioncorrect!="NA"]<-"Pass"
ML3$AttentionCheck[ML3$attention!="NA"]<-"Fail"

hist(ML3$AttentionCheck)

#############BIG FIVE###############
###Openness
ML3$Openness<-(ML3$big5_05+(8-ML3$big5_10))/2
summary(ML3$Openness)
sd(ML3$Openness,na.rm=TRUE)
hist(ML3$Openness)

###Conscientiousness
ML3$Conscientiousness<-(ML3$big5_03+(8-ML3$big5_08))/2
summary(ML3$Conscientiousness)
sd(ML3$Conscientiousness,na.rm=TRUE)
hist(ML3$Conscientiousness)

###Extraversion
ML3$Extraversion<-(ML3$big5_01+(8-ML3$big5_06))/2
summary(ML3$Extraversion)
sd(ML3$Extraversion,na.rm=TRUE)
hist(ML3$Extraversion)

###Agreeableness
ML3$Agreeableness<-(ML3$big5_07+(8-ML3$big5_02))/2
summary(ML3$Agreeableness)
sd(ML3$Agreeableness,na.rm=TRUE)
hist(ML3$Agreeableness)

###Neuroticism
ML3$Neuroticism<-(ML3$big5_04+(8-ML3$big5_09))/2
summary(ML3$Neuroticism)
sd(ML3$Neuroticism,na.rm=TRUE)
hist(ML3$Neuroticism)

#############INTRINSIC MOTIVATION###############
#All questions on 1(never or almost never true of me)-4(Always or almost always true of me) scale
ML3$Intrinsic<-(ML3$intrinsic_01+ML3$intrinsic_02+ML3$intrinsic_03+ML3$intrinsic_04+ML3$intrinsic_05+(5-ML3$intrinsic_06)+(5-ML3$intrinsic_07)+ML3$intrinsic_08+ML3$intrinsic_09+ML3$intrinsic_10+ML3$intrinsic_11+ML3$intrinsic_12+ML3$intrinsic_13+ML3$intrinsic_14+ML3$intrinsic_15)/15
summary(ML3$Intrinsic)
sd(ML3$Intrinsic,na.rm=TRUE)
hist(ML3$Intrinsic)

#############MOOD###############
ML3$Mood<-((8-ML3$mood_01)+(8-ML3$mood_02))/2
summary(ML3$Mood)
sd(ML3$Mood,na.rm=TRUE)
hist(ML3$Mood)

#############NEED FOR COGNITION###############
ML3$NFC<-(ML3$nfc_01+(6-ML3$nfc_02)+(6-ML3$nfc_03)+ML3$nfc_04+ML3$nfc_05+(6-ML3$nfc_06))/6
summary(ML3$NFC)
sd(ML3$NFC,na.rm=TRUE)
hist(ML3$NFC)

#############REPORTED EFFORT###############
#1(no effort)-5(tried my hardest) scale
ML3$ReportedAttention<-ML3$pate_01
summary(ML3$ReportedAttention)
sd(ML3$ReportedAttention,na.rm=TRUE)
hist(ML3$ReportedAttention)

#############REPORTED ATTENTION###############
#1(none)-5(I have my undivided attention) scale
ML3$ReportedEffort<-ML3$pate_02
summary(ML3$ReportedEffort)
sd(ML3$ReportedEffort,na.rm=TRUE)
hist(ML3$ReportedEffort)

#############SELF-ESTEEM###############
#1(not very true of me)-7(very true of me) scale
#statement is: I have high self-esteem
ML3$SelfEsteem<-ML3$selfesteem_01
summary(ML3$SelfEsteem)
sd(ML3$SelfEsteem,na.rm=TRUE)
hist(ML3$SelfEsteem)

#############STRESS###############
#Higher scores indicate more stress
ML3$Stress<-(ML3$stress_01+(6-ML3$stress_02)+(6-ML3$stress_03)+ML3$stress_04)/4
summary(ML3$Stress)
sd(ML3$Stress,na.rm=TRUE)
hist(ML3$Stress)

##########################Demographics#################################
###See Variable Codebook###




########################COMPUTER BASED EFFECTS###########################

############Power and Perspective Taking##############

str(ML3$sarcasm)

##Labeling the conditions##
ML3$PowerCond[ML3$lowpower!="NA"]<-"LowPower"
ML3$PowerCond[ML3$highpower!="NA"]<-"HighPower"
list(ML3$PowerCond)
head(ML3$PowerCond)
str(ML3$PowerCond)
ML3$PowerCond<-as.factor(ML3$PowerCond)

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

######################PRINTING DATA FOR EFFECT#####################
head(ML3)
head(PowerData)
PowerPerspective<-PowerData[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","sarcasm","galinskyvignette_order","PowerCond","PowerText","TextLength")]
write.csv(PowerPerspective,file="PowerPerspectiveData.csv",row.names=FALSE)

#NOTE: Need to add date variables and participant pool info variables to this


###Replicating Previous Effect###
##sarcasm is DV, lower numbers = more sarcastic
#Following the original study, all participants with data on the dependent variable will be included in the analysis. An independent samples t-test will be conducted comparing the sarcastic-sincere ratings for participants in the high-power and low-power conditions.

t.test(sarcasm~PowerCond,data=PowerData,var.equal=TRUE)

###Planned follow-up analyses###
#As a secondary analysis for this project, we will investigate whether the length of participants’ responses to the power prime (as measured by the number of characters in their response) moderates this effect. 

sarcasm.length.inter.lm<-lm(sarcasm~PowerCond*TextLength,data=PowerData)
Anova(sarcasm.length.inter.lm,type="II")

###DESCRIPTIVES###
summaryBy(sarcasm~PowerCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
#Note-those in NA row did not write anything for the manipulated prompt

summaryBy(TextLength~PowerCond,data=PowerData,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

############Moral Credentialing##############

##Looking at number of statements endorsed
str(ML3$mcmost1)

ML3$MostEndorse<-(ML3$mcmost1+ML3$mcmost2+ML3$mcmost3+ML3$mcmost4+ML3$mcmost5)-5
list(ML3$MostEndorse)
##For this variable, a score of 5 means that a participant rejected every statement

ML3$SomeEndorse<-(ML3$mcsome1+ML3$mcsome2+ML3$mcsome3+ML3$mcsome4+ML3$mcsome5)-5
list(ML3$SomeEndorse)

#Manipulation Check
t.test(ML3$MostEndorse,ML3$SomeEndorse)


##Labeling the conditions##
ML3$CredCond[ML3$MostEndorse!="NA"]<-"Credentials"
ML3$CredCond[ML3$SomeEndorse!="NA"]<-"NoCredentials"
str(ML3$CredCond)
ML3$CredCond<-as.factor(ML3$CredCond)

ML3$Genderfactor[ML3$gender==1]<-"Female"
ML3$Genderfactor[ML3$gender==2]<-"Male"
list(ML3$Genderfactor)


######################PRINTING DATA FOR EFFECT#####################
head(ML3)
head(Order)
MoralCredentials<-ML3[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","mcdv1","mcdv2","MostEndorse","SomeEndorse","moninvignette_order","CredCond")]
write.csv(MoralCredentials,file="MoralCredentialsData.csv",row.names=FALSE)

#NOTE: Need to add date variables and participant pool info variables to this


###Replicating Previous Effect###

list(ML3$mcdv1)
str(ML3$mcdv1)
##More negative score means that job is better for women, high score means better for men

#Following the original study, all participants with data in the first dependent item will be included in the analysis.  The primary effect of interest for this replication is the condition (some/most statements) x gender (male/female) interaction, with an expected difference between conditions among males only.  Participants’ responses to the first dependent measure question will be subjected to a 2x2 factorial ANOVA.

Credential.inter.lm<-lm(mcdv1<-ML3$CredCond*ML3$Genderfactor)
Anova(Credential.inter.lm,type="II")


###Follow Up Analyses###
#The second dependent measure question is available for secondary analysis, using the same analysis plan as the primary dependent measure.

list(ML3$mcdv2)
##More negative score means strongly disagree that women can do any job a man can

Credential2nd.inter.lm<-lm(mcdv2<-ML3$CredCond*ML3$Genderfactor)
Anova(Credential2nd.inter.lm,type=III)

###DESCRIPTIVES###
summaryBy(mcdv1~CredCond*Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

summaryBy(mcdv2~CredCond*Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

############Self-Esteem and Subjective Distance##############

str(ML3$bestgrade2)
str(ML3$worstgrade2)

ML3$SubDistCond[ML3$bestgrade2!="NA"]<-"BestGrade"
ML3$SubDistCond[ML3$worstgrade2!="NA"]<-"WorstGrade"
list(ML3$SubDistCond)

###worst/bestgrade3 is DV
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

##Calculating number of months##
list(SESDdata$TimeSince)
SESDdata$TimeSince<-tolower(SESDdata$TimeSince)
SESDdata$TimeSince<-as.character(SESDdata$TimeSince)


years<-unlist(strsplit(SESDdata$TimeSince, " .*"))
years

###OLD TRIES
SESDdata$yearsesd<-gregexpr("[0-9]+",SESDdata$TimeSince)
SESDdata$YearNumber<-as.numeric(unlist(regmatches(SESDdata$TimeSince,SESDdata$yearsesd)))
SESDdata$year<-as.numeric(unlist(strsplit(SESDdata$TimeSince,"[^[:digit:]]")))
###END OLD TRIES


#Centering continuous predictor (months since class)
SESDdata$MonthsSince<-SESDdata$MonthsSince-mean(SESDdata$MonthsSince,na.rm=TRUE)

######################PRINTING DATA FOR EFFECT#####################
head(ML3)
head(Order)
SubjectiveDistance<-SESDdata[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","SubDist","bestgrade_order","worstgrade_order","SubDistCond")]
write.csv(SubjectiveDistance,file="SubjectiveDistanceData.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  Also need to finish months since variable

###Replicating Previous Effect###
#First, we will conduct stepwise regression analyses to examine the variables of interest. Subjective distance is the dependent variable. All continuous predictor variables will be centered. In the first step, we will enter a variable that indicates actual time, which is calculated from the number of months since the participant’s course ended. In the second step, grade condition (best vs. worst grade) and self-esteem are entered simultaneously. In the last step, the interaction between grade condition (best vs. worst grade) and self-esteem will be entered.  Follow-up tests will clarify the nature of the interaction effect.

SESDmodel1.lm<-lm(SubDist~MonthsSince,data=SESDdata)
SESDmodel2.lm<-lm(SubDist~MonthsSince+SubDistCond+selfesteem_01,data=SESDdata)
SESDmodel3.lm<-lm(SubDist~MonthsSince+SubDistCond+selfesteem_01+SubDistCond*selfesteem_01,data=SESDdata)
anova(SESDmodel1,SESDmodel2,SESDmodel3)

##Follow-up Analyses##
#Follow-up tests will clarify the nature of the interaction effect.
#To be completed once interaction is detected

###DESCRIPTIVES###
summaryBy(SubDist~SubDistCond,data=SESDdata,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

#################Availability Heuristic######################

######################PRINTING DATA FOR EFFECT#####################
head(ML3)
head(Order)
Availability<-ML3[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","kposition","lposition","nposition","rposition","vposition","kratio","lratio","nratio","rratio","vratio","availk_order","availl_order","availn_order","availr_order","availv_order")]
write.csv(Availability,file="AvailabilityData.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  Also need to finish ratios and order


head(ML3)
str(ML3$vposition)  #1=more often in first position, 2=more often in third position
sum(ML3$vposition==1,na.rm=TRUE)
sum(ML3$vposition==2,na.rm=TRUE)
list(ML3$vposition)

###Tallying number of times 1st position was judged to be more common
K1stTotal<-sum(ML3$kposition==1,na.rm=TRUE)
L1stTotal<-sum(ML3$Lposition==1,na.rm=TRUE)
N1stTotal<-sum(ML3$nposition==1,na.rm=TRUE)
R1stTotal<-sum(ML3$rposition==1,na.rm=TRUE)
V1stTotal<-sum(ML3$vposition==1,na.rm=TRUE)

###Tallying number of times 3rd position was judged to be more common
K3rdTotal<-sum(ML3$kposition==2,na.rm=TRUE)
L3rdTotal<-sum(ML3$Lposition==2,na.rm=TRUE)
N3rdTotal<-sum(ML3$nposition==2,na.rm=TRUE)
R3rdTotal<-sum(ML3$rposition==2,na.rm=TRUE)
V3rdTotal<-sum(ML3$vposition==2,na.rm=TRUE)

###Calculating total times 1st postion was judged to be more common
FirstSelected<-K1stTotal+L1stTotal+N1stTotal+R1stTotal+V1stTotal

###Calculating total responses
TotalResponses<-K1stTotal+L1stTotal+N1stTotal+R1stTotal+V1stTotal+K3rdTotal+L3rdTotal+N3rdTotal+R3rdTotal+V3rdTotal

###Assigning Sign 
str(ML3$FirstSelected)
ML3$K1st[ML3$kposition==1]<-0
ML3$L1st[ML3$lposition==1]<-0
ML3$N1st[ML3$nposition==1]<-0
ML3$R1st[ML3$rposition==1]<-0
ML3$V1st[ML3$vposition==1]<-0
ML3$K1st[ML3$kposition==2]<-1
ML3$L1st[ML3$lposition==2]<-1
ML3$N1st[ML3$nposition==2]<-1
ML3$R1st[ML3$rposition==2]<-1
ML3$V1st[ML3$vposition==2]<-1
ML3$K1st<-as.integer(ML3$K1st)
ML3$L1st<-as.integer(ML3$L1st)
ML3$N1st<-as.integer(ML3$N1st)
ML3$R1st<-as.integer(ML3$R1st)
ML3$V1st<-as.integer(ML3$V1st)
list(ML3$K1st)
str(ML3$K1st)
list(ML3$L1st)
str(ML3$L1st)
list(ML3$N1st)
str(ML3$N1st)
list(ML3$R1st)
str(ML3$R1st)
list(ML3$V1st)
str(ML3$V1st)

ML3$AvailFirst<-ML3$K1st+ML3$L1st+ML3$N1st+ML3$R1st+ML3$V1st
list(ML3$AvailFirst)
ML3$AvailSign[ML3$AvailFirst==0]<-"-"
ML3$AvailSign[ML3$AvailFirst==1]<-"-"
ML3$AvailSign[ML3$AvailFirst==2]<-"-"
ML3$AvailSign[ML3$AvailFirst==3]<-"+"
ML3$AvailSign[ML3$AvailFirst==4]<-"+"
ML3$AvailSign[ML3$AvailFirst==5]<-"+"
list(ML3$AvailSign)

PlusTotal<-sum(ML3$AvailSign=="+",na.rm=TRUE)
MinusTotal<-sum(ML3$AvailSign=="-",na.rm=TRUE)
PlusTotal
MinusTotal

###Calculating Ratios (UNDER CONSTRUCTION)
str(ML3$kratio)
list(ML3$kratio)
Kless<-subset(ML3,ML3$kratio<10 & ML3$vratio>0)
list(Kless$kratio)
Kequal<-subset(ML3,ML3$kratio==10)
list(Kequal$kratio)
Kgreater<-subset(ML3,ML3$kratio>10)
list(Kgreater$kratio)

Lless<-subset(ML3,ML3$lratio<10 & ML3$vratio>0)
list(Lless$lratio)
Lequal<-subset(ML3,ML3$lratio==10)
list(Lequal$lratio)
Lgreater<-subset(ML3,ML3$lratio>10)
list(Lgreater$lratio)

Nless<-subset(ML3,ML3$nratio<10 & ML3$vratio>0)
list(Nless$nratio)
Nequal<-subset(ML3,ML3$nratio==10)
list(Nequal$nratio)
Ngreater<-subset(ML3,ML3$nratio>10)
list(Ngreater$nratio)

Rless<-subset(ML3,ML3$rratio<10 & ML3$vratio>0)
list(Rless$rratio)
Requal<-subset(ML3,ML3$rratio==10)
list(Requal$rratio)
Rgreater<-subset(ML3,ML3$rratio>10)
list(Rgreater$rratio)

Vless<-subset(ML3,ML3$vratio<10 & ML3$vratio>0)
list(Vless$vratio)
Vequal<-subset(ML3,ML3$vratio==10)
list(Vequal$vratio)
Vgreater<-subset(ML3,ML3$vratio>10)
list(Vgreater$vratio)

AvailLess<-rbind(Kless,Lless,Nless,Rless,Vless)
head(AvailLess)
AvailEqual<-rbind(Kequal,Lequal,Nequal,Requal,Vequal)
AvailGreater<-rbind(Kgreater,Lgreater,Ngreater,Rgreater,Vgreater)

AvailLess$kratio2<-AvailLess$kratio
AvailLess$lratio2<-AvailLess$lratio
AvailLess$nratio2<-AvailLess$nratio
AvailLess$rratio2<-AvailLess$rratio
AvailLess$vratio2<-AvailLess$vratio

AvailLess$kratio2<-1-10/AvailLess$kratio2
AvailLess$lratio2<-1-10/AvailLess$lratio2
AvailLess$nratio2<-1-10/AvailLess$nratio2
AvailLess$rratio2<-1-10/AvailLess$rratio2
AvailLess$vratio2<-1-10/AvailLess$vratio2
list(AvailLess$vratio2)

###NOTE: Having trouble here.  Consulting expert.




###Replicating Previous Effect###
#All participants with data will be included in the analysis.  We will conduct an analysis strategy very close to the original.  Participants who judge the first position to be more frequent for the majority of the letters will get a sign of +, and participants who judge the third position to be more frequent for the majority of the letters will get a sign of -. The number of +s and –s will be used in the sign test.

binom.test(PlusTotal,(PlusTotal+MinusTotal))


###Follow Up Analyses###
###NOTE TO SELF: come back and do ratios analyses (these are less concrete, there's a proposal for this in the protocol)
#With our focus on effect size and different response options, we also considering a follow-up analysis strategy with the ratio responses.  The following is our present plan for that analysis, but this will undergo more intensive review prior to observing the outcomes of the data collection.  The estimated number of occurrence of R in the third position (O3) compared to the first position is the primary variable for analysis.  We will recode the data for each response as follows:

#If O3 > 10, then SCORE = O3/10 - 1
#If O3 = 10, then SCORE = 0
#If O3 < 10, then SCORE = 1 - 10/O3 

#This rescales the ratio estimate to theoretically normalize the distribution and recenters on 0 indicating no difference in frequency estimates.  Then, the five ratings will be averaged to create a single index of relative estimation for first versus third letter.  It is conceivable that the distributions for these responses will be unusual and require some adjustment in data preparation prior to inferential testing. Using a t-test, the mean rating will be compared against zero indicating no difference in frequency estimates between first and third position.  Another possibility is to rescale the distribution for each letter so that 0 indicates the actual difference in frequency of 1st and 3rd position, with positive values indicating overestimation of 1st position and negative values indicating overestimation of 3rd position.  The latter would be more definitively an estimate of the magnitude of the misperception. 


##Effect of Order on Responses
#Finally, as a secondary test, we will examine the effect of order on responses.  Is the overestimation of 1st position more extreme for the first estimate compared to the others?  This was not examined in the original research, but it is possible that the effect is dampened with multiple assessments as participants have a meta-experience of realizing that not every letter could be more frequent in the first than third position.

###DESCRIPTIVES###
hist(ML3$kposition)
hist(ML3$lposition)
hist(ML3$nposition)
hist(ML3$rposition)
hist(ML3$vposition)

############Stroop##############

Stroop<-read.csv(file="stroop.csv",header=TRUE,stringsAsFactors=FALSE)
head(Stroop)
str(Stroop)

###Cleaning Stroop Data###

JustTest<-subset(Stroop,Stroop$task_name=='stroop')
head(JustTest)
list(JustTest$task_name)
str(JustTest)

trialword<-JustTest$trial_name
JustTest$trial_name<-strsplit(JustTest$trial_name,"c",fixed=TRUE)
JustTest$trial_name<-sub('c.*','',JustTest$trial_name)
JustTest$trial_name<-substr(JustTest$trial_name,nchar(JustTest$trial_name)-1)
?substr
trialword3
trialword3<-tolower(trialword3)

trialword<-JustTest$trial_name
trialword2<-sub('c.*','',trialword)
trialword2<-substr(trialword2,1,nchar(trialword2)-1)
trialword2
trialword2<-tolower(trialword3)
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

###making variable for congruent vs. incongruent trials
JustTest$block_pairing_definition==JustTest$trial_word
JustTest$congruent<-JustTest$block_pairing_definition==JustTest$trial_word
str(JustTest)
JustTest[JustTest$congruent=="TRUE",]$congruent="Congruent"
JustTest[JustTest$congruent=="FALSE",]$congruent="Incongruent"
list(JustTest$congruent)
str(JustTest$congruent)
JustTest$congruent<-as.factor(JustTest$congruent)

###Cleaning Latency Data###
Latency<-JustTest
head(Latency)
str(Latency)


###Cleaning data with Dan's dplyr IAT functions
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

meanReplace<-filter(myTblNotFast,TRIAL_ERROR==1) %>%
summarise(blockMean=mean(TRIAL_LATENCY)+600)
meanReplace

mergeTbl<-merge(myTblNotFast,meanReplace,by=c(SESSION_ID,CONGRUENT),all=TRUE)
myTblNotFast$tmpLatency<-myTblNotFast$TRIAL_LATENCY
names(myTblNotFast)[c(14,18)]<-c("oldLatency",trialLatency)
blockMeans<-summarise(myTblNotFast,M=mean(TRIAL_LATENCY),N=length(TRIAL_LATENCY))
blockMeans<-summarise(myTblNotFast,M=mean(TRIAL_LATENCY),N=length(TRIAL_LATENCY))

blockSDs<-summarise(myTblNotFast,S=sd(TRIAL_LATENCY),N=length(TRIAL_LATENCY))

tblResult<-blockMeans


###Replicating Previous Effect###
#NOTE: This part is not ready yet.  Consulting stats expert.

###Follow Up Analyses###
#analyze by error rates


############Warmer Hearts and Rooms##############

head(ML3)
###tempest1 is temperature estimate

#converting all to F
if(ML3$Site==c("UniversityOfToronto","CarletonUniversity"))ML3$tempest1<-ML3$tempest1*(9/5)+32

###Eliminating estimates higher than 95, lower than 50
str(ML3$tempest1)
ML3$tempest1<-as.integer(ML3$tempest1)
tempclean1<-subset(ML3,ML3$tempest1<95)
TempClean<-subset(tempclean1,tempclean1$tempest1>50)
head(TempClean)

###Assigning Conditions###
TempClean$TempCond4[TempClean$tempcomm_order!="NA"]<-"CommunalMale"
TempClean$TempCond4[TempClean$tempcomf_order!="NA"]<-"CommunalFemale"
TempClean$TempCond4[TempClean$tempagenm_order!="NA"]<-"AgenticMale"
TempClean$TempCond4[TempClean$tempagenf_order!="NA"]<-"AgenticFemale"
list(TempClean$TempCond4)

TempClean$TempCond[TempClean$TempCond4=="CommunalMale"]<-"Communal"
TempClean$TempCond[TempClean$TempCond4=="CommunalFemale"]<-"Communal"
TempClean$TempCond[TempClean$TempCond4=="AgenticMale"]<-"Agentic"
TempClean$TempCond[TempClean$TempCond4=="AgenticFemale"]<-"Agentic"
head(TempClean)
list(TempClean$TempCond)

ML3$TargetGender[ML3$TempCond4=="CommunalMale"]<-"MaleTarget"
ML3$TargetGender[ML3$TempCond4=="CommunalFemale"]<-"FemaleTarget"
ML3$TargetGender[ML3$TempCond4=="AgenticMale"]<-"MaleTarget"
ML3$TargetGender[ML3$TempCond4=="AgenticFemale"]<-"FemaleTarget"
list(ML3$TargetGender)

######################PRINTING DATA FOR EFFECT#####################
WarmerHearts<-TempClean[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","tempest1","TempCond4","TempCond","TargetGender","tempestimate_order","Temperature")]
write.csv(TempClean,file="WarmerHeartsData.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  Temperature from in-lab

###Replicating Previous Effect###
#The original study did not exclude extreme data points.  However, on the suggestion of an original author, we defined extreme boundaries for data removal.  Participants who estimate the temperature as being higher than 95 degrees Fahrenheit (35 degrees Celsius) or lower than 50 degrees Fahrenheit (10 degrees Celsius) will be removed prior to descriptive and inferential analyses.  An independent samples t-test will test the difference in temperature estimates between the communal and agentic conditions.

t.test(tempest1~TempCond,data=TempClean,var.equal=TRUE)

###Follow Up Analyses###
#(make cleaned data match these labels)
#Secondary analyses will be conducted with a multivariate model to test the effects of the manipulation when including additional predictors: target gender, participant gender, the actual temperature of the room, and the interaction between target and participant gender.

TempModel.lm<-lm(tempest1~Temperature+Genderfactor+TargetGender+Genderfactor*TargetGender,data=TempClean)
Anova(TempModel.lm,type="II")

###DESCRIPTIVES###
summaryBy(tempest1~TempCond,data=TempClean,FUN=list(mean,max,min,median,sd),na.rm=TRUE)


############Elaboration Likelihood Model##############

#creating index of argument quality (no reverse coding needed)
ML3$ArgumentQuality=(ML3$elm_01+ML3$elm_02+ML3$elm_03+ML3$elm_04+ML3$elm_05)/5

#creating index of Need for Cognition (#'s reverse coded)
ML3$NFC<-(ML3$nfc_01+(6-ML3$nfc_02)+(6-ML3$nfc_03)+ML3$nfc_04+ML3$nfc_05+(6-ML3$nfc_06))/6
ML3$NFCcenter<-ML3$NFC-mean(ML3$NFC,na.rm=TRUE)

#Labeling conditions
head(ML3)
ML3$ELMCond[ML3$elmstrong_order!="NA"]<-"1"
ML3$ELMCond[ML3$elmweak_order!="NA"]<-"-1"
list(ML3$ELMCond)

######################PRINTING DATA FOR EFFECT#####################
head(ML3)
ELMData<-ML3[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","ArgumentQuality","NFCcenter","ELMCond","elmques_order")]
write.csv(ELMData,file="ELMData.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  

###Replicating Previous Effect###
#The five items will be averaged as an index of argument quality.  In the original study, participants who scored in the upper or lower third of need for cognition were recruited from the available sample for the study and were labeled as being high or low on the trait for analysis.  We will use all participants and treat need for cognition as a continuous measure.  As such the key analysis will be a general linear model with condition (strong = 1 vs. weak = -1), need for cognition (mean centered), and their interaction predicting argument quality.  The key test is the interaction term to show that the effect of the manipulation is moderated by need for cognition.

ELM.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=ML3)
Anova(ELM.lm,type="II")

###DESCRIPTIVES###
summaryBy(ArgumentQuality~ELMCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

############Conscientiousness and Persistence##############

###adding persistence data###
PersistenceClean<-read.csv(file="ML3PersistenceClean.csv",header=TRUE)
head(PersistenceClean)
ML3<-merge(ML3,PersistenceClean,by="session_id",all.x=TRUE)
head(ML3)

##big5_03 and big5_08(reverse coded), 1-disagree strongly to 7-agree strongly scale
str(ML3$big5_03)
str(ML3$big5_08)

#Calculating Conscientiousness
ML3$Conscientiousness<-(ML3$big5_03+(8-ML3$big5_08))/2

######################PRINTING DATA FOR EFFECT#####################
head(ML3)
PersistenceConscientiousness<-ML3[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","Persistence","anagrams_order","bigfive_order")]
write.csv(PersistenceConscientiousness,file="PersistenceConscientiousness Data.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  


##########################IN-LAB EFFECTS#################################


############Metaphoric Restructuring##############

###Removing those who failed the priming manipulation
PrimeCorrect<-subset(ML3,ML3$SR T/F Correct=="Yes")

###Removing those who did not answer Monday or Friday
Monday<-subset(ML3,ML3$SR Meeting Response=="Monday")
Friday<-subset(ML3,ML3$SR Meeting Response=="Friday")
MonFri<-rbind(Monday,Friday)

#Separating Control from Priming Conditions, and labeling responses for priming conditions as PrimeConsistent or PrimeInconsistent
SRControl<-subset(MonFri,MonFri$SRCond=="C")
SRA<-subset(MonFri,MonFri$SRCond=="A")
SRB<-subset(MonFri,MonFri$SRCond=="B")

SRA$PrimeCons[SRA$SR Meeting Response=="Friday"]<-"Consistent"
SRA$PrimeCons[SRA$SR Meeting Response=="Monday"]<-"Inconsistent"
SRB$PrimeCons[SRB$SR Meeting Response=="Monday"]<-"Consistent"
SRB$PrimeCons[SRB$SR Meeting Response=="Friday"]<-"Inconsistent"

SRPrime<-rbind(SRA,SRB)

###Removing those who failed the priming manipulation (priming conditions only)
SRPrimeCorrect<-subset(SRPrime,SRPrime$SRTrueFalse=="Yes")


######################PRINTING DATA FOR EFFECT#####################
head(ML3)
MetaphoricRestructuring<-ML3[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","inlab_order","Order of Tasks","SR T/F Correct","SR Meeting Response","SR Confidence Response")]
write.csv(MetaphoricRestructuring,file="MetaphoricRestructuring Data.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  Also inlab stuff.

###Replicating Previous Effect###
###Testing effect of prime on consistent vs. inconsistent responding
#Participants who do not correctly answer all four priming questions will be removed from the analyses.  A two-way contingency table will be built with Prime condition (ego-moving vs. object-moving) and Response (Monday vs. Friday) as factors. The critical replication hypothesis will be given by a χ2 test between prime consistent and inconsistent responses, collapsing across priming condition.  In addition, a χ2 test will be conducted on the control condition in order to determine if there was a bias toward responding Monday or Friday in the absence of any prime. 

PrimeTable=table(SRPrimeCorrect$PrimeCons)
chisq.test(PrimeTable)
PrimeTable

###Testing bias toward Monday or Friday in control
ControlTable=table(SRControl$SRMeetingResponse)
chisq.test(ControlTable)
ControlTable

###DESCRIPTIVES###
hist(SRPrimeCorrect$PrimeCons)
hist(SRControl$SR Meeting Response)

############Weight as Embodied Importance##############

#Adding Clipboard Material
if(ML3$Site==c("UniversityOfVirginia","MiamiUniversity","UCDavis")){ML3$ClipBoardMaterial<-"Metal"}else{ML3$ClipBoardMaterial<-"Plastic"}

######################PRINTING DATA FOR EFFECT#####################
head(ML3)
WeightImportance<-ML3[,c("session_id","age","gender","Genderfactor","major","year","AttentionCheck","Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Intrinsic","Mood","NFC","ReportedAttention","ReportedEffort","SelfEsteem","Stress","inlab_order","Order of Tasks","Clipboard Weight","ClipboardMaterial","II Response")]
write.csv(WeightImportance,file="WeightImportanceData.csv",row.names=FALSE)

#NOTE: Need to add Participant_ID, date variables, and participant pool info variables to this.  Also inlab stuff.

###Replicating Previous Effect###
#Following the original study, all participants answering the dependent variable will be included in the analysis. The differences between the two groups will be measured using an independent samples t-test.

t.test(IIResponse~ClipboardWeight,data=ML3,var.equal=TRUE)

##Follow-up tests
#The material of the clipboard (metal or plastic) will be added to the model to determine if the clipboard’s composition influenced the strength of the effect.

II.material.inter.lm<-lm(II Response~ClipBoard Weight*ClipBoardMaterial,data=ML3)

###DESCRIPTIVES###
summaryBy(II Response~ClipBoardWeight*ClipBoard Material,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

##########################Time of Semester Vatiation#####################






##########################Order Effects#################################




#########################Printing Current Data Set#####################
head(ML3)
?write.csv
write.csv(ML3,file="ML3updated.csv",row.names=FALSE)

