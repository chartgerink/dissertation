#############MANY LABS 3 POWER AND PERSPECTIVE TAKING##################
#######Charlie Ebersole, Begin January 21, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)
length(unique(ML3$session_id))

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(lme4)
require(MBESS)
require(compute.es)
require(dplyr)
require(metafor)

###Splitting by condition
LowPower<-subset(ML3,ML3$PowerCond=="LowPower")
HighPower<-subset(ML3,ML3$PowerCond=="HighPower")

LowPower$PowerText<-LowPower$lowpower
HighPower$PowerText<-HighPower$highpower
PowerData<-rbind(LowPower,HighPower)
head(PowerData)
tail(PowerData)
#Number of Participants Left
length(unique(PowerData$session_id))
length(unique(LowPower$session_id))
length(unique(HighPower$session_id))

str(PowerData$PowerText)
PowerData$TextLength<-nchar(PowerData$PowerText,allowNA=TRUE)
list(PowerData$TextLength)

#number Answering DV
PowerDV<-subset(PowerData,PowerData$sarcasm!="NA")
length(unique(PowerDV$session_id))
summaryBy(sarcasm~PowerCond,data=PowerDV,FUN=list(length))

str(PowerData$PowerCond)
PowerData$PowerCond<-as.factor(PowerData$PowerCond)
str(PowerData$sarcasm)
str(PowerData$DaysInComp)
head(PowerData)

###Replicating Previous Effect###
##sarcasm is DV, lower numbers = more sarcastic
#Following the original study, all participants with data on the dependent variable will be included in the analysis. An independent samples t-test will be conducted comparing the sarcastic-sincere ratings for participants in the high-power and low-power conditions.

t.test(sarcasm~PowerCond,data=PowerData,var.equal=TRUE)
tes(-0.8946,1500,1469)
tes(0.8946,1500,1469,level=99)

#Checking mTurk Numbers
mTurk<-subset(PowerData,PowerData$Site=="mTurk")
t.test(sarcasm~PowerCond,data=mTurk,var.equal=TRUE)

###Planned follow-up analyses###
#As a secondary analysis for this project, we will investigate whether the length of participants’ responses to the power prime (as measured by the number of characters in their response) moderates this effect. 

sarcasm.length.inter.lm<-lm(sarcasm~PowerCond*TextLength,data=PowerData)
Anova(sarcasm.length.inter.lm,type="II")
summary(sarcasm.length.inter.lm)
plot(allEffects(sarcasm.length.inter.lm))

###DESCRIPTIVES###
summaryBy(sarcasm~PowerCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
#Note-those in NA row did not write anything for the manipulated prompt

ggplot(PowerData,aes(x=sarcasm,fill=PowerCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")

summaryBy(TextLength~PowerCond,data=PowerData,FUN=list(mean,max,min,median,sd),na.rm=TRUE)


##############################Supplementary Analyses#######################################
AttentionPass<-subset(PowerData,PowerData$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication
t.test(sarcasm~PowerCond,data= AttentionPass,var.equal=TRUE)

#Planned Follow-Up
sarcasm2.length.inter.lm<-lm(sarcasm~PowerCond*TextLength,data= AttentionPass)
Anova(sarcasm2.length.inter.lm,type="II")
summary(sarcasm2.length.inter.lm)
plot(allEffects(sarcasm2.length.inter.lm))


###This task first
Low1st<-subset(PowerData,PowerData$lowpower_order==2)
list(Low1st$lowpower_order)
High1st<-subset(PowerData,PowerData$highpower_order==2)
list(High1st$highpower_order)
PowerFirst<-rbind(Low1st,High1st)

t.test(sarcasm~PowerCond,data=PowerFirst,var.equal=TRUE)
summaryBy(sarcasm~PowerCond,data=PowerFirst,FUN=list(length))
tes(0.246,191,191)

##############################Exploratory Analyses#######################################
str(PowerData$DaysInComp)
Pool<-subset(PowerData,PowerData$Site!="mTurk")
length(unique(Pool$session_id))
str(Pool$PowerCond)
Pool$PowerCond<-as.factor(Pool$PowerCond)

Power.Time.lm<-lm(sarcasm~PowerCond*DaysInComp,data=Pool)
Anova(Power.Time.lm,type="II")
plot(allEffects(Power.Time.lm))

Time.Length.lm<-lm(TextLength~DaysInComp,data=Pool)
Anova(Time.Length.lm,type="II")
plot(allEffects(Time.Length.lm))
plot(Pool$DaysInComp,Pool$TextLength)

Time.Length.inter.lm<-lm(sarcasm~TextLength*DaysInComp,data=Pool)
Anova(Time.Length.inter.lm,type="II")
plot(allEffects(Time.Length.inter.lm))

###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(PowerData,PowerData$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)

#First 80
t.test(sarcasm~PowerCond,data= First80,var.equal=TRUE)
tes(1.1682,945,914)

DV80<-subset(First80, First80$sarcasm!="NA")
length(unique(DV80$session_id))
summaryBy(sarcasm~PowerCond,data=DV80,FUN=list(length))

#Last 20
t.test(sarcasm~PowerCond,data= Last20,var.equal=TRUE)
tes(0.7544,252,274)

DV20<-subset(Last20, Last20$sarcasm!="NA")
length(unique(DV20$session_id))
summaryBy(sarcasm~PowerCond,data=DV20,FUN=list(length))

test<-lmer(sarcasm~PowerCond*DaysInComp+(1+DaysInComp|Site),data=PowerData)
summary(test)
plot(allEffects(test))
coef(test)
plot(test)


#####Mixed Models####
str(Pool$DaysInComp)

#Unconditional Model
sarcasm.Uncond<-lmer(sarcasm~1+(1|PowerCond/Site),data=Pool)
summary(sarcasm.Uncond)
0.0217/(0.0217+ 2.4516)
coef(sarcasm.Uncond)

#Full Model
sarcasm.MEmodel<-lmer(sarcasm~PowerCond*DaysInComp+(1|Site),data=Pool)
summary(sarcasm.MEmodel)

#Model Comparison
sarcasm.MEmodel.null<-lmer(sarcasm~PowerCond+(1|Site),data=Pool,REML=FALSE)
sarcasm.MEmodel.test<-lmer(sarcasm~PowerCond*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(sarcasm.MEmodel.null,sarcasm.MEmodel.test)
coef(sarcasm.MEmodel.test)


###############################Moderator and Order Analyses#################################

###Moderators
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(sarcasm~PowerCond*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(sarcasm~PowerCond*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(sarcasm~PowerCond*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(sarcasm~PowerCond*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(sarcasm~PowerCond*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(sarcasm~PowerCond*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-lm(sarcasm~PowerCond*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)


#Summary Data

PowerSum<-summarise(group_by(PowerData,Site),HighN=sum(PowerCond=="HighPower"),LowN=sum(PowerCond=="LowPower"),HighMean=mean(sarcasm[PowerCond=="HighPower"],na.rm=TRUE),LowMean=mean(sarcasm[PowerCond=="LowPower"],na.rm=TRUE),HighSD=sd(sarcasm[PowerCond=="HighPower"],na.rm=TRUE),LowSD=sd(sarcasm[PowerCond=="LowPower"],na.rm=TRUE))
PowerSum
setwd("/Users/Charlie/Desktop")
write.csv(PowerSum,file="PowerSum.csv",row.names=FALSE)

#Testing



?apply
ttest<-function(x=sarcasm,y=PowerCond){t.test(x~y,var.equal=TRUE)}
apply(PowerData,PowerData$Site,ttest)

test<-group_by(PowerData,Site)%>%t.test(sarcasm~PowerCond,data=PowerData,var.equal=TRUE)

head(Pool)
#Order
sarcasm.order<-lmer(sarcasm~1+(1|galinskyvignette_order),data=Pool)
summary(sarcasm.order)

sarcasm.order<-lmer(sarcasm~PowerCond+(1|galinskyvignette_order),data=Pool)
summary(sarcasm.order)


Site<-lm(sarcasm~PowerCond*Site,data=Pool)
Anova(Site,type="II")


#####Effect by Site#####
PowerData$Site<-as.factor(PowerData$Site)
list(levels(PowerData$Site))
sink()
Ashland<-subset(PowerData,PowerData$Site=="AshlandUniversity")
length(Ashland$session_id)
t.test(sarcasm~PowerCond,data=Ashland,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Ashland,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Ashland,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Ashland,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Ashland,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Ashland,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Ashland,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Ashland,var.equal=TRUE)$parameter+2)/2)

Bradley<-subset(PowerData,PowerData$Site=="BradleyUniversity")
length(Bradley$session_id)
t.test(sarcasm~PowerCond,data=Bradley,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Bradley,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Bradley,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Bradley,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Bradley,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Bradley,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Bradley,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Bradley,var.equal=TRUE)$parameter+2)/2)


Carleton<-subset(PowerData,PowerData$Site=="CarletonUniversity")
length(Carleton$session_id)
t.test(sarcasm~PowerCond,data=Carleton,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Carleton,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Carleton,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Carleton,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Carleton,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Carleton,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Carleton,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Carleton,var.equal=TRUE)$parameter+2)/2)


Ithaca<-subset(PowerData,PowerData$Site=="IthacaCollege")
length(Ithaca$session_id)
t.test(sarcasm~PowerCond,data=Ithaca,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Ithaca,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Ithaca,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Ithaca,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2)

Miami<-subset(PowerData,PowerData$Site=="MiamiUniversity")
length(Miami$session_id)
t.test(sarcasm~PowerCond,data=Miami,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Miami,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Miami,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Miami,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Miami,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Miami,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Miami,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Miami,var.equal=TRUE)$parameter+2)/2)

MichSt<-subset(PowerData,PowerData$Site=="MichiganStateUniversity")
length(MichSt$session_id)
t.test(sarcasm~PowerCond,data=MichSt,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= MichSt,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= MichSt,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= MichSt,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=MichSt,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= MichSt,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= MichSt,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= MichSt,var.equal=TRUE)$parameter+2)/2)




Montana<-subset(PowerData,PowerData$Site=="MontanaStateUniversity")
length(Montana$session_id)
t.test(sarcasm~PowerCond,data=Montana,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Montana,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Montana,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Montana,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Montana,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Montana,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Montana,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Montana,var.equal=TRUE)$parameter+2)/2)

Nova<-subset(PowerData,PowerData$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
t.test(sarcasm~PowerCond,data=Nova,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Nova,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Nova,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Nova,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Nova,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Nova,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Nova,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Nova,var.equal=TRUE)$parameter+2)/2)


OSU<-subset(PowerData,PowerData$Site=="OSUNewark")
length(OSU$session_id)
t.test(sarcasm~PowerCond,data=OSU,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= OSU,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= OSU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= OSU,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=OSU,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= OSU,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= OSU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= OSU,var.equal=TRUE)$parameter+2)/2)

PLU<-subset(PowerData,PowerData$Site=="PacificLutheranUniversity")
length(PLU$session_id)
t.test(sarcasm~PowerCond,data=PLU,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= PLU,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= PLU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= PLU,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=PLU,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= PLU,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= PLU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= PLU,var.equal=TRUE)$parameter+2)/2)


Penn<-subset(PowerData,PowerData$Site=="PennStateAbington")
length(Penn$session_id)
t.test(sarcasm~PowerCond,data=Penn,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Penn,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Penn,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Penn,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Penn,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Penn,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Penn,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Penn,var.equal=TRUE)$parameter+2)/2)

SDSU<-subset(PowerData,PowerData$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
t.test(sarcasm~PowerCond,data=SDSU,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= SDSU,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= SDSU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= SDSU,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=SDSU,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= SDSU,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= SDSU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= SDSU,var.equal=TRUE)$parameter+2)/2)


Texas<-subset(PowerData,PowerData$Site=="TexasAandM")
length(Texas$session_id)
t.test(sarcasm~PowerCond,data=Texas,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Texas,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Texas,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Texas,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Texas,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Texas,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Texas,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Texas,var.equal=TRUE)$parameter+2)/2)


Davis<-subset(PowerData,PowerData$Site=="UCDavis")
length(Davis$session_id)
t.test(sarcasm~PowerCond,data=Davis,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Davis,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Davis,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Davis,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Davis,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Davis,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Davis,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Davis,var.equal=TRUE)$parameter+2)/2)

Riverside<-subset(PowerData,PowerData$Site=="UCRiverside")
length(Riverside$session_id)
t.test(sarcasm~PowerCond,data=Riverside,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Riverside,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Riverside,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Riverside,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Riverside,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Riverside,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Riverside,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Riverside,var.equal=TRUE)$parameter+2)/2)


Florida<-subset(PowerData,PowerData$Site=="UniversityOfFlorida")
length(Florida$session_id)
t.test(sarcasm~PowerCond,data=Florida,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Florida,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Florida,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Florida,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Florida,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Florida,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Florida,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Florida,var.equal=TRUE)$parameter+2)/2)


Mississippi<-subset(PowerData,PowerData$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
t.test(sarcasm~PowerCond,data=Mississippi,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Mississippi,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Mississippi,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Mississippi,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2)

Toronto<-subset(PowerData,PowerData$Site=="UniversityOfToronto")
length(Toronto$session_id)
t.test(sarcasm~PowerCond,data=Toronto,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Toronto,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Toronto,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Toronto,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=Toronto,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Toronto,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Toronto,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Toronto,var.equal=TRUE)$parameter+2)/2)


Virginia<-subset(PowerData,PowerData$Site=="UniversityOfVirginia")
length(Virginia$session_id)
t.test(sarcasm~PowerCond,data= Virginia,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= Virginia,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= Virginia,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Virginia,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data= Virginia,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= Virginia,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= Virginia,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= Virginia,var.equal=TRUE)$parameter+2)/2)

VCU<-subset(PowerData,PowerData$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
t.test(sarcasm~PowerCond,data=VCU,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= VCU,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= VCU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= VCU,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=VCU,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= VCU,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= VCU,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= VCU,var.equal=TRUE)$parameter+2)/2)


mTurk<-subset(PowerData,PowerData$Site=="mTurk")
length(mTurk$session_id)
t.test(sarcasm~PowerCond,data=mTurk,var.equal=TRUE)
(tes(t.test(sarcasm~PowerCond,data= mTurk,var.equal=TRUE)$statistic,( t.test(sarcasm~PowerCond,data= mTurk,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= mTurk,var.equal=TRUE)$parameter+2)/2))

t.test(sarcasm~PowerCond,data=mTurk,var.equal=FALSE)
tes(t.test(sarcasm~PowerCond,data= mTurk,var.equal=FALSE)$statistic,( t.test(sarcasm~PowerCond,data= mTurk,var.equal=TRUE)$parameter+2)/2, ( t.test(sarcasm~PowerCond,data= mTurk,var.equal=TRUE)$parameter+2)/2)


#####Task Order Effects####
head(PowerData)
str(PowerData$galinskyvignette_order)
Order.lm<-lm(sarcasm~PowerCond*galinskyvignette_order,data=PowerData)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 1.0908,df.1=1,df.2= 2965,N=2969,conf.level=.95)

PowerData$galinskyvignette_ordersquare<-PowerData$galinskyvignette_order^2
OrderQuad.lm<-lm(sarcasm~PowerCond*galinskyvignette_ordersquare,data=PowerData)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.9109,df.1=1,df.2= 2965,N=2969,conf.level=.95)


Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
PowerData<-merge(PowerData,Order2,by="session_id",all=TRUE)
head(PowerData)

str(PowerData$PowerOrder10)
Order.lm<-lm(sarcasm~PowerCond*PowerOrder10,data=PowerData)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 1.1651,df.1=1,df.2= 2965,N=2969,conf.level=.95)

PowerData$PowerOrder10square<-PowerData$PowerOrder10^2
OrderQuad.lm<-lm(sarcasm~PowerCond*PowerOrder10square,data=PowerData)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.5417,df.1=1,df.2= 2965,N=2969,conf.level=.95)







