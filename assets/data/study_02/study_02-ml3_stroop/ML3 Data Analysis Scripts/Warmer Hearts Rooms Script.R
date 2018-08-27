#############MANY LABS 3 WARMER HEARTS WARMER ROOMS SCRIPT#################
#######Charlie Ebersole, Begin January 21, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,na.strings=c("","NA"),stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(stringr)
require(compute.es)
require(dplyr)

#Reformatting temperature estimate
str(ML3$tempest1)
list(ML3$tempest1)
regexp<-"[[:digit:]]+"
ML3$tempestclean<-str_extract(ML3$tempest1,regexp)
list(ML3$tempestclean)
str(ML3$tempestclean)
ML3$tempestclean<-as.numeric(ML3$tempestclean)

#Checking other variables
str(ML3$TempCond)
ML3$TempCond<-as.factor(ML3$TempCond)
list(levels(ML3$TempCond))
str(ML3$TargetGender)
ML3$TargetGender<-as.factor(ML3$TargetGender)
str(ML3$Genderfactor)
ML3$Genderfactor<-as.factor(ML3$Genderfactor)
str(ML3$Temperatureinlab)
ML3$Temperatureinlab<-as.numeric(ML3$Temperatureinlab)

###Eliminating estimates higher than 95, lower than 50
tempclean1<-subset(ML3,ML3$tempestclean<95)
TempClean<-subset(tempclean1,tempclean1$tempestclean>50)
head(TempClean)
range(TempClean$tempestclean)
str(TempClean$tempestclean)
length(TempClean$tempestclean)
list(TempClean$tempestclean)

Communal<-subset(TempClean,TempClean$TempCond=="Communal")
length(Communal$session_id)
Agentic<-subset(TempClean,TempClean$TempCond=="Agentic")
length(Agentic$session_id)

###Replicating Previous Effect###
#The original study did not exclude extreme data points.  However, on the suggestion of an original author, we defined extreme boundaries for data removal.  Participants who estimate the temperature as being higher than 95 degrees Fahrenheit or lower than 50 degrees Fahrenheit will be removed prior to descriptive and inferential analyses.  An independent samples t-test will test the difference in temperature estimates between the communal and agentic conditions.

t.test(tempestclean~TempCond,data=TempClean,var.equal=TRUE)
summaryBy(tempestclean~TempCond,data=TempClean,FUN=list(length))
ci.smd(ncp=-0.2181,n.1=1515,n.2=1604,conf.level=.95)
tes(-0.2181,1515,1604)
tes(0.2181,1515,1604,level=99)


#Checking mTurk Numbers
mTurk<-subset(TempClean,TempClean$Site=="mTurk")
t.test(tempestclean~TempCond,data=mTurk,var.equal=TRUE)
3119-575
###Follow Up Analyses###
#(make cleaned data match these labels)
#Secondary analyses will be conducted with a multivariate model to test the effects of the manipulation when including additional predictors: target gender, participant gender, the actual temperature of the room, and the interaction between target and participant gender.

TempModel.lm<-lm(tempestclean~TempCond+TargetGender+Genderfactor+Temperatureinlab,data=TempClean)
Anova(TempModel.lm,type="II")
TempModel.inter.lm<-lm(tempestclean~TempCond+TargetGender+Genderfactor+Temperatureinlab+TargetGender*Genderfactor,data=TempClean)
anova(TempModel.lm,TempModel.inter.lm)
Anova(TempModel.inter.lm,type="II")

###DESCRIPTIVES###
summaryBy(tempestclean~TempCond,data=TempClean,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
ci.smd(ncp=-0.2181,n.1=1500,n.2=1469,conf.level=.95)

ggplot(TempClean,aes(x= tempestclean,fill= TempCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")



##############################Supplementary Analyses#######################################
###Controlling for Lab Temp
TempControl.lm<-lm(tempestclean~TempCond+Temperatureinlab,data=TempClean)
Anova(TempControl.lm,type="II")

str(TempClean$Site)
TempClean$Site<-as.factor(TempClean$Site)
TempControl.lm<-lm(tempestclean~TempCond+Temperatureinlab+Site,data=TempClean)
Anova(TempControl.lm,type="II")

TempControl.lm<-lm(tempestclean~Temperatureinlab+Site+TempCond,data=TempClean)
Anova(TempControl.lm,type="II")
anova(TempControl.lm)

AttentionPass<-subset(TempClean,TempClean$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication
t.test(tempestclean~TempCond,data=AttentionPass,var.equal=TRUE)

#Planned follow-up analyses
TempModel2.lm<-lm(tempestclean~TempCond+TargetGender+Genderfactor+Temperatureinlab,data= AttentionPass)
Anova(TempModel2.lm,type="II")
plot(allEffects(TempModel2.lm))
TempModel2.inter.lm<-lm(tempestclean~TempCond+TargetGender+Genderfactor+Temperatureinlab+TargetGender*Genderfactor,data=AttentionPass)
anova(TempModel2.lm,TempModel2.inter.lm)

###This Task First
head(TempClean)
Agenm1st<-subset(TempClean,TempClean$tempagenm_order==2)
Agenf1st<-subset(TempClean,TempClean$tempagenf_order==2)
Comm1st<-subset(TempClean,TempClean$tempcomm_order==2)
Comf1st<-subset(TempClean,TempClean$tempcomf_order==2)

TempFirst<-rbind(Agenm1st,Agenf1st,Comm1st,Comf1st)

t.test(tempestclean~TempCond,data=TempFirst,var.equal=TRUE)
summaryBy(tempestclean~TempCond,data=TempFirst,FUN=list(length))
tes(.6997,213,207)

##############################Exploratory Analyses#######################################
str(ML3$DaysInComp)

Hearts.Time.lm<-lm(tempestclean~Temperatureinlab+TempCond*DaysInComp,data=TempClean)
Anova(Hearts.Time.lm,type="II")
plot(allEffects(Hearts.Time.lm))

LastFifth<-subset(TempClean,TempClean$DaysInComp>.8)
t.test(tempestclean~TempCond,data=LastFifth,var.equal=TRUE)
Hearts.Fifth.lm<-lm(tempestclean~Temperatureinlab+TempCond,data=LastFifth)
Anova(Hearts.Fifth.lm,type="II")


###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(TempClean,TempClean$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)

#First 80
t.test(tempestclean~TempCond,data=First80,var.equal=TRUE)
summaryBy(tempestclean~TempCond,data=First80,FUN=list(length))
tes(1.0322,978,991)
978+991

#Last 20
t.test(tempestclean~TempCond,data=Last20,var.equal=TRUE)
summaryBy(tempestclean~TempCond,data=Last20,FUN=list(length))
tes(0.0475,255,320)
255+320


#####Mixed Models####
str(Pool$DaysInComp)

#Unconditional Model
tempestclean.Uncond<-lmer(tempestclean~1+(1|Site),data=Pool)
summary(tempestclean.Uncond)
5.211/(5.211 + 18.529)

#Full Model
tempestclean.MEmodel<-lmer(tempestclean~TempCond*DaysInComp+(1+DaysInComp|Site),data=Pool)
summary(tempestclean.MEmodel)

#Model Comparison
tempestclean.MEmodel.null<-lmer(tempestclean~TempCond+(1+DaysInComp|Site),data=Pool,REML=FALSE)
tempestclean.MEmodel.test<-lmer(tempestclean~TempCond*DaysInComp+(1+DaysInComp|Site),data=Pool,REML=FALSE)
anova(tempestclean.MEmodel.null,tempestclean.MEmodel.test)
coef(tempestclean.MEmodel.test)

###Exploratory Follow Up
Temp.lm<-lm(tempestclean~TempCond*DaysInComp,data=Pool)
Anova(Temp.lm,type="II")
plot(allEffects(Temp.lm))
summary(Temp.lm)
ci.pvaf(F.value= 2.9468,df.1=1,df.2=2540,N=2544,conf.level=.95)



###############Moderator and Order Analyses#################

###Moderators
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(tempestclean~TempCond*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(tempestclean~TempCond*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(tempestclean~TempCond*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(tempestclean~TempCond*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(tempestclean~TempCond*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(tempestclean~TempCond*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 4.2881,df.1=1,df.2=2497,N=2501,conf.level=.95)
plot(allEffects(Mood))

Happy<-subset(Pool,Mood>4)
t.test(tempestclean~TempCond,data=Happy,var.equal=TRUE)
tes(2.7642,860,927)
summaryBy(tempestclean~TempCond,data=Happy,FUN=list(length))
summaryBy(tempestclean~TempCond,data=Happy,FUN=list(mean,sd),na.rm=TRUE)

str(Pool$Stress)
Stress<-lm(tempestclean~TempCond*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)


mTurk<-subset(TempClean,Site=="mTurk")
mTurk.Warm<-lm(tempestclean~TempCond*DaysInComp,data=mTurk)
Anova(mTurk.Warm)
ci.pvaf(F.value= 0.8768,df.1=1,df.2=571,N=575,conf.level=.95)
plot(allEffects(mTurk.Warm))


#Summary Data
str(TempClean$TempCond)
TempSum<-summarise(group_by(TempClean,Site),CredentialN=sum(TempCond=="Agentic",na.rm=TRUE),NoCredentialN=sum(TempCond=="Communal",na.rm=TRUE),CredentialMean=mean(tempestclean[TempCond=="Agentic"],na.rm=TRUE),NoCredentialMean=mean(tempestclean[TempCond=="Communal"],na.rm=TRUE),AgenticD=sd(tempestclean[TempCond=="Agentic"],na.rm=TRUE),CommunalD=sd(tempestclean[TempCond=="Communal"],na.rm=TRUE))
TempSum
setwd("/Users/Charlie/Desktop")
write.csv(TempSum,file="TempSum.csv",row.names=FALSE)


###Effect by site


Ashland<-subset(TempClean,TempClean$Site=="AshlandUniversity")
length(Ashland$session_id)
t.test(tempestclean~TempCond,data=Ashland,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Ashland,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Ashland,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Ashland,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Ashland,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Ashland,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Ashland,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Ashland,var.equal=TRUE)$parameter+2)/2)

Bradley<-subset(TempClean,TempClean$Site=="BradleyUniversity")
length(Bradley$session_id)
t.test(tempestclean~TempCond,data=Bradley,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Bradley,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Bradley,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Bradley,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Bradley,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Bradley,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Bradley,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Bradley,var.equal=TRUE)$parameter+2)/2)


Carleton<-subset(TempClean,TempClean$Site=="CarletonUniversity")
length(Carleton$session_id)
t.test(tempestclean~TempCond,data=Carleton,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Carleton,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Carleton,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Carleton,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Carleton,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Carleton,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Carleton,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Carleton,var.equal=TRUE)$parameter+2)/2)


Ithaca<-subset(TempClean,TempClean$Site=="IthacaCollege")
length(Ithaca$session_id)
t.test(tempestclean~TempCond,data=Ithaca,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Ithaca,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Ithaca,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Ithaca,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Ithaca,var.equal=TRUE)$parameter+2)/2)

Miami<-subset(TempClean,TempClean$Site=="MiamiUniversity")
length(Miami$session_id)
t.test(tempestclean~TempCond,data=Miami,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Miami,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Miami,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Miami,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Miami,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Miami,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Miami,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Miami,var.equal=TRUE)$parameter+2)/2)

MichSt<-subset(TempClean,TempClean$Site=="MichiganStateUniversity")
length(MichSt$session_id)
t.test(tempestclean~TempCond,data=MichSt,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= MichSt,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= MichSt,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= MichSt,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=MichSt,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= MichSt,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= MichSt,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= MichSt,var.equal=TRUE)$parameter+2)/2)




Montana<-subset(TempClean,TempClean$Site=="MontanaStateUniversity")
length(Montana$session_id)
t.test(tempestclean~TempCond,data=Montana,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Montana,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Montana,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Montana,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Montana,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Montana,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Montana,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Montana,var.equal=TRUE)$parameter+2)/2)

Nova<-subset(TempClean,TempClean$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
t.test(tempestclean~TempCond,data=Nova,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Nova,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Nova,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Nova,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Nova,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Nova,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Nova,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Nova,var.equal=TRUE)$parameter+2)/2)


OSU<-subset(TempClean,TempClean$Site=="OSUNewark")
length(OSU$session_id)
t.test(tempestclean~TempCond,data=OSU,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= OSU,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= OSU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= OSU,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=OSU,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= OSU,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= OSU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= OSU,var.equal=TRUE)$parameter+2)/2)

PLU<-subset(TempClean,TempClean$Site=="PacificLutheranUniversity")
length(PLU$session_id)
t.test(tempestclean~TempCond,data=PLU,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= PLU,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= PLU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= PLU,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=PLU,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= PLU,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= PLU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= PLU,var.equal=TRUE)$parameter+2)/2)


Penn<-subset(TempClean,TempClean$Site=="PennStateAbington")
length(Penn$session_id)
t.test(tempestclean~TempCond,data=Penn,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Penn,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Penn,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Penn,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Penn,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Penn,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Penn,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Penn,var.equal=TRUE)$parameter+2)/2)

SDSU<-subset(TempClean,TempClean$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
t.test(tempestclean~TempCond,data=SDSU,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= SDSU,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= SDSU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= SDSU,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=SDSU,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= SDSU,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= SDSU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= SDSU,var.equal=TRUE)$parameter+2)/2)


Texas<-subset(TempClean,TempClean$Site=="TexasAandM")
length(Texas$session_id)
t.test(tempestclean~TempCond,data=Texas,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Texas,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Texas,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Texas,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Texas,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Texas,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Texas,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Texas,var.equal=TRUE)$parameter+2)/2)


Davis<-subset(TempClean,TempClean$Site=="UCDavis")
length(Davis$session_id)
t.test(tempestclean~TempCond,data=Davis,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Davis,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Davis,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Davis,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Davis,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Davis,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Davis,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Davis,var.equal=TRUE)$parameter+2)/2)

Riverside<-subset(TempClean,TempClean$Site=="UCRiverside")
length(Riverside$session_id)
t.test(tempestclean~TempCond,data=Riverside,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Riverside,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Riverside,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Riverside,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Riverside,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Riverside,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Riverside,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Riverside,var.equal=TRUE)$parameter+2)/2)


Florida<-subset(TempClean,TempClean$Site=="UniversityOfFlorida")
length(Florida$session_id)
t.test(tempestclean~TempCond,data=Florida,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Florida,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Florida,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Florida,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Florida,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Florida,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Florida,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Florida,var.equal=TRUE)$parameter+2)/2)


Mississippi<-subset(TempClean,TempClean$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
t.test(tempestclean~TempCond,data=Mississippi,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Mississippi,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Mississippi,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Mississippi,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Mississippi,var.equal=TRUE)$parameter+2)/2)

Toronto<-subset(TempClean,TempClean$Site=="UniversityOfToronto")
length(Toronto$session_id)
t.test(tempestclean~TempCond,data=Toronto,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Toronto,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Toronto,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Toronto,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=Toronto,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Toronto,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Toronto,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Toronto,var.equal=TRUE)$parameter+2)/2)


Virginia<-subset(TempClean,TempClean$Site=="UniversityOfVirginia")
length(Virginia$session_id)
t.test(tempestclean~TempCond,data= Virginia,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= Virginia,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= Virginia,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Virginia,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data= Virginia,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= Virginia,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= Virginia,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= Virginia,var.equal=TRUE)$parameter+2)/2)

VCU<-subset(TempClean,TempClean$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
t.test(tempestclean~TempCond,data=VCU,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= VCU,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= VCU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= VCU,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=VCU,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= VCU,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= VCU,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= VCU,var.equal=TRUE)$parameter+2)/2)


mTurk<-subset(TempClean,TempClean$Site=="mTurk")
length(mTurk$session_id)
t.test(tempestclean~TempCond,data=mTurk,var.equal=TRUE)
(tes(t.test(tempestclean~TempCond,data= mTurk,var.equal=TRUE)$statistic,( t.test(tempestclean~TempCond,data= mTurk,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= mTurk,var.equal=TRUE)$parameter+2)/2))

t.test(tempestclean~TempCond,data=mTurk,var.equal=FALSE)
tes(t.test(tempestclean~TempCond,data= mTurk,var.equal=FALSE)$statistic,( t.test(tempestclean~TempCond,data= mTurk,var.equal=TRUE)$parameter+2)/2, ( t.test(tempestclean~TempCond,data= mTurk,var.equal=TRUE)$parameter+2)/2)

#####Task Order Effects####
head(TempClean)
str(TempClean$tempestimate_order)
Order.lm<-lm(tempestclean~TempCond*tempestimate_order,data=TempClean)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.0181,df.1=1,df.2= 3115,N=3119,conf.level=.95)

TempClean$tempestimate_ordersquare<-TempClean$tempestimate_order^2
OrderQuad.lm<-lm(tempestclean~TempCond*tempestimate_ordersquare,data=TempClean)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.0119,df.1=1,df.2= 3115,N=3119,conf.level=.95)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
TempClean<-merge(TempClean,Order2,by="session_id",all=TRUE)
head(TempClean)

str(TempClean$WarmthOrder10)
Order.lm<-lm(tempestclean~TempCond*WarmthOrder10,data=TempClean)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.0490,df.1=1,df.2= 3115,N=3119,conf.level=.95)

TempClean$WarmthOrder10square<-TempClean$WarmthOrder10^2
OrderQuad.lm<-lm(tempestclean~TempCond*WarmthOrder10square,data=TempClean)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.1243,df.1=1,df.2= 3115,N=3119,conf.level=.95)


