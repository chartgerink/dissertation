#############MANY LABS 3 MORAL CREDENTIALS##################
#######Charlie Ebersole, Begin January 21, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(MBESS)
require(plyr)
require(dplyr)
require(lme4)


#Manipulation Check (higher number means that more statements were rejected)
t.test(ML3$MostEndorse,ML3$SomeEndorse,var.equal=TRUE)

#Number who completed manipulation in both conditions
MostComp<-subset(ML3,ML3$MostEndorse!=0)
length(MostComp$session_id)
SomeComp<-subset(ML3,ML3$SomeEndorse!=0)
length(SomeComp$session_id)
MCComp<-rbind(MostComp,SomeComp)


###Replicating Previous Effect###

list(ML3$mcdv1)
str(ML3$mcdv1)
##More negative score means that job is better for women, high score means better for men

#Number who answered DV
MCDVYes<-subset(MCComp,MCComp$mcdv1!="NA")
length(MCDVYes$session_id)
summaryBy(mcdv1~CredCond*Genderfactor,data=ML3,FUN=list(length))
summaryBy(mcdv1~CredCond,data=MCDVYes,FUN=list(length))

str(ML3$CredCond)
ML3$CredCond<-as.factor(ML3$CredCond)
str(ML3$Genderfactor)
ML3$Genderfactor<-as.factor(ML3$Genderfactor)

#Number who got this far and identified as Male or Female
Male<-subset(MCDVYes,MCDVYes$Genderfactor=="Male")
length(Male$session_id)
Female<-subset(MCDVYes,MCDVYes$Genderfactor=="Female")
length(Female$session_id)


#Following the original study, all participants with data in the first dependent item will be included in the analysis.  The primary effect of interest for this replication is the condition (some/most statements) x gender (male/female) interaction, with an expected difference between conditions among males only.  Participants’ responses to the first dependent measure question will be subjected to a 2x2 factorial ANOVA.

Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=ML3)
Anova(Credential.inter.lm,type="II")
plot(allEffects(Credential.inter.lm))

ci.pvaf(F.value=17.0098,df.1=1,df.2=3130,N=3134,conf.level=.95)
ci.pvaf(F.value= 0.0004,df.1=1,df.2=3130,N=3134,conf.level=.95)

ci.pvaf(F.value=17.0098,df.1=1,df.2=3130,N=3134,conf.level=.99)
ci.pvaf(F.value= 0.0004,df.1=1,df.2=3130,N=3134,conf.level=.99)

#checking mTurk
mTurk<-subset(ML3,ML3$Site=="mTurk")
Credential2.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=mTurk)
Anova(Credential2.inter.lm,type="II")

###Follow Up Analyses###
#The second dependent measure question is available for secondary analysis, using the same analysis plan as the primary dependent measure.  On this measure, higher numbers indicate agreement with the statement that women are as capabale as men.

list(ML3$mcdv2)
##More negative score means strongly disagree that women can do any job a man can

Credential2nd.inter.lm<-lm(mcdv2~CredCond*Genderfactor,data=ML3)
Anova(Credential2nd.inter.lm,type="II")
plot(allEffects(Credential2nd.inter.lm))

###DESCRIPTIVES###
summaryBy(mcdv1~CredCond*Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
summaryBy(mcdv1~CredCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

summaryBy(mcdv2~CredCond*Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
summaryBy(mcdv2~Genderfactor,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)

ggplot(ML3,aes(x=mcdv1,fill=CredCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")
ggplot(ML3,aes(x=mcdv1,fill=Genderfactor))+geom_histogram(binwidth=1,alpha=.5,position="identity")

ggplot(ML3,aes(x=mcdv2,fill=CredCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")
ggplot(ML3,aes(x=mcdv2,fill=Genderfactor))+geom_histogram(binwidth=1,alpha=.5,position="identity")


##############################Supplementary Analyses#######################################
AttentionPass<-subset(ML3,ML3$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication

Credential2.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data= AttentionPass)
Anova(Credential2.inter.lm,type="II")
plot(allEffects(Credential2.inter.lm))


###This task first
Some1st<-subset(ML3,ML3$mcsome_order==2)
list(Some1st$mcsome_order)
Most1st<-subset(ML3,ML3$mcmost_order==2)
list(Most1st$mcmost_order)
MCFirst<-rbind(Some1st,Most1st)

Credential3.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=MCFirst)
Anova(Credential3.inter.lm,type="II")
plot(allEffects(Credential3.inter.lm))

##############################Exploratory Analyses#######################################
str(ML3$DaysInComp)
Pool<-subset(ML3,ML3$Site!="mTurk")
length(unique(Pool$session_id))
str(Pool$CredCond)
Pool$CredCond<-as.factor(Pool$CredCond)
str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)

Credential.Time.inter.lm<-lm(mcdv1~CredCond*Genderfactor*DaysInComp,data=Pool)
Anova(Credential.Time.inter.lm,type="II")
plot(allEffects(Credential.Time.inter.lm))

Male<-subset(Pool,Pool$Genderfactor=="Male")
Male.MC.lm<-lm(mcdv1~CredCond*DaysInComp,data=Male)
Anova(Male.MC.lm,type="II")
plot(allEffects(Male.MC.lm))
Female<-subset(Pool,Pool$Genderfactor=="Female")

CredentialsM<-subset(Male,Male$CredCond=="Credentials")
MCDV.Male.lm<-lm(mcdv1~DaysInComp,data=CredentialsM)
Anova(MCDV.Male.lm,type="II")
plot(allEffects(MCDV.Male.lm))

FirstFifth<-subset(Pool,Pool$DaysInComp<.2)
LastFifth<-subset(Pool,Pool$DaysInComp>.8)

FirstFifth.lm<-lm(mcdv1~CredCond*Genderfactor,data=FirstFifth)
Anova(FirstFifth.lm,type="II")
plot(allEffects(FirstFifth.lm))

LastFifth.lm<-lm(mcdv1~CredCond*Genderfactor,data=LastFifth)
Anova(LastFifth.lm,type="II")
plot(allEffects(LastFifth.lm))

FirstHalf<-subset(Pool,Pool$DaysInComp<.5)
LastHalf<-subset(Pool,Pool$DaysInComp>.5)

FirstHalf.lm<-lm(mcdv1~CredCond*Genderfactor,data= FirstHalf)
Anova(FirstHalf.lm,type="II")
plot(allEffects(FirstHalf.lm))

LastHalf.lm<-lm(mcdv1~CredCond*Genderfactor,data=LastHalf)
Anova(LastHalf.lm,type="II")
plot(allEffects(LastHalf.lm))

###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(ML3,ML3$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)

#First 80
Credential80.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=First80)
Anova(Credential80.inter.lm,type="II")
ci.pvaf(F.value= 0.0412,df.1=1,df.2=1985,N=1989,conf.level=.95)
ci.pvaf(F.value= 14.1517,df.1=1,df.2=1985,N=1989,conf.level=.95)

#Last 20
Credential20.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Last20)
Anova(Credential20.inter.lm,type="II")
ci.pvaf(F.value= 0.2703,df.1=1,df.2=578,N=582,conf.level=.95)
ci.pvaf(F.value= 2.5294,df.1=1,df.2=578,N=582,conf.level=.95)

#####Mixed Models####
str(Pool$DaysInComp)

#Unconditional Model
mcdv1.Uncond<-lmer(mcdv1~1+(1|Site),data=Pool)
summary(mcdv1.Uncond)
0.002246/(0.002246 + 0.626816)

#Full Model
mcdv1.MEmodel<-lmer(mcdv1~CredCond*Genderfactor*DaysInComp+(1|Site),data=Pool)
summary(mcdv1.MEmodel)

#Model Comparison
mcdv1.MEmodel.null<-lmer(mcdv1~CredCond*Genderfactor+(1|Site),data=Pool,REML=FALSE)
mcdv1.MEmodel.test<-lmer(mcdv1~CredCond*Genderfactor*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(mcdv1.MEmodel.null,mcdv1.MEmodel.test)
coef(mcdv1.MEmodel.test)

###Added Main Effect
str(Pool$DaysInComp)

#Unconditional Model
mcdv1.Uncond<-lmer(mcdv1~1+(1|Site),data=Pool)
summary(mcdv1.Uncond)
0.002246/(0.002246 + 0.626816)

#Full Model
mcdv1.MEmodel<-lmer(mcdv1~CredCond*DaysInComp+(1|Site),data=Pool)
summary(mcdv1.MEmodel)

#Model Comparison
mcdv1.MEmodel.null<-lmer(mcdv1~CredCond+(1|Site),data=Pool,REML=FALSE)
mcdv1.MEmodel.test<-lmer(mcdv1~CredCond*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(mcdv1.MEmodel.null,mcdv1.MEmodel.test)
coef(mcdv1.MEmodel.test)

###############Moderator and Order Analyses#################

###Moderators Main Effect
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(mcdv1~CredCond*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(mcdv1~CredCond*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(mcdv1~CredCond*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(mcdv1~CredCond*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(mcdv1~CredCond*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(mcdv1~CredCond*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-lm(mcdv1~CredCond*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)


###Moderators Primary Effect
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(mcdv1~CredCond*Genderfactor*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(mcdv1~CredCond*Genderfactor*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(mcdv1~CredCond*Genderfactor*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(mcdv1~CredCond*Genderfactor*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(mcdv1~CredCond*Genderfactor*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(mcdv1~CredCond*Genderfactor*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-lm(mcdv1~CredCond*Genderfactor*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)
plot(allEffects(Stress))


#Summary Data
str(ML3$CredCond)
CredSum<-summarise(group_by(ML3,Site),CredentialN=sum(CredCond=="Credentials",na.rm=TRUE),NoCredentialN=sum(CredCond=="NoCredentials",na.rm=TRUE),CredentialMean=mean(mcdv1[CredCond=="Credentials"],na.rm=TRUE),NoCredentialMean=mean(mcdv1[CredCond=="NoCredentials"],na.rm=TRUE),CredentialSD=sd(mcdv1[CredCond=="Credentials"],na.rm=TRUE),NoCredentialSD=sd(mcdv1[CredCond=="NoCredentials"],na.rm=TRUE))
CredSum
setwd("/Users/Charlie/Desktop")
write.csv(CredSum,file="CredSum.csv",row.names=FALSE)

###Calculating Effects by Site

Ashland<-subset(ML3,ML3$Site=="AshlandUniversity")
length(Ashland$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Ashland)
Anova(Credential.inter.lm,type="II")


Bradley<-subset(ML3,ML3$Site=="BradleyUniversity")
length(Bradley$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Bradley)
Anova(Credential.inter.lm,type="II")



Carleton<-subset(ML3,ML3$Site=="CarletonUniversity")
length(Carleton$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Carleton)
Anova(Credential.inter.lm,type="II")



Ithaca<-subset(ML3,ML3$Site=="IthacaCollege")
length(Ithaca$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Ithaca)
Anova(Credential.inter.lm,type="II")


Miami<-subset(ML3,ML3$Site=="MiamiUniversity")
length(Miami$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Miami)
Anova(Credential.inter.lm,type="II")


MichSt<-subset(ML3,ML3$Site=="MichiganStateUniversity")
length(MichSt$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=MichSt)
Anova(Credential.inter.lm,type="II")




Montana<-subset(ML3,ML3$Site=="MontanaStateUniversity")
length(Montana$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Montana)
Anova(Credential.inter.lm,type="II")


Nova<-subset(ML3,ML3$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Nova)
Anova(Credential.inter.lm,type="II")



OSU<-subset(ML3,ML3$Site=="OSUNewark")
length(OSU$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=OSU)
Anova(Credential.inter.lm,type="II")


PLU<-subset(ML3,ML3$Site=="PacificLutheranUniversity")
length(PLU$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=PLU)
Anova(Credential.inter.lm,type="II")



Penn<-subset(ML3,ML3$Site=="PennStateAbington")
length(Penn$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Penn)
Anova(Credential.inter.lm,type="II")


SDSU<-subset(ML3,ML3$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=SDSU)
Anova(Credential.inter.lm,type="II")



Texas<-subset(ML3,ML3$Site=="TexasAandM")
length(Texas$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Texas)
Anova(Credential.inter.lm,type="II")



Davis<-subset(ML3,ML3$Site=="UCDavis")
length(Davis$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Davis)
Anova(Credential.inter.lm,type="II")


Riverside<-subset(ML3,ML3$Site=="UCRiverside")
length(Riverside$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Riverside)
Anova(Credential.inter.lm,type="II")


Florida<-subset(ML3,ML3$Site=="UniversityOfFlorida")
length(Florida$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Florida)
Anova(Credential.inter.lm,type="II")


Mississippi<-subset(ML3,ML3$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Mississippi)
Anova(Credential.inter.lm,type="II")


Toronto<-subset(ML3,ML3$Site=="UniversityOfToronto")
length(Toronto$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Toronto)
Anova(Credential.inter.lm,type="II")


Virginia<-subset(ML3,ML3$Site=="UniversityOfVirginia")
length(Virginia$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=Virginia)
Anova(Credential.inter.lm,type="II")


VCU<-subset(ML3,ML3$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=VCU)
Anova(Credential.inter.lm,type="II")


mTurk<-subset(ML3,ML3$Site=="mTurk")
length(mTurk$session_id)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=mTurk)
Anova(Credential.inter.lm,type="II")


#####Task Order Effects####
head(ML3)
str(ML3$moninvignette_order)
Order.lm<-lm(mcdv1~CredCond*Genderfactor*moninvignette_order,data=ML3)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.2497,df.1=1,df.2= 3126,N=3134,conf.level=.95)
ci.pvaf(F.value= 0.0473,df.1=1,df.2= 3126,N=3134,conf.level=.95)

ML3$moninvignette_ordersquare<-ML3$moninvignette_order^2
OrderQuad.lm<-lm(mcdv1~CredCond*Genderfactor*moninvignette_ordersquare,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.0149,df.1=1,df.2= 3126,N=3134,conf.level=.95)
ci.pvaf(F.value= 0.2099,df.1=1,df.2= 3126,N=3134,conf.level=.95)


Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
ML3<-merge(ML3,Order2,by="session_id",all=TRUE)
head(ML3)
str(ML3$CredentialOrder10)

Order.lm<-lm(mcdv1~CredCond*Genderfactor*CredentialOrder10,data=ML3)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.0152,df.1=1,df.2= 3126,N=3134,conf.level=.95)
ci.pvaf(F.value= 0.3269,df.1=1,df.2= 3126,N=3134,conf.level=.95)

ML3$CredentialOrder10square<-ML3$CredentialOrder10^2
OrderQuad.lm<-lm(mcdv1~CredCond*Genderfactor*CredentialOrder10square,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.0793,df.1=1,df.2= 3126,N=3134,conf.level=.95)
ci.pvaf(F.value= 0.0960,df.1=1,df.2= 3126,N=3134,conf.level=.95)

First<-subset(ML3,CredentialOrder10==1)
Credential.inter.lm<-lm(mcdv1~CredCond*Genderfactor,data=First)
Anova(Credential.inter.lm,type="II")
ci.pvaf(F.value= 0.0905,df.1=1,df.2= 363,N=367,conf.level=.95)
ci.pvaf(F.value= 1.4025,df.1=1,df.2= 363,N=367,conf.level=.95)


