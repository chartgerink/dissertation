#############MANY LABS 3 SELF-ESTEEM AND SUBJECTIVE DISTANCE##################
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
BestGrade$SESDOrder<-BestGrade$bestgrade_order
WorstGrade$SESDOrder<-WorstGrade$worstgrade_order
SESDdata<-rbind(BestGrade,WorstGrade)
head(SESDdata)
tail(SESDdata)
list(SESDdata$SubDistCond)

###Adding Months Since data
setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
Months<-read.csv(file="ML3 SESD Open Response.csv",header=TRUE,stringsAsFactors=FALSE)
head(Months)
tail(Months)
str(Months$Year)
str(Months$TermsBack)
Months$YearMonths<-(2014-Months$Year)*12
Months$TermMonths[Months$TermsBack==0]<-0
Months$TermMonths[Months$TermsBack==1]<-1
Months$TermMonths[Months$TermsBack==2]<-4
Months$TermMonths[Months$TermsBack==3]<-8
Months$MonthsSince<-Months$YearMonths+Months$TermMonths
head(Months)
tail(Months)
hist(Months$MonthsSince)
mean(Months$MonthsSince,na.rm=TRUE)
median(Months$MonthsSince,na.rm=TRUE)

#merging data
SESDdata<-merge(SESDdata,Months,by="session_id",all.x=TRUE)
head(SESDdata)


#Centering continuous predictors (months since class and self-esteem)
SESDdata$MonthsSince<-SESDdata$MonthsSince-mean(SESDdata$MonthsSince,na.rm=TRUE)

SESDdata$SelfEsteem<-SESDdata$SelfEsteem-mean(SESDdata$SelfEsteem,na.rm=TRUE)

SESDdata<-subset(SESDdata,SESDdata$SubDistCond!="NA")
SESDdata<-subset(SESDdata,SESDdata$SelfEsteem!="NA")
SESDdata<-subset(SESDdata,SESDdata$MonthsSince!="NA")
str(SESDdata$SelfEsteem)
str(SESDdata$SubDistCond)
SESDdata$SubDistCond<-as.factor(SESDdata$SubDistCond)
length(SESDdata$session_id)

###Replicating Previous Effect###
#First, we will conduct stepwise regression analyses to examine the variables of interest. Subjective distance is the dependent variable. All continuous predictor variables will be centered. In the first step, we will enter a variable that indicates actual time, which is calculated from the number of months since the participant’s course ended. In the second step, grade condition (best vs. worst grade) and self-esteem are entered simultaneously. In the last step, the interaction between grade condition (best vs. worst grade) and self-esteem will be entered.  Follow-up tests will clarify the nature of the interaction effect.

SESDmodel1.lm<-lm(SubDist~MonthsSince,data=SESDdata)
Anova(SESDmodel1.lm,type="II")
SESDmodel2.lm<-lm(SubDist~MonthsSince+SubDistCond+SelfEsteem,data=SESDdata)
Anova(SESDmodel2.lm,type="II")
SESDmodel3.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=SESDdata)
anova(SESDmodel1.lm,SESDmodel2.lm,SESDmodel3.lm)
Anova(SESDmodel3.lm,type="II")
summary(SESDmodel3.lm)

ci.pvaf(F.value= 1.9795,df.1=1,df.2=3131,N=3136,conf.level=.95)
ci.pvaf(F.value= 33.2447,df.1=1,df.2=3131,N=3136,conf.level=.95)
ci.pvaf(F.value= 7.9705,df.1=1,df.2=3131,N=3136,conf.level=.95)

ci.pvaf(F.value= 1.9795,df.1=1,df.2=3131,N=3136,conf.level=.99)
ci.pvaf(F.value= 33.2447,df.1=1,df.2=3131,N=3136,conf.level=.99)

plot(allEffects(SESDmodel3.lm))

##Follow-up Analyses##
#Follow-up tests will clarify the nature of the interaction effect.
#To be completed once interaction is detected

###Looking at just pool###
Pool<-subset(SESDdata,SESDdata$Site!="mTurk")
length(Pool$session_id)

SESDmodel4.lm<-lm(SubDist~MonthsSince,data=Pool)
Anova(SESDmodel4.lm,type="II")
SESDmodel5.lm<-lm(SubDist~MonthsSince+SubDistCond+SelfEsteem,data=Pool)
Anova(SESDmodel5.lm,type="II")
SESDmodel6.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Pool)
anova(SESDmodel1.lm,SESDmodel2.lm,SESDmodel3.lm)
Anova(SESDmodel6.lm,type="II")
plot(allEffects(SESDmodel6.lm))
summary(SESDmodel6.lm)

###DESCRIPTIVES###
summaryBy(SubDist~SubDistCond,data=SESDdata,FUN=list(mean,max,min,median,sd),na.rm=TRUE)


###############################Supplementary Analyses#####################################

###This Effect First###
BestGrade1st<-subset(SESDdata,SESDdata$bestgrade_order==2)
list(BestGrade1st$bestgrade_order)
WorstGrade1st<-subset(SESDdata,SESDdata$worstgrade_order==2)
list(WorstGrade1st$worstgrade_order)
SESDfirst<-rbind(BestGrade1st,WorstGrade1st)

SESD1model1.lm<-lm(SubDist~MonthsSince,data=SESDfirst)
Anova(SESD1model1.lm,type="II")
SESD1model2.lm<-lm(SubDist~MonthsSince+SubDistCond+SelfEsteem,data=SESDfirst)
Anova(SESD1model2.lm,type="II")
SESD1model3.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=SESDfirst)
anova(SESD1model1.lm,SESD1model2.lm,SESD1model3.lm)
Anova(SESD1model3.lm,type="II")
summary(SESD1model3.lm)

plot(allEffects(SESD1model3.lm))

###Checking just those who passed attention check###
AttentionPass<-subset(SESDdata,SESDdata$AttentionCheck=="Pass")

SESDamodel1.lm<-lm(SubDist~MonthsSince,data=AttentionPass)
Anova(SESDamodel1.lm,type="II")
SESDamodel2.lm<-lm(SubDist~MonthsSince+SubDistCond+SelfEsteem,data=AttentionPass)
Anova(SESDamodel2.lm,type="II")
SESDamodel3.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=AttentionPass)
anova(SESDamodel1.lm,SESDamodel2.lm,SESDamodel3.lm)
Anova(SESDamodel3.lm,type="II")
summary(SESDamodel3.lm)
plot(allEffects(SESDamodel3.lm))


##############################Exploratory Analyses#######################################
str(SESDdata$DaysInComp)
Pool<-subset(SESDdata,SESDdata$Site!="mTurk")
length(unique(Pool$session_id))
str(Pool$SubDistCond)


SESDmodel.Time.lm<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*DaysInComp,data=Pool)
Anova(SESDmodel.Time.lm,type="II")
plot(allEffects(SESDmodel.Time.lm))

BestGradeOnly<-subset(Pool,Pool$SubDistCond=="BestGrade")
BestGrade.Time.lm<-lm(SubDist~MonthsSince+SelfEsteem*DaysInComp,data=BestGradeOnly)
Anova(BestGrade.Time.lm,type="II")
plot(allEffects(BestGrade.Time.lm))

FirstFifth<-subset(Pool,Pool$DaysInComp<.2)
LastFifth<-subset(Pool,Pool$DaysInComp>.8)
list(LastFifth$DaysInComp)
range(FirstFifth$DaysInComp)
range(LastFifth$DaysInComp)


###############################Time of Semester Analyses#################################
Pool<-subset(SESDdata,SESDdata$Site!="mTurk")
First80<-subset(Pool,Pool$DaysInComp<.8)
Last20<-subset(Pool,Pool$DaysInComp>=.8)

range(First80$DaysInComp)
range(Last20$DaysInComp)

SESDmodel.First80.lm<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem,data=First80)
Anova(SESDmodel.First80.lm,type="II")
plot(allEffects(SESDmodel.First80.lm))
ci.pvaf(F.value= 2.9665,df.1=1,df.2=1985,N=1990,conf.level=.95)
ci.pvaf(F.value= 19.8751,df.1=1,df.2=1985,N=1990,conf.level=.95)

SESDmodel.Last20.lm<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem,data=Last20)
Anova(SESDmodel.Last20.lm,type="II")
plot(allEffects(SESDmodel.Last20.lm))
ci.pvaf(F.value= 0.6803,df.1=1,df.2=567,N=572,conf.level=.95)
ci.pvaf(F.value= 10.1199,df.1=1,df.2=567,N=572,conf.level=.95)

#####Mixed Models####
str(Pool$DaysInComp)

#Unconditional Model
SubDist.Uncond<-lmer(SubDist~1+(1|Site),data=Pool)
summary(SubDist.Uncond)
0.06719/(0.06719 + 7.49844)

#Full Model
SubDist.MEmodel<-lmer(SubDist~MonthsSince+SubDistCond*SelfEsteem*DaysInComp+(1|Site),data=Pool)
summary(SubDist.MEmodel)

#Model Comparison
SubDist.MEmodel.null<-lmer(SubDist~MonthsSince+SubDistCond*SelfEsteem+(1|Site),data=Pool,REML=FALSE)
SubDist.MEmodel.test<-lmer(SubDist~MonthsSince+SubDistCond*SelfEsteem*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(SubDist.MEmodel.null,SubDist.MEmodel.test)
coef(SubDist.MEmodel.test)

###Added Main Effect
#Full Model
SubDist.MEmodel<-lmer(SubDist~MonthsSince+SubDistCond*DaysInComp+(1|Site),data=Pool)
summary(SubDist.MEmodel)

#Model Comparison
SubDist.MEmodel.null<-lmer(SubDist~MonthsSince+SubDistCond+(1|Site),data=Pool,REML=FALSE)
SubDist.MEmodel.test<-lmer(SubDist~MonthsSince+SubDistCond*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(SubDist.MEmodel.null,SubDist.MEmodel.test)
coef(SubDist.MEmodel.test)



###############Moderator and Order Analyses#################

###Moderators Main Effect
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(SubDist~MonthsSince+SubDistCond*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(SubDist~MonthsSince+SubDistCond*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(SubDist~MonthsSince+SubDistCond*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(SubDist~MonthsSince+SubDistCond*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(SubDist~MonthsSince+SubDistCond*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(SubDist~MonthsSince+SubDistCond*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 5.4313,df.1=1,df.2=2552,N=2557,conf.level=.95)
plot(allEffects(Mood))
#Effect Driven by people in good mood pulling class espcially close

str(Pool$Stress)
Stress<-lm(SubDist~MonthsSince+SubDistCond*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 30.563,df.1=1,df.2=2544,N=2549,conf.level=.95)
plot(allEffects(Stress))
#Stressed people feel like best grade is further and worst grade is closer

###############Moderator and Order Analyses#################

###Moderators Primary Effect
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 5.4313,df.1=1,df.2=2552,N=2557,conf.level=.95)
plot(allEffects(Mood))
Happy<-subset(Pool,Mood>=5)
Sad<-subset(Pool,Mood<=3)
Happy.lm<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem,data=Happy)
Anova(Happy.lm,type="II")
Sad.lm<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem,data=Sad)
Anova(Sad.lm,type="II")

str(Pool$Stress)
Stress<-lm(SubDist~MonthsSince+SubDistCond*SelfEsteem*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 30.563,df.1=1,df.2=2544,N=2549,conf.level=.95)
plot(allEffects(Stress))


#Summary Data

SESDSum<-summarise(group_by(SESDdata,Site),HighN=sum(SubDistCond=="BestGrade",na.rm=TRUE),LowN=sum(SubDistCond=="WorstGrade",na.rm=TRUE),HighMean=mean(SubDist[SubDistCond=="BestGrade"],na.rm=TRUE),LowMean=mean(SubDist[SubDistCond=="WorstGrade"],na.rm=TRUE),HighSD=sd(SubDist[SubDistCond=="BestGrade"],na.rm=TRUE),LowSD=sd(SubDist[SubDistCond=="WorstGrade"],na.rm=TRUE))
SESDSum
setwd("/Users/Charlie/Desktop")
write.csv(SESDSum,file="SESDSum.csv",row.names=FALSE)

###Effect by Site

Ashland<-subset(SESDdata,SESDdata$Site=="AshlandUniversity")
length(Ashland$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Ashland)
Anova(SESDinter.inter.lm,type="II")


Bradley<-subset(SESDdata,SESDdata$Site=="BradleyUniversity")
length(Bradley$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Bradley)
Anova(SESDinter.inter.lm,type="II")



Carleton<-subset(SESDdata,SESDdata$Site=="CarletonUniversity")
length(Carleton$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Carleton)
Anova(SESDinter.inter.lm,type="II")
plot(allEffects(SESDinter.inter.lm))


Ithaca<-subset(SESDdata,SESDdata$Site=="IthacaCollege")
length(Ithaca$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Ithaca)
Anova(SESDinter.inter.lm,type="II")


Miami<-subset(SESDdata,SESDdata$Site=="MiamiUniversity")
length(Miami$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Miami)
Anova(SESDinter.inter.lm,type="II")


MichSt<-subset(SESDdata,SESDdata$Site=="MichiganStateUniversity")
length(MichSt$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=MichSt)
Anova(SESDinter.inter.lm,type="II")




Montana<-subset(SESDdata,SESDdata$Site=="MontanaStateUniversity")
length(Montana$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Montana)
Anova(SESDinter.inter.lm,type="II")


Nova<-subset(SESDdata,SESDdata$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Nova)
Anova(SESDinter.inter.lm,type="II")



OSU<-subset(SESDdata,SESDdata$Site=="OSUNewark")
length(OSU$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=OSU)
Anova(SESDinter.inter.lm,type="II")


PLU<-subset(SESDdata,SESDdata$Site=="PacificLutheranUniversity")
length(PLU$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=PLU)
Anova(SESDinter.inter.lm,type="II")



Penn<-subset(SESDdata,SESDdata$Site=="PennStateAbington")
length(Penn$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Penn)
Anova(SESDinter.inter.lm,type="II")


SDSU<-subset(SESDdata,SESDdata$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=SDSU)
Anova(SESDinter.inter.lm,type="II")



Texas<-subset(SESDdata,SESDdata$Site=="TexasAandM")
length(Texas$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Texas)
Anova(SESDinter.inter.lm,type="II")



Davis<-subset(SESDdata,SESDdata$Site=="UCDavis")
length(Davis$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Davis)
Anova(SESDinter.inter.lm,type="II")


Riverside<-subset(SESDdata,SESDdata$Site=="UCRiverside")
length(Riverside$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Riverside)
Anova(SESDinter.inter.lm,type="II")
plot(allEffects(SESDinter.inter.lm))

Florida<-subset(SESDdata,SESDdata$Site=="UniversityOfFlorida")
length(Florida$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Florida)
Anova(SESDinter.inter.lm,type="II")


Mississippi<-subset(SESDdata,SESDdata$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Mississippi)
Anova(SESDinter.inter.lm,type="II")


Toronto<-subset(SESDdata,SESDdata$Site=="UniversityOfToronto")
length(Toronto$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Toronto)
Anova(SESDinter.inter.lm,type="II")


Virginia<-subset(SESDdata,SESDdata$Site=="UniversityOfVirginia")
length(Virginia$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=Virginia)
Anova(SESDinter.inter.lm,type="II")


VCU<-subset(SESDdata,SESDdata$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=VCU)
Anova(SESDinter.inter.lm,type="II")
plot(allEffects(SESDinter.inter.lm))

mTurk<-subset(SESDdata,SESDdata$Site=="mTurk")
length(mTurk$session_id)
SESDinter.inter.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=mTurk)
Anova(SESDinter.inter.lm,type="II")

#####Task Order Effects####
head(SESDdata)
str(SESDdata$SESDOrder)
Order.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem*SESDOrder,data=SESDdata)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.1946,df.1=1,df.2= 3127,N=3136,conf.level=.95)
ci.pvaf(F.value= 1.1241,df.1=1,df.2= 3127,N=3136,conf.level=.95)

SESDdata$SESDOrdersquare<-SESDdata$SESDOrder^2
OrderQuad.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem*SESDOrdersquare,data=SESDdata)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.6657,df.1=1,df.2= 3127,N=3136,conf.level=.95)
ci.pvaf(F.value= 1.3843,df.1=1,df.2= 3127,N=3136,conf.level=.95)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
SESDdata<-merge(SESDdata,Order2,by="session_id",all=TRUE)
head(SESDdata)

str(SESDdata$SESDOrder10)
Order.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem*SESDOrder10,data=SESDdata)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.4209,df.1=1,df.2= 3127,N=3136,conf.level=.95)
ci.pvaf(F.value= 0.0913,df.1=1,df.2= 3127,N=3136,conf.level=.95)

SESDdata$SESDOrder10square<-SESDdata$SESDOrder10^2
OrderQuad.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem*SESDOrder10square,data=SESDdata)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.6038,df.1=1,df.2= 3127,N=3136,conf.level=.95)
ci.pvaf(F.value= 0.4251,df.1=1,df.2= 3127,N=3136,conf.level=.95)


SESDmodel3.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=SESDdata)
anova(SESDmodel1.lm,SESDmodel2.lm,SESDmodel3.lm)
Anova(SESDmodel3.lm,type="II")

First<-subset(SESDdata,SESDOrder10==1)
SESDmodel3.lm<-lm(SubDist~MonthsSince+SubDistCond+ SelfEsteem +SubDistCond* SelfEsteem,data=First)
Anova(SESDmodel3.lm,type="II")
ci.pvaf(F.value= 0.3690,df.1=1,df.2= 362,N=367,conf.level=.95)
ci.pvaf(F.value= 8.8986,df.1=1,df.2= 362,N=367,conf.level=.95)




