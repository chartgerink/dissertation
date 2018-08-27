#############MANY LABS 3 CONSCIENTIOUSNESS AND PERSISTENCE#################
#######Charlie Ebersole, Begin January 21, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)

str(ML3$Conscientiousness)
str(ML3$Persistence)

#Checking number of participants who answered both measures
Consc<-subset(ML3,ML3$Conscientiousness!="NA")
length(Consc$session_id)
Persist<-subset(ML3,ML3$Persistence!="NA")
length(Persist$session_id)
Persist2<-subset(Persist,Persist$Conscientiousness!="NA")
length(unique(Persist2$session_id))

#checking mTurk
mTurk<-subset(Persist2,Persist2$Site=="mTurk")
length(mTurk$session_id)
list(mTurk$Conscientiousness)
list(mTurk$Persistence)


###Replicating Previous Effect###
#Persistence will be measured as the amount of time spent on the anagrams page before pressing the continue button.  The relationship between persistence and conscientiousness will be assessed as a pearson’s correlation.

cor(ML3$Conscientiousness,ML3$Persistence,use="complete.obs",method="pearson")
plot(ML3$Conscientiousness,ML3$Persistence)
cor.test(ML3$Conscientiousness,ML3$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(0.02655341,var.r=NULL,3193)
res(0.02655341,var.r=NULL,3193,level=99)

###Calculating effect size from previous work
res(.46,var.r=NULL,130)

###DESCRIPTIVES###
summary(ML3$Conscientiousness)
sd(ML3$Conscientiousness,na.rm=TRUE)
hist(ML3$Conscientiousness)

summary(ML3$Persistence)
sd(ML3$Persistence,na.rm=TRUE)
hist(ML3$Persistence)

##############################Exploratory Analyses#######################################
str(ML3$DaysInComp)
Pool<-subset(ML3,ML3$Site!="mTurk")
length(unique(Pool$session_id))
str(Pool$Conscientiousness)
str(Pool$Persistence)

plot(Pool$Concientiousness,Pool$DaysInComp,complete.obs=TRUE)
cor(Pool$Conscientiousness,Pool$Persistence,use="complete.obs",method="pearson")

cor(Pool$Conscientiousness,Pool$DaysInComp,use="complete.obs",method="pearson")
cor(Pool$Persistence,Pool$DaysInComp,use="complete.obs",method="pearson")

GaveUp<-subset(Pool,Pool$Persistence<240)
range(GaveUp$Persistence)
cor(GaveUp$Persistence,GaveUp$DaysInComp,use="complete.obs",method="pearson")

Persistence.lm<-lm(Persistence~Conscientiousness*DaysInComp,data=Pool)
Anova(Persistence.lm,type="II")
plot(allEffects(Persistence.lm))

Conscientiousness.lm<-lm(Conscientiousness~Persistence*DaysInComp,data=Pool)
Anova(Conscientiousness.lm,type="II")
plot(allEffects(Conscientiousness.lm))


##############################Supplementary Analyses#######################################
AttentionPass<-subset(ML3,ML3$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication

cor(AttentionPass $Conscientiousness, AttentionPass $Persistence,use="complete.obs",method="pearson")

###This Effect First###
Anagrams1st<-subset(ML3,ML3$anagrams_order==2)
list(Anagrams1st$anagrams_order)

cor(Anagrams1st$Conscientiousness,Anagrams1st$Persistence,use="complete.obs",method="pearson")



###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(ML3,ML3$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)

#First 80
cor(First80$Conscientiousness,First80$Persistence,use="complete.obs",method="pearson")
cor.test(First80$Conscientiousness,First80$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(0.0124703,var.r=NULL,2033)

#Last 20
cor(Last20$Conscientiousness,Last20$Persistence,use="complete.obs",method="pearson")
cor.test(Last20$Conscientiousness,Last20$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(0.03195829,var.r=NULL,591)

str(Pool$Persistence)

#####Mixed Models####
str(Pool$DaysInComp)
str(Pool$Conscientiousness)

#Unconditional Model
Persistence.Uncond<-lmer(Persistence~1+(1|Site),data=Pool)
summary(Persistence.Uncond)
225.1/(225.1 + 4290.5)

#Full Model
Persistence.MEmodel<-lmer(Persistence~Conscientiousness*DaysInComp+(1|Site),data=Pool)
summary(Persistence.MEmodel)

#Model Comparison
Persistence.MEmodel.null<-lmer(Persistence~Conscientiousness+(1|Site),data=Pool,REML=FALSE)
Persistence.MEmodel.test<-lmer(Persistence~Conscientiousness*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Persistence.MEmodel.null,Persistence.MEmodel.test)
coef(Persistence.MEmodel.test)

###############Moderator and Order Analyses#################

###Moderators
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(Persistence~Conscientiousness*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(Persistence~Conscientiousness*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(Persistence~Conscientiousness*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 11.7115,df.1=1,df.2=2347,N=2613,conf.level=.95)
plot(allEffects(ReportedEffort))

One<-subset(Pool,ReportedEffort==1)
cor.test(One$Conscientiousness,One$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
Two<-subset(Pool,ReportedEffort==2)
cor.test(Two$Conscientiousness,Two$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
Three<-subset(Pool,ReportedEffort==3)
cor.test(Three$Conscientiousness,Three$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
Four<-subset(Pool,ReportedEffort==4)
cor.test(Four$Conscientiousness,Four$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
Five<-subset(Pool,ReportedEffort==5)
cor.test(Five$Conscientiousness,Five$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)

OneandTwo<-rbind(One,Two)
cor.test(OneandTwo$Conscientiousness, OneandTwo$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(Persistence~Conscientiousness*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(Persistence~Conscientiousness*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(Persistence~Conscientiousness*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 4.2881,df.1=1,df.2=2497,N=2501,conf.level=.95)
plot(allEffects(Mood))



#Summary Data
str(ML3$Conscientiousness)
str(ML3$Persistence)
CPSum<-summarise(group_by(ML3,Site),CN=sum(Conscientiousness !="NA",na.rm=TRUE),PN=sum(Persistence!="NA",na.rm=TRUE),CM=mean(Conscientiousness,na.rm=TRUE),PM=mean(Persistence,na.rm=TRUE),CSD=sd(Conscientiousness,na.rm=TRUE),PSD=sd(Persistence,na.rm=TRUE))
CPSum
setwd("/Users/Charlie/Desktop")
write.csv(CPSum,file="CPSum.csv",row.names=FALSE)


####Effect by Site####

Ashland<-subset(ML3,ML3$Site=="AshlandUniversity")
length(Ashland$session_id)
cor.test(Ashland$Conscientiousness,Ashland$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Ashland$Conscientiousness,Ashland$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Ashland$Conscientiousness,Ashland$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Bradley<-subset(ML3,ML3$Site=="BradleyUniversity")
length(Bradley$session_id)
cor.test(Bradley$Conscientiousness,Bradley$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Bradley$Conscientiousness,Bradley$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Bradley$Conscientiousness,Bradley$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Carleton<-subset(ML3,ML3$Site=="CarletonUniversity")
length(Carleton$session_id)
cor.test(Carleton$Conscientiousness,Carleton$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Carleton$Conscientiousness,Carleton$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Carleton$Conscientiousness,Carleton$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Ithaca<-subset(ML3,ML3$Site=="IthacaCollege")
length(Ithaca$session_id)
cor.test(Ithaca$Conscientiousness,Ithaca$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Ithaca$Conscientiousness,Ithaca$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Ithaca$Conscientiousness,Ithaca$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

Miami<-subset(ML3,ML3$Site=="MiamiUniversity")
length(Miami$session_id)
cor.test(Miami$Conscientiousness,Miami$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Miami$Conscientiousness,Miami$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Miami$Conscientiousness,Miami$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


MichSt<-subset(ML3,ML3$Site=="MichiganStateUniversity")
length(MichSt$session_id)
cor.test(MichSt$Conscientiousness,MichSt$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(MichSt$Conscientiousness,MichSt$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(MichSt$Conscientiousness,MichSt$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)



Montana<-subset(ML3,ML3$Site=="MontanaStateUniversity")
length(Montana$session_id)
cor.test(Montana$Conscientiousness,Montana$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Montana$Conscientiousness,Montana$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Montana$Conscientiousness,Montana$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

Nova<-subset(ML3,ML3$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
cor.test(Nova$Conscientiousness,Nova$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Nova$Conscientiousness,Nova$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Nova$Conscientiousness,Nova$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


OSU<-subset(ML3,ML3$Site=="OSUNewark")
length(OSU$session_id)
cor.test(OSU$Conscientiousness,OSU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(OSU$Conscientiousness,OSU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(OSU$Conscientiousness,OSU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

PLU<-subset(ML3,ML3$Site=="PacificLutheranUniversity")
length(PLU$session_id)
cor.test(PLU$Conscientiousness,PLU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(PLU$Conscientiousness,PLU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(PLU$Conscientiousness,PLU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Penn<-subset(ML3,ML3$Site=="PennStateAbington")
length(Penn$session_id)
cor.test(Penn$Conscientiousness,Penn$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Penn$Conscientiousness,Penn$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Penn$Conscientiousness,Penn$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

SDSU<-subset(ML3,ML3$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
cor.test(SDSU$Conscientiousness,SDSU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(SDSU$Conscientiousness,SDSU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(SDSU$Conscientiousness,SDSU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Texas<-subset(ML3,ML3$Site=="TexasAandM")
length(Texas$session_id)
cor.test(Texas$Conscientiousness,Texas$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Texas$Conscientiousness,Texas$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Texas$Conscientiousness,Texas$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

Davis<-subset(ML3,ML3$Site=="UCDavis")
length(Davis$session_id)
cor.test(Davis$Conscientiousness,Davis$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Davis$Conscientiousness,Davis$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Davis$Conscientiousness,Davis$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

Riverside<-subset(ML3,ML3$Site=="UCRiverside")
length(Riverside$session_id)
cor.test(Riverside$Conscientiousness,Riverside$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Riverside$Conscientiousness,Riverside$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Riverside$Conscientiousness,Riverside$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Florida<-subset(ML3,ML3$Site=="UniversityOfFlorida")
length(Florida$session_id)
cor.test(Florida$Conscientiousness,Florida$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Florida$Conscientiousness,Florida$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Florida$Conscientiousness,Florida$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Mississippi<-subset(ML3,ML3$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
cor.test(Mississippi$Conscientiousness,Mississippi$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Mississippi$Conscientiousness,Mississippi$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Mississippi$Conscientiousness,Mississippi$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

Toronto<-subset(ML3,ML3$Site=="UniversityOfToronto")
length(Toronto$session_id)
cor.test(Toronto$Conscientiousness,Toronto$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Toronto$Conscientiousness,Toronto$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Toronto$Conscientiousness,Toronto$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)


Virginia<-subset(ML3,ML3$Site=="UniversityOfVirginia")
length(Virginia$session_id)
cor.test(Virginia$Conscientiousness,Virginia$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(Virginia$Conscientiousness,Virginia$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(Virginia$Conscientiousness,Virginia$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

VCU<-subset(ML3,ML3$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
cor.test(VCU$Conscientiousness,VCU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(VCU$Conscientiousness,VCU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(VCU$Conscientiousness,VCU$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

mTurk<-subset(ML3,ML3$Site=="mTurk")
length(mTurk$session_id)
cor.test(mTurk$Conscientiousness,mTurk$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(cor.test(mTurk$Conscientiousness,mTurk$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$estimate,var.r=NULL,cor.test(mTurk$Conscientiousness,mTurk$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)$parameter+2)

#####Task Order Effects####
head(ML3)
str(ML3$anagrams_order)
Order.lm<-lm(Persistence~Conscientiousness*anagrams_order,data=ML3)
Anova(Order.lm,type="II")
plot(allEffects(Order.lm))
ci.pvaf(F.value= 0.1025,df.1=1,df.2=3189,N=3193,conf.level=.95)

ML3$anagrams_ordersquare<-ML3$anagrams_order^2
OrderQuad.lm<-lm(Persistence~Conscientiousness*anagrams_ordersquare,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.1025,df.1=1,df.2=3189,N=3193,conf.level=.95)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
ML3<-merge(ML3,Order2,by="session_id",all=TRUE)
head(ML3)

str(ML3$AnagramsOrder10)
Order.lm<-lm(Persistence~Conscientiousness*AnagramsOrder10,data=ML3)
Anova(Order.lm,type="II")
plot(allEffects(Order.lm))
ci.pvaf(F.value= 0.0223,df.1=1,df.2=3189,N=3193,conf.level=.95)

ML3$AnagramsOrder10square<-ML3$AnagramsOrder10^2
OrderQuad.lm<-lm(Persistence~Conscientiousness*AnagramsOrder10square,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.0561,df.1=1,df.2=3189,N=3193,conf.level=.95)

#This effect first
First<-subset(ML3,AnagramsOrder10==1)
cor.test(First$Conscientiousness,First$Persistence,alternative="two.sided",method="pearson",conf.level=0.95)
res(0.006852696,var.r=NULL,367)
