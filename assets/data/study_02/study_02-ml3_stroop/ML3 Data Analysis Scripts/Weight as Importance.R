#############MANY LABS 3 WEIGHT AS EMBODIED IMPORTANCE##################
#######Charlie Ebersole, Begin January 21, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(lmer)
require(compute.es)
require(MBESS)
require(dplyr)

###Removing those who sat themselves or the clipboard down###

WeightClean<-subset(ML3,ML3$CBReject==0)

str(WeightClean$IIResponse)
WeightClean$IIResponse<-as.integer(WeightClean$IIResponse)
str(WeightClean$ClipboardWeight)
WeightClean$ClipboardWeight<-as.factor(WeightClean$ClipboardWeight)
str(WeightClean$ClipBoardMaterial)
WeightClean$ClipBoardMaterial<-as.factor(WeightClean$ClipBoardMaterial)
WeightCleanAnswer<-subset(WeightClean,WeightClean$IIResponse!="NA")
summaryBy(IIResponse~ClipboardWeight,data=WeightCleanAnswer,FUN=list(length))

###Replicating Previous Effect###
#Following the original study, all participants answering the dependent variable will be included in the analysis. The differences between the two groups will be measured using an independent samples t-test.

t.test(IIResponse~ClipboardWeight,data=WeightClean,var.equal=TRUE)
ci.smd(ncp=-0.6091,n.1=1152,n.2=1133,conf.level=.95)
tes(0.6091,1152,1133)
tes(0.6091,1152,1133,level=99)

##Follow-up tests
#The material of the clipboard (metal or plastic) will be added to the model to determine if the clipboard’s composition influenced the strength of the effect.

II.material.inter.lm<-lm(IIResponse~ClipboardWeight*ClipBoardMaterial,data=WeightClean)
Anova(II.material.inter.lm,type="II")
plot(allEffects(II.material.inter.lm))

###Looking just at metal

Metal<-subset(WeightClean,WeightClean$ClipBoardMaterial=="Metal")
t.test(IIResponse~ClipboardWeight,data=Metal,var.equal=TRUE)
ci.smd(ncp=-0.6789,n.1=177,n.2=171,conf.level=.95)
tes(0.6789,177,171)

MetalYes<-subset(Metal,Metal$IIResponse!="NA")
summaryBy(IIResponse~ClipboardWeight,data=MetalYes,FUN=list(length))

#By metal site
#UVA
UVA<-subset(Metal,Metal$Site=="UniversityOfVirginia")
t.test(IIResponse~ClipboardWeight,data=UVA,var.equal=TRUE)
ci.smd(ncp=-0.5762,n.1=84,n.2=80,conf.level=.95)
UVAYes<-subset(UVA,UVA$IIResponse!="NA")
summaryBy(IIResponse~ClipboardWeight,data=UVAYes,FUN=list(length))

#Davis
Davis<-subset(Metal,Metal$Site=="UCDavis")
t.test(IIResponse~ClipboardWeight,data=Davis,var.equal=TRUE)
ci.smd(ncp=-0.0857,n.1=52,n.2=51,conf.level=.95)
DavisYes<-subset(Davis,Davis$IIResponse!="NA")
summaryBy(IIResponse~ClipboardWeight,data=DavisYes,FUN=list(length))

#Miami
Miami<-subset(Metal,Metal$Site=="MiamiUniversity")
t.test(IIResponse~ClipboardWeight,data=Miami,var.equal=TRUE)
ci.smd(ncp=-0.527,n.1=41,n.2=40,conf.level=.95)
MiamiYes<-subset(Miami,Miami$IIResponse!="NA")
summaryBy(IIResponse~ClipboardWeight,data=MiamiYes,FUN=list(length))

####Looking at Plastic###
Plastic<-subset(WeightClean,WeightClean$ClipBoardMaterial=="Plastic")
t.test(IIResponse~ClipboardWeight,data=Plastic,var.equal=TRUE)
ci.smd(ncp=-0.3601,n.1=975,n.2=962,conf.level=.95)
tes(0.3601,975,962)

PlasticYes<-subset(Plastic,Plastic$IIResponse!="NA")
summaryBy(IIResponse~ClipboardWeight,data=PlasticYes,FUN=list(length))

###DESCRIPTIVES###
summaryBy(IIResponse~ClipboardWeight,data=WeightClean,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
summaryBy(IIResponse~ClipboardWeight,data=WeightClean,FUN=list(length))
ggplot(WeightClean,aes(x=IIResponse,fill=ClipboardWeight))+geom_histogram(binwidth=1,alpha=.5,position="identity")+xlab("Rated Importance")+ggtitle("Weight as Importance Response Distribution,10=lighter clipboard, 20=heavier clipboard")

##############################Supplementary Analyses#######################################
AttentionPass<-subset(WeightClean,WeightClean$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication

t.test(IIResponse~ClipboardWeight,data=AttentionPass,var.equal=TRUE)

#Checking in lab effects first
Lab1st<-subset(WeightClean,WeightClean$inlab_order==2)
list(Lab1st$inlab_order)

t.test(IIResponse~ClipboardWeight,data=Lab1st,var.equal=TRUE)


##############################Exploratory Analyses#######################################
str(ML3$DaysInComp)

Weight.Time.lm<-lm(IIResponse~ClipboardWeight*DaysInComp,data=WeightClean)
Anova(Weight.Time.lm,type="II")
plot(allEffects(Weight.Time.lm))

###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(WeightClean, WeightClean$Site!="mTurk")
First80<-subset(WeightClean, WeightClean$DaysInLab<.8)
Last20<-subset(WeightClean, WeightClean$DaysInLab>=.8)
range(First80$DaysInLab)
range(Last20$DaysInLab)

#First 80
t.test(IIResponse~ClipboardWeight,data=First80,var.equal=TRUE)
tes(0.791,877,871)
DV80<-subset(First80, First80$IIResponse!="NA")
length(unique(DV80$session_id))
summaryBy(IIResponse~ClipboardWeight,data=DV80,FUN=list(length))

#Last 20
t.test(IIResponse~ClipboardWeight,data=Last20,var.equal=TRUE)
tes(0.069,236,220)
DV20<-subset(Last20, Last20$IIResponse!="NA")
length(unique(DV20$session_id))
summaryBy(IIResponse~ClipboardWeight,data=DV20,FUN=list(length))
0.005924/(0.005924+ 1.044108)


###Mixed Effects Model
str(Pool$ClipboardWeight)
IIResponse.Uncond<-lmer(IIResponse~1+(1|Site),data=Pool)
summary(IIResponse.Uncond)
0.005924/(0.005924+ 1.044108)

#Weight Embodiment Full Model Output
IIResponse.MEmodel<-lmer(IIResponse~ClipboardWeight*DaysInLab+(1|Site),data=Pool)
summary(IIResponse.MEmodel)
coef(IIResponse.MEmodel)
qqPlot(resid(IIResponse.MEmodel))

#Model Comparison
IIResponse.Null<-lmer(IIResponse~ClipboardWeight+(1|Site),data=Pool,REML=FALSE)
IIResponse.MEmodel2<-lmer(IIResponse~ClipboardWeight*DaysInLab+(1|Site),data=Pool,REML=FALSE)
anova(IIResponse.Null,IIResponse.MEmodel2)
coef(IIResponse.MEmodel2)

###Inverse Reflection Transformation
Pool$IIReflect<-1/(8-Pool$IIResponse)

#Weight Embodiment Full Model with Transformation
IIReflect.Uncond<-lmer(IIReflect~1+(1|Site),data=Pool)
summary(IIReflect.Uncond)
0.001385/(0.001385+ 0.081670)

IIReflect.MEmodel<-lmer(IIReflect~ClipboardWeight*DaysInLab+(1|Site),data=Pool)
summary(IIReflect.MEmodel)
coef(IIReflect.MEmodel)
qqPlot(resid(IIReflect.MEmodel))


#Model Comparison
IIReflect.Null<-lmer(IIReflect~ClipboardWeight+(1|Site),data=Pool,REML=FALSE)
IIReflect.MEmodel2<-lmer(IIReflect~ClipboardWeight*DaysInLab+(1|Site),data=Pool,REML=FALSE)
anova(IIReflect.Null,IIReflect.MEmodel2)
coef(IIReflect.MEmodel2)




###############Moderator and Order Analyses#################

###Moderators
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(IIResponse~ClipboardWeight*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 0.3393,df.1=1,df.2=2049,N=2053,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(IIResponse~ClipboardWeight*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 1.3072,df.1=1,df.2=2058,N=2062,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(IIResponse~ClipboardWeight*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.0318,df.1=1,df.2=2058,N=2062,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(IIResponse~ClipboardWeight*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0002,df.1=1,df.2=2042,N=2046,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-lm(IIResponse~ClipboardWeight*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.3158,df.1=1,df.2=2059,N=2063,conf.level=.95)

str(Pool$Mood)
Mood<-lm(IIResponse~ClipboardWeight*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.0168,df.1=1,df.2=2063,N=2067,conf.level=.95)

str(Pool$Stress)
Stress<-lm(IIResponse~ClipboardWeight*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 1.8547,df.1=1,df.2=2055,N=2059,conf.level=.95)







############Older Models############

Centering Things
Pool$IIResponseCen<-Pool$IIResponse-mean(Pool$IIResponse,na.rm=TRUE)
Pool$DaysInLabCen<-Pool$DaysInLab-mean(Pool$DaysInLab,na.rm=TRUE)
Light<-subset(Pool,ClipboardWeight=="10")
Heavy<-subset(Pool,ClipboardWeight=="20")
Light$WeightDummy<- -1
Heavy$WeightDummy<- 1
PoolTwo<-rbind(Light,Heavy)
str(Pool2$WeightDummy)
IIResponseCen.MEmodel<-lmer(IIResponseCen~WeightDummy*DaysInLabCen+(1|Site),data=PoolTwo)
summary(IIResponseCen.MEmodel)
qqPlot(resid(IIResponseCen.MEmodel))

#Model Comparison with Centered
IIResponseCen.Null<-lmer(IIResponseCen~WeightDummy+(1|Site),data=PoolTwo,REML=FALSE)
IIResponseCen.MEmodel2<-lmer(IIResponseCen~WeightDummy*DaysInLabCen+(1|Site),data=PoolTwo,REML=FALSE)
anova(IIResponseCen.Null,IIResponseCen.MEmodel2)

###Inverse Reflection Transformation
PoolTwo$IIReflect<-1/(8-PoolTwo$IIResponse)
hist(PoolTwo$IIReflect)

IIResponseReflect.MEmodel<-lmer(IIReflect~WeightDummy*DaysInLabCen+(1|Site),data=PoolTwo)
summary(IIResponseReflect.MEmodel)
qqPlot(resid(IIResponseReflect.MEmodel))
IIResponseReflect.Null<-lmer(IIReflect~WeightDummy+(1|Site),data=PoolTwo,REML=FALSE)
IIResponseReflect.MEmodel2<-lmer(IIReflect~WeightDummy*DaysInLabCen+(1|Site),data=PoolTwo,REML=FALSE)
anova(IIResponseReflect.Null,IIResponseReflect.MEmodel2)

#Hierarchical Follow Up
IIData<-subset(Pool,ClipboardWeight!="NA")
IIData<-subset(IIData,DaysInLab!="NA")
IIsite<-lm(IIResponse~Site,data=IIData)
Anova(IIsite,type="II")
33.19/(33.19+ 2364.94)
IICondition<-lm(IIResponse~Site*ClipboardWeight,data=IIData)
anova(IIsite,IICondition)
IIFinal<-lm(IIResponse~Site*DaysInLab*ClipboardWeight,data=Pool)
Anova(IIFinal,type="II")
anova(IIsite,IICondition,IIFinal)

ci.pvaf(F.value= 1.6849,df.1=19,df.2=2259,N=2279,conf.level=.95)

IICenFollow<-lm(IIResponseCen~Site*WeightDummy*DaysInLabCen,data=PoolTwo)
Anova(IICenFollow,type="II")

str(Light$DaysInLabCen)
str(Light$IIResponseCen)
cor(Light$DaysInLabCen,Light$IIResponseCen,use="complete.obs",method="pearson")
cor.test(Light$DaysInLabCen,Light$IIResponseCen,use="complete.obs",method="pearson")
cor(Heavy$DaysInLabCen,Heavy$IIResponseCen,use="complete.obs",method="pearson")
cor.test(Heavy$DaysInLabCen,Heavy$IIResponseCen,use="complete.obs",method="pearson")


ggplot(Pool, aes(x = DaysInLab, y = IIResponse, colour = ClipboardWeight)) +
  geom_smooth(method = 'lm', formula = y ~ x,fill=NA) +
  xlab('Time into Semester') + ylab('IIResponse')

ggplot(WeightClean, aes(x = DaysInComp, y = IIResponse, colour = Site)) +
  geom_smooth(method = 'lm', formula = y ~ x,fill=NA) +
  xlab('Time into Semester') + ylab('IIResponse')

plot(Pool$Site,Pool$DaysInLab)
cor(Pool$ClipboardWeight,Pool$DaysInLab)


ggplot(Pool,aes(x=IIResponse,fill=ClipboardWeight))+geom_histogram(binwidth=1,alpha=.5,position="identity")+xlab("Rated Importance")+ggtitle("Weight as Importance Response Distribution,10=lighter clipboard, 20=heavier clipboard")


#Summary Data
str(WeightClean$ClipboardWeight)
WeightSum<-summarise(group_by(WeightClean,Site),CredentialN=sum(ClipboardWeight=="10",na.rm=TRUE),NoCredentialN=sum(ClipboardWeight=="20",na.rm=TRUE),CredentialMean=mean(IIResponse[ClipboardWeight=="10"],na.rm=TRUE),NoCredentialMean=mean(IIResponse[ClipboardWeight=="20"],na.rm=TRUE),CredentialSD=sd(IIResponse[ClipboardWeight=="10"],na.rm=TRUE),NoCredentialSD=sd(IIResponse[ClipboardWeight=="20"],na.rm=TRUE))
WeightSum
setwd("/Users/Charlie/Desktop")
write.csv(WeightSum,file="WeightSum.csv",row.names=FALSE)



###Effect by Site

Ashland<-subset(WeightClean,WeightClean$Site=="AshlandUniversity")
length(Ashland$session_id)
t.test(IIResponse~ClipboardWeight,data=Ashland,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Ashland,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Ashland,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Ashland,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Ashland,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Ashland,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Ashland,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Ashland,var.equal=TRUE)$parameter+2)/2)

Bradley<-subset(WeightClean,WeightClean$Site=="BradleyUniversity")
length(Bradley$session_id)
t.test(IIResponse~ClipboardWeight,data=Bradley,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Bradley,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Bradley,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Bradley,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Bradley,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Bradley,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Bradley,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Bradley,var.equal=TRUE)$parameter+2)/2)


Carleton<-subset(WeightClean,WeightClean$Site=="CarletonUniversity")
length(Carleton$session_id)
t.test(IIResponse~ClipboardWeight,data=Carleton,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Carleton,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Carleton,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Carleton,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Carleton,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Carleton,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Carleton,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Carleton,var.equal=TRUE)$parameter+2)/2)


Ithaca<-subset(WeightClean,WeightClean$Site=="IthacaCollege")
length(Ithaca$session_id)
t.test(IIResponse~ClipboardWeight,data=Ithaca,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Ithaca,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Ithaca,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Ithaca,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Ithaca,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Ithaca,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Ithaca,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Ithaca,var.equal=TRUE)$parameter+2)/2)

Miami<-subset(WeightClean,WeightClean$Site=="MiamiUniversity")
length(Miami$session_id)
t.test(IIResponse~ClipboardWeight,data=Miami,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Miami,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Miami,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Miami,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Miami,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Miami,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Miami,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Miami,var.equal=TRUE)$parameter+2)/2)

MichSt<-subset(WeightClean,WeightClean$Site=="MichiganStateUniversity")
length(MichSt$session_id)
t.test(IIResponse~ClipboardWeight,data=MichSt,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= MichSt,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= MichSt,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= MichSt,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=MichSt,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= MichSt,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= MichSt,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= MichSt,var.equal=TRUE)$parameter+2)/2)




Montana<-subset(WeightClean,WeightClean$Site=="MontanaStateUniversity")
length(Montana$session_id)
t.test(IIResponse~ClipboardWeight,data=Montana,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Montana,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Montana,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Montana,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Montana,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Montana,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Montana,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Montana,var.equal=TRUE)$parameter+2)/2)

Nova<-subset(WeightClean,WeightClean$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
t.test(IIResponse~ClipboardWeight,data=Nova,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Nova,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Nova,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Nova,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Nova,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Nova,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Nova,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Nova,var.equal=TRUE)$parameter+2)/2)


OSU<-subset(WeightClean,WeightClean$Site=="OSUNewark")
length(OSU$session_id)
t.test(IIResponse~ClipboardWeight,data=OSU,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= OSU,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= OSU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= OSU,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=OSU,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= OSU,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= OSU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= OSU,var.equal=TRUE)$parameter+2)/2)

PLU<-subset(WeightClean,WeightClean$Site=="PacificLutheranUniversity")
length(PLU$session_id)
t.test(IIResponse~ClipboardWeight,data=PLU,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= PLU,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= PLU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= PLU,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=PLU,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= PLU,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= PLU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= PLU,var.equal=TRUE)$parameter+2)/2)


Penn<-subset(WeightClean,WeightClean$Site=="PennStateAbington")
length(Penn$session_id)
t.test(IIResponse~ClipboardWeight,data=Penn,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Penn,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Penn,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Penn,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Penn,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Penn,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Penn,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Penn,var.equal=TRUE)$parameter+2)/2)

SDSU<-subset(WeightClean,WeightClean$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
t.test(IIResponse~ClipboardWeight,data=SDSU,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= SDSU,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= SDSU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= SDSU,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=SDSU,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= SDSU,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= SDSU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= SDSU,var.equal=TRUE)$parameter+2)/2)


Texas<-subset(WeightClean,WeightClean$Site=="TexasAandM")
length(Texas$session_id)
t.test(IIResponse~ClipboardWeight,data=Texas,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Texas,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Texas,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Texas,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Texas,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Texas,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Texas,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Texas,var.equal=TRUE)$parameter+2)/2)


Davis<-subset(WeightClean,WeightClean$Site=="UCDavis")
length(Davis$session_id)
t.test(IIResponse~ClipboardWeight,data=Davis,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Davis,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Davis,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Davis,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Davis,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Davis,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Davis,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Davis,var.equal=TRUE)$parameter+2)/2)

Riverside<-subset(WeightClean,WeightClean$Site=="UCRiverside")
length(Riverside$session_id)
t.test(IIResponse~ClipboardWeight,data=Riverside,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Riverside,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Riverside,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Riverside,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Riverside,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Riverside,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Riverside,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Riverside,var.equal=TRUE)$parameter+2)/2)


Florida<-subset(WeightClean,WeightClean$Site=="UniversityOfFlorida")
length(Florida$session_id)
t.test(IIResponse~ClipboardWeight,data=Florida,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Florida,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Florida,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Florida,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Florida,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Florida,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Florida,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Florida,var.equal=TRUE)$parameter+2)/2)


Mississippi<-subset(WeightClean,WeightClean$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
t.test(IIResponse~ClipboardWeight,data=Mississippi,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Mississippi,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Mississippi,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Mississippi,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Mississippi,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Mississippi,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Mississippi,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Mississippi,var.equal=TRUE)$parameter+2)/2)

Toronto<-subset(WeightClean,WeightClean$Site=="UniversityOfToronto")
length(Toronto$session_id)
t.test(IIResponse~ClipboardWeight,data=Toronto,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Toronto,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Toronto,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Toronto,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=Toronto,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Toronto,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Toronto,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Toronto,var.equal=TRUE)$parameter+2)/2)


Virginia<-subset(WeightClean,WeightClean$Site=="UniversityOfVirginia")
length(Virginia$session_id)
t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= Virginia,var.equal=TRUE)$parameter+2)/2)

VCU<-subset(WeightClean,WeightClean$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
t.test(IIResponse~ClipboardWeight,data=VCU,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= VCU,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= VCU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= VCU,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=VCU,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= VCU,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= VCU,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= VCU,var.equal=TRUE)$parameter+2)/2)


mTurk<-subset(WeightClean,WeightClean$Site=="mTurk")
length(mTurk$session_id)
t.test(IIResponse~ClipboardWeight,data=mTurk,var.equal=TRUE)
(tes(t.test(IIResponse~ClipboardWeight,data= mTurk,var.equal=TRUE)$statistic,( t.test(IIResponse~ClipboardWeight,data= mTurk,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= mTurk,var.equal=TRUE)$parameter+2)/2))

t.test(IIResponse~ClipboardWeight,data=mTurk,var.equal=FALSE)
tes(t.test(IIResponse~ClipboardWeight,data= mTurk,var.equal=FALSE)$statistic,( t.test(IIResponse~ClipboardWeight,data= mTurk,var.equal=TRUE)$parameter+2)/2, ( t.test(IIResponse~ClipboardWeight,data= mTurk,var.equal=TRUE)$parameter+2)/2)


#####Task Order Effects####
head(WeightClean)
str(WeightClean$inlab_order)
Order.lm<-lm(IIResponse~ClipboardWeight*inlab_order,data=WeightClean)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.1435,df.1=1,df.2= 2066,N=2070,conf.level=.95)

WeightClean$inlab_ordersquare<-WeightClean$inlab_order^2
OrderQuad.lm<-lm(IIResponse~ClipboardWeight*inlab_ordersquare,data=WeightClean)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.1863,df.1=1,df.2= 2066,N=2070,conf.level=.95)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
WeightClean<-merge(WeightClean,Order2,by="session_id",all=TRUE)
head(WeightClean)

str(WeightClean$WeightOrder10)
Order.lm<-lm(IIResponse~ClipboardWeight*WeightOrder10,data=WeightClean)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.0235,df.1=1,df.2= 2066,N=2070,conf.level=.95)

WeightClean$WeightOrder10square<-WeightClean$WeightOrder10^2
OrderQuad.lm<-lm(IIResponse~ClipboardWeight*WeightOrder10square,data=WeightClean)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.0047,df.1=1,df.2= 2066,N=2070,conf.level=.95)


###This task first
First<-subset(WeightClean,WeightOrder10==1)
t.test(IIResponse~ClipboardWeight,data=First,var.equal=TRUE)
summaryBy(IIResponse~ClipboardWeight,data=First,FUN=list(length))
tes(0.151,55,50)
