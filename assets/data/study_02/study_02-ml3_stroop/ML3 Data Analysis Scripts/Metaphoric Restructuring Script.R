#############MANY LABS 3 METAPHORIC RESTRUCTURING##################
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
require(compute.es)
require(lme4)

str(ML3$SRMeetingResponse)
str(ML3$SRCondition)
list(ML3$SRCondition)
ML3$SRCondition<-as.factor(ML3$SRCondition)
list(levels(ML3$SRCondition))

summary(ML3$SRCondition)
900+882+877

###Removing those who did not answer Monday or Friday
Monday<-subset(ML3,ML3$SRMeetingResponse=="Monday")
Friday<-subset(ML3,ML3$SRMeetingResponse=="Friday")
MonFri<-rbind(Monday,Friday)

#Separating Control from Priming Conditions, and labeling responses for priming conditions as PrimeConsistent or PrimeInconsistent
SRControl<-subset(MonFri,MonFri$SRCondition=="C")
SRA<-subset(MonFri,MonFri$SRCondition=="A")
SRB<-subset(MonFri,MonFri$SRCondition=="B")

SRA$PrimeCons[SRA$SRMeetingResponse=="Friday"]<-"Consistent"
SRA$PrimeCons[SRA$SRMeetingResponse=="Monday"]<-"Inconsistent"
SRB$PrimeCons[SRB$SRMeetingResponse=="Monday"]<-"Consistent"
SRB$PrimeCons[SRB$SRMeetingResponse=="Friday"]<-"Inconsistent"

SRPrime<-rbind(SRA,SRB)
str(SRPrime$PrimeCons)
SRPrime$PrimeCons<-as.factor(SRPrime$PrimeCons)

###Removing those who failed the priming manipulation (priming conditions only)
head(SRPrime)
SRPrimeCorrect<-subset(SRPrime,SRPrime$SRTFCorrect=="Yes")
head(SRPrimeCorrect)

###Replicating Previous Effect###
###Testing effect of prime on consistent vs. inconsistent responding
#Participants who do not correctly answer all four priming questions will be removed from the analyses.  A two-way contingency table will be built with Prime condition (ego-moving vs. object-moving) and Response (Monday vs. Friday) as factors. The critical replication hypothesis will be given by a χ2 test between prime consistent and inconsistent responses, collapsing across priming condition.  In addition, a χ2 test will be conducted on the control condition in order to determine if there was a bias toward responding Monday or Friday in the absence of any prime. 

PrimeTable=table(SRPrimeCorrect$PrimeCons)
chisq.test(PrimeTable)
PrimeTable
753/(753+582)
chies(21.9034,1335)
chies(21.9034,1335,level=99)


##getting information for original study
chies(5.2,56)

###Testing bias toward Monday or Friday in control
ControlTable=table(SRControl$SRMeetingResponse)
chisq.test(ControlTable)
ControlTable
542/(542+314)


#looking at the individual priming conditions
A<-subset(SRPrimeCorrect,SRPrimeCorrect$SRCondition=="A")
ATable=table(A$PrimeCons)
ATable
527/(527+250)
B<-subset(SRPrimeCorrect,SRPrimeCorrect$SRCondition=="B")
BTable=table(B$PrimeCons)
BTable
226/(226+332)


##############################Supplementary Analyses#######################################
AttentionPass<-subset(SRPrimeCorrect, SRPrimeCorrect$AttentionCheck=="Pass")
length(AttentionPass$session_id)

AttentionPass2<-subset(SRControl, SRControl$AttentionCheck=="Pass")
length(AttentionPass2$session_id)

#Primary Replication

PrimeTable=table(AttentionPass $PrimeCons)
chisq.test(PrimeTable)
PrimeTable

ControlTable=table(AttentionPass2$SRMeetingResponse)
chisq.test(ControlTable)
ControlTable

###This Task First
Lab1st<-subset(SRPrimeCorrect,SRPrimeCorrect$inlab_order==2)
list(Lab1st$inlab_order)

PrimeTable=table(Lab1st$PrimeCons)
chisq.test(PrimeTable)
PrimeTable

Lab1st<-subset(SRControl,SRControl$inlab_order==2)
list(Lab1st$inlab_order)

###Testing bias toward Monday or Friday in control
ControlTable=table(Lab1st$SRMeetingResponse)
chisq.test(ControlTable)
ControlTable


###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(SRPrimeCorrect,SRPrimeCorrect$Site!="mTurk")
First80<-subset(Pool,Pool$DaysInLab<.8)
Last20<-subset(Pool,Pool$DaysInLab>=.8)
range(First80$DaysInLab)
range(Last20$DaysInLab)

#First 80
First80Table=table(First80$PrimeCons)
chisq.test(First80Table)
First80Table
573+452
chies(14.2839,1025)

#Last 20
Last20Table=table(Last20$PrimeCons)
chisq.test(Last20Table)
Last20Table
159+111
chies(8.5333,270)

###Mixed Effects Model
str(Pool$DaysInLab)
str(Pool$PrimeCons)

PrimeCons.Uncond<-glmer(PrimeCons~1+(1|Site),data=Pool,family=binomial)
summary(PrimeCons.Uncond)
4e-14/(pi^2/3)

#Metaphoric Restructuring Full Model Output
PrimeCons.MEmodel<-glmer(PrimeCons~DaysInLab+(1|Site),data=Pool,family=binomial)
summary(PrimeCons.MEmodel)
plot(allEffects(PrimeCons.MEmodel))
qqPlot(resid(PrimeCons.MEmodel))

PrimeCons.Null<-glmer(PrimeCons~1+(1|Site),data=Pool,family=binomial)
PrimeCons.MEmodel2<-glmer(PrimeCons~DaysInLab+(1|Site),data=Pool,family=binomial)
anova(PrimeCons.Null,PrimeCons.MEmodel2)
coef(PrimeCons.MEmodel2)

#Follow up
Meta<-subset(Pool,DaysInLab!="NA")
Prime.glm<-glm(PrimeCons~Site,data=Meta,family=binomial)
Prime2.glm<-glm(PrimeCons~Site*DaysInLab,data=Meta,family=binomial)
summary(Prime.glm)
summary(Prime2.glm)
Anova(Prime2.glm,type="II")
anova(Prime.glm,Prime2.glm)

Prime3.glm<-glm(PrimeCons~Site+DaysInLab,data=Pool,family=binomial)
summary(Prime3.glm)
coef(Prime3.glm)
Anova(Prime3.glm,type="II")


###############Moderator and Order Analyses#################

###Moderators
str(Pool$AttentionCheck)
str(Pool$PrimeCons)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-glm(PrimeCons~AttentionCheck,data=Pool,family=binomial)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-glm(PrimeCons~ReportedAttention,data=Pool,family=binomial)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-glm(PrimeCons~ReportedEffort,data=Pool,family=binomial)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-glm(PrimeCons~Genderfactor,data=Pool,family=binomial)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)

str(Pool$Conscientiousness)
Conscientiousness<-glm(PrimeCons~Conscientiousness,data=Pool,family=binomial)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-glm(PrimeCons~Mood,data=Pool,family=binomial)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-glm(PrimeCons~Stress,data=Pool,family=binomial)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)

MetaSum<-summarise(group_by(SRPrimeCorrect,Site),FirstN=sum(PrimeCons=="Consistent",na.rm=TRUE),ThirdN=sum(PrimeCons=="Inconsistent",na.rm=TRUE))
MetaSum
setwd("/Users/Charlie/Desktop")
write.csv(MetaSum,file="MetaSum.csv",row.names=FALSE)


###############Effect by Site##############

Ashland<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="AshlandUniversity")
length(Ashland$session_id)
AshlandTable<-table(Ashland$PrimeCons)
AshlandTable
chisq.test(AshlandTable)
chies(chisq.test(AshlandTable)$statistic,sum(AshlandTable))


Bradley<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="BradleyUniversity")
length(Bradley$session_id)
BradleyTable<-table(Bradley$PrimeCons)
BradleyTable
chisq.test(BradleyTable)
chies(chisq.test(BradleyTable)$statistic,sum(BradleyTable))


Carleton<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="CarletonUniversity")
length(Carleton$session_id)
CarletonTable<-table(Carleton$PrimeCons)
CarletonTable
chisq.test(CarletonTable)
chies(chisq.test(CarletonTable)$statistic,sum(CarletonTable))


Ithaca<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="IthacaCollege")
length(Ithaca$session_id)
IthacaTable<-table(Ithaca$PrimeCons)
IthacaTable
chisq.test(IthacaTable)
chies(chisq.test(IthacaTable)$statistic,sum(IthacaTable))

Miami<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="MiamiUniversity")
length(Miami$session_id)
MiamiTable<-table(Miami$PrimeCons)
MiamiTable
chisq.test(MiamiTable)
chies(chisq.test(MiamiTable)$statistic,sum(MiamiTable))

MichSt<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="MichiganStateUniversity")
length(MichSt$session_id)
MichStTable<-table(MichSt$PrimeCons)
MichStTable
chisq.test(MichStTable)
chies(chisq.test(MichStTable)$statistic,sum(MichStTable))



Montana<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="MontanaStateUniversity")
length(Montana$session_id)
MontanaTable<-table(Montana$PrimeCons)
MontanaTable
chisq.test(MontanaTable)
chies(chisq.test(MontanaTable)$statistic,sum(MontanaTable))

Nova<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
NovaTable<-table(Nova$PrimeCons)
NovaTable
chisq.test(NovaTable)
chies(chisq.test(NovaTable)$statistic,sum(NovaTable))


OSU<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="OSUNewark")
length(OSU$session_id)
OSUTable<-table(OSU$PrimeCons)
OSUTable
chisq.test(OSUTable)
chies(chisq.test(OSUTable)$statistic,sum(OSUTable))

PLU<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="PacificLutheranUniversity")
length(PLU$session_id)
PLUTable<-table(PLU$PrimeCons)
PLUTable
chisq.test(PLUTable)
chies(chisq.test(PLUTable)$statistic,sum(PLUTable))


Penn<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="PennStateAbington")
length(Penn$session_id)
PennTable<-table(Penn$PrimeCons)
PennTable
chisq.test(PennTable)
chies(chisq.test(PennTable)$statistic,sum(PennTable))

SDSU<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
SDSUTable<-table(SDSU$PrimeCons)
SDSUTable
chisq.test(SDSUTable)
chies(chisq.test(SDSUTable)$statistic,sum(SDSUTable))


Texas<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="TexasAandM")
length(Texas$session_id)
TexasTable<-table(Texas$PrimeCons)
TexasTable
chisq.test(TexasTable)
chies(chisq.test(TexasTable)$statistic,sum(TexasTable))

Davis<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="UCDavis")
length(Davis$session_id)
DavisTable<-table(Davis$PrimeCons)
DavisTable
chisq.test(DavisTable)
chies(chisq.test(DavisTable)$statistic,sum(DavisTable))

Riverside<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="UCRiverside")
length(Riverside$session_id)
RiversideTable<-table(Riverside$PrimeCons)
RiversideTable
chisq.test(RiversideTable)
chies(chisq.test(RiversideTable)$statistic,sum(RiversideTable))


Florida<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="UniversityOfFlorida")
length(Florida$session_id)
FloridaTable<-table(Florida$PrimeCons)
FloridaTable
chisq.test(FloridaTable)
chies(chisq.test(FloridaTable)$statistic,sum(FloridaTable))


Mississippi<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
MississippiTable<-table(Mississippi$PrimeCons)
MississippiTable
chisq.test(MississippiTable)
chies(chisq.test(MississippiTable)$statistic,sum(MississippiTable))

Toronto<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="UniversityOfToronto")
length(Toronto$session_id)
TorontoTable<-table(Toronto$PrimeCons)
TorontoTable
chisq.test(TorontoTable)
chies(chisq.test(TorontoTable)$statistic,sum(TorontoTable))


Virginia<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="UniversityOfVirginia")
length(Virginia$session_id)
VirginiaTable<-table(Virginia$PrimeCons)
VirginiaTable
chisq.test(VirginiaTable)
chies(chisq.test(VirginiaTable)$statistic,sum(VirginiaTable))

VCU<-subset(SRPrimeCorrect,SRPrimeCorrect$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
VCUTable<-table(VCU$PrimeCons)
VCUTable
chisq.test(VCUTable)
chies(chisq.test(VCUTable)$statistic,sum(VCUTable))

#####Task Order Effects####
head(SRPrimeCorrect)
str(SRPrimeCorrect$inlab_order)
str(SRPrimeCorrect$PrimeCons)
Order.glm<-glm(PrimeCons~inlab_order,data=SRPrimeCorrect,family=binomial)
Anova(Order.glm,type="II")
summary(Order.glm,type="II")
chies(1.2269e-05,1211)

SRPrimeCorrect$inlab_ordersquare<-SRPrimeCorrect$inlab_order^2
OrderQuad.glm<-glm(PrimeCons~inlab_ordersquare,data=SRPrimeCorrect, family=binomial)
Anova(OrderQuad.glm,type="II")
summary(OrderQuad.glm)
chies(0.28577,1211)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
SRPrimeCorrect<-merge(SRPrimeCorrect,Order2,by="session_id",all=TRUE)
head(SRPrimeCorrect)

str(SRPrimeCorrect$MetaphorOrder10)
str(SRPrimeCorrect$PrimeCons)
Order.glm<-glm(PrimeCons~MetaphorOrder10,data=SRPrimeCorrect,family=binomial)
Anova(Order.glm,type="II")
summary(Order.glm,type="II")
chies(0.025564,1211)

SRPrimeCorrect$MetaphorOrder10square<-SRPrimeCorrect$MetaphorOrder10^2
OrderQuad.glm<-glm(PrimeCons~MetaphorOrder10square,data=SRPrimeCorrect, family=binomial)
Anova(OrderQuad.glm,type="II")
summary(OrderQuad.glm)
chies(0.17098,1211)

##This effect First
First<-subset(SRPrimeCorrect,MetaphorOrder10==1)

PrimeTable=table(First$PrimeCons)
chisq.test(PrimeTable)
PrimeTable
chies(0.0833,48)


