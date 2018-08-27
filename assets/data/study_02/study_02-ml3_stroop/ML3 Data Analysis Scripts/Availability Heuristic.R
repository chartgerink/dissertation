#############MANY LABS 3 AVAILABILITY HEURISTIC##################
#######Charlie Ebersole, Begin January 21, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(compute.es)

###Calculating whether individual selected first or third position more

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
ML3$K1st[ML3$kposition==1]<-1
ML3$L1st[ML3$lposition==1]<-1
ML3$N1st[ML3$nposition==1]<-1
ML3$R1st[ML3$rposition==1]<-1
ML3$V1st[ML3$vposition==1]<-1
ML3$K1st[ML3$kposition==2]<-0
ML3$L1st[ML3$lposition==2]<-0
ML3$N1st[ML3$nposition==2]<-0
ML3$R1st[ML3$rposition==2]<-0
ML3$V1st[ML3$vposition==2]<-0
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
AvailScore<-subset(ML3,ML3$AvailFirst!="NA")
length(AvailScore$session_id)
ML3$AvailSign[ML3$AvailFirst==0]<-"-"
ML3$AvailSign[ML3$AvailFirst==1]<-"-"
ML3$AvailSign[ML3$AvailFirst==2]<-"-"
ML3$AvailSign[ML3$AvailFirst==3]<-"+"
ML3$AvailSign[ML3$AvailFirst==4]<-"+"
ML3$AvailSign[ML3$AvailFirst==5]<-"+"
list(ML3$AvailSign)

PlusTotal<-sum(ML3$AvailSign=="+",na.rm=TRUE)
MinusTotal<-sum(ML3$AvailSign=="-",na.rm=TRUE)
PlusTotal+MinusTotal

#total mTurk Numbers
mTurk<-subset(ML3,ML3$Site=="mTurk")
PlusTotal2<-sum(mTurk$AvailSign=="+",na.rm=TRUE)
MinusTotal2<-sum(mTurk$AvailSign=="-",na.rm=TRUE)
PlusTotal2+MinusTotal2

##############ORDER OF PRESENTATION##############
head(ML3)
dealing with order
ML3$availk_order
ML3$availl_order
ML3$availk_order<ML3$availl_order

###The Letter K###

ML3$AvailOrderK[(ML3$availk_order<ML3$availl_order)+(ML3$availk_order<ML3$availn_order)+(ML3$availk_order<ML3$availr_order)+(ML3$availk_order<ML3$availv_order)==4]<-1

ML3$AvailOrderK[(ML3$availk_order<ML3$availl_order)+(ML3$availk_order<ML3$availn_order)+(ML3$availk_order<ML3$availr_order)+(ML3$availk_order<ML3$availv_order)==3]<-2

ML3$AvailOrderK[(ML3$availk_order<ML3$availl_order)+(ML3$availk_order<ML3$availn_order)+(ML3$availk_order<ML3$availr_order)+(ML3$availk_order<ML3$availv_order)==2]<-3

ML3$AvailOrderK[(ML3$availk_order<ML3$availl_order)+(ML3$availk_order<ML3$availn_order)+(ML3$availk_order<ML3$availr_order)+(ML3$availk_order<ML3$availv_order)==1]<-4

ML3$AvailOrderK[(ML3$availk_order<ML3$availl_order)+(ML3$availk_order<ML3$availn_order)+(ML3$availk_order<ML3$availr_order)+(ML3$availk_order<ML3$availv_order)==0]<-5
list(ML3$AvailOrderK)

###The Letter L###

ML3$AvailOrderL[(ML3$availl_order<ML3$availk_order)+(ML3$availl_order<ML3$availn_order)+(ML3$availl_order<ML3$availr_order)+(ML3$availl_order<ML3$availv_order)==4]<-1

ML3$AvailOrderL[(ML3$availl_order<ML3$availk_order)+(ML3$availl_order<ML3$availn_order)+(ML3$availl_order<ML3$availr_order)+(ML3$availl_order<ML3$availv_order)==3]<-2

ML3$AvailOrderL[(ML3$availl_order<ML3$availk_order)+(ML3$availl_order<ML3$availn_order)+(ML3$availl_order<ML3$availr_order)+(ML3$availl_order<ML3$availv_order)==2]<-3

ML3$AvailOrderL[(ML3$availl_order<ML3$availk_order)+(ML3$availl_order<ML3$availn_order)+(ML3$availl_order<ML3$availr_order)+(ML3$availl_order<ML3$availv_order)==1]<-4

ML3$AvailOrderL[(ML3$availl_order<ML3$availk_order)+(ML3$availl_order<ML3$availn_order)+(ML3$availl_order<ML3$availr_order)+(ML3$availl_order<ML3$availv_order)==0]<-5

list(ML3$AvailOrderL)

###The Letter N###

ML3$AvailOrderN[(ML3$availn_order<ML3$availk_order)+(ML3$availn_order<ML3$availl_order)+(ML3$availn_order<ML3$availr_order)+(ML3$availn_order<ML3$availv_order)==4]<-1

ML3$AvailOrderN[(ML3$availn_order<ML3$availk_order)+(ML3$availn_order<ML3$availl_order)+(ML3$availn_order<ML3$availr_order)+(ML3$availn_order<ML3$availv_order)==3]<-2

ML3$AvailOrderN[(ML3$availn_order<ML3$availk_order)+(ML3$availn_order<ML3$availl_order)+(ML3$availn_order<ML3$availr_order)+(ML3$availn_order<ML3$availv_order)==2]<-3

ML3$AvailOrderN[(ML3$availn_order<ML3$availk_order)+(ML3$availn_order<ML3$availl_order)+(ML3$availn_order<ML3$availr_order)+(ML3$availn_order<ML3$availv_order)==1]<-4

ML3$AvailOrderN[(ML3$availn_order<ML3$availk_order)+(ML3$availn_order<ML3$availl_order)+(ML3$availn_order<ML3$availr_order)+(ML3$availn_order<ML3$availv_order)==0]<-5

list(ML3$AvailOrderN)

###The Letter R###

ML3$AvailOrderR[(ML3$availr_order<ML3$availk_order)+(ML3$availr_order<ML3$availl_order)+(ML3$availr_order<ML3$availn_order)+(ML3$availr_order<ML3$availv_order)==4]<-1

ML3$AvailOrderR[(ML3$availr_order<ML3$availk_order)+(ML3$availr_order<ML3$availl_order)+(ML3$availr_order<ML3$availn_order)+(ML3$availr_order<ML3$availv_order)==3]<-2

ML3$AvailOrderR[(ML3$availr_order<ML3$availk_order)+(ML3$availr_order<ML3$availl_order)+(ML3$availr_order<ML3$availn_order)+(ML3$availr_order<ML3$availv_order)==2]<-3

ML3$AvailOrderR[(ML3$availr_order<ML3$availk_order)+(ML3$availr_order<ML3$availl_order)+(ML3$availr_order<ML3$availn_order)+(ML3$availr_order<ML3$availv_order)==1]<-4

ML3$AvailOrderR[(ML3$availr_order<ML3$availk_order)+(ML3$availr_order<ML3$availl_order)+(ML3$availr_order<ML3$availn_order)+(ML3$availr_order<ML3$availv_order)==0]<-5

list(ML3$AvailOrderR)

###The Letter V###

ML3$AvailOrderV[(ML3$availv_order<ML3$availk_order)+(ML3$availv_order<ML3$availl_order)+(ML3$availv_order<ML3$availn_order)+(ML3$availv_order<ML3$availr_order)==4]<-1

ML3$AvailOrderV[(ML3$availv_order<ML3$availk_order)+(ML3$availv_order<ML3$availl_order)+(ML3$availv_order<ML3$availn_order)+(ML3$availv_order<ML3$availr_order)==3]<-2

ML3$AvailOrderV[(ML3$availv_order<ML3$availk_order)+(ML3$availv_order<ML3$availl_order)+(ML3$availv_order<ML3$availn_order)+(ML3$availv_order<ML3$availr_order)==2]<-3

ML3$AvailOrderV[(ML3$availv_order<ML3$availk_order)+(ML3$availv_order<ML3$availl_order)+(ML3$availv_order<ML3$availn_order)+(ML3$availv_order<ML3$availr_order)==1]<-4

ML3$AvailOrderV[(ML3$availv_order<ML3$availk_order)+(ML3$availv_order<ML3$availl_order)+(ML3$availv_order<ML3$availn_order)+(ML3$availv_order<ML3$availr_order)==0]<-5

list(ML3$AvailOrderV)

ML3$AvailOrderV==ML3$AvailOrderR
ML3$AvailOrderV==ML3$AvailOrderN
ML3$AvailOrderV==ML3$AvailOrderL
ML3$AvailOrderV==ML3$AvailOrderK

###Replicating Previous Effect###
#All participants with data will be included in the analysis.  We will conduct an analysis strategy very close to the original.  Participants who judge the first position to be more frequent for the majority of the letters will get a sign of +, and participants who judge the third position to be more frequent for the majority of the letters will get a sign of -. The number of +s and –s will be used in the sign test.

binom.test(PlusTotal,(PlusTotal+MinusTotal))
#Number of people that selected first position a majority of times
PlusTotal
#Number of people who selected third position a majority of times
MinusTotal


###Calculating Chi Square for effect size
AvailTable<-table(ML3$AvailSign)
AvailTable
chisq.test(AvailTable)
chies(5.9896,3088)
chies(5.9896,3088,level=99)
?chisq.test
###replication of original effect for comparison effect size
RepPlus<-(rep("+",105))
RepMinus<-(rep("-",47))
Replication<-c(RepPlus,RepMinus)
Replication
ReplicationTable<-table(Replication)
ReplicationTable
chisq.test(ReplicationTable)
chies(22.1316,152)

###########Calculating Ratios###########
ML3$kratio<-as.numeric(ML3$kratio)
ML3$lratio<-as.numeric(ML3$lratio)
ML3$nratio<-as.numeric(ML3$nratio)
ML3$rratio<-as.numeric(ML3$rratio)
ML3$vratio<-as.numeric(ML3$vratio)

Avail<-ML3
Avail$Ratio<-Avail$kratio+Avail$lratio+Avail$nratio+Avail$rratio+Avail$vratio
str(Avail$Ratio)
range(Avail$Ratio,na.rm=TRUE)
Avail<-subset(Avail,Avail$Ratio>0)
range(Avail$Ratio,na.rm=TRUE)
list(Avail$Ratio,na.rm=TRUE)
Avail$Ratio2<-(Avail$Ratio/10)/5
list(Avail$Ratio2)
length(Avail$Ratio2)
mean(Avail$Ratio2)
sd(Avail$Ratio2)
hist(Avail$Ratio2)
plot(density(Avail$Ratio2))
range(Avail$Ratio2)
list(Avail$Ratio2)


Check<-subset(Avail,Avail$Ratio2<100)
range(Check$Ratio2)
#this removes 173.5 and 44.60

t.test(Avail$Ratio2,y=NULL,alternative="two.sided",mu=1,var.equal=FALSE)

t.test(Check$Ratio2,y=NULL,alternative="two.sided",mu=1,var.equal=FALSE)


###Calculating better ratio (reported in manuscript)###

Avail$RatioNew<-Avail$Ratio/5

setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
write.csv(Avail,file="Avail.csv",row.names=FALSE)
AvailClean<-read.csv(file="AvailClean.csv",header=TRUE)
#Data cleaned in excel

mean(AvailClean$RatioClean)
sd(AvailClean$RatioClean)
range(AvailClean$RatioClean)

t.test(AvailClean$RatioClean,y=NULL,alternative="two.sided",mu=0,var.equal=FALSE)
mean(AvailClean$RatioClean)/sd(AvailClean$RatioClean)

###Follow Up Analyses###
#With our focus on effect size and different response options, we also considering a follow-up analysis strategy with the ratio responses.  The following is our present plan for that analysis, but this will undergo more intensive review prior to observing the outcomes of the data collection.  The estimated number of occurrence of R in the third position (O3) compared to the first position is the primary variable for analysis.  We will recode the data for each response as follows:

#If O3 > 10, then SCORE = O3/10 - 1
#If O3 = 10, then SCORE = 0
#If O3 < 10, then SCORE = 1 - 10/O3 

#This rescales the ratio estimate to theoretically normalize the distribution and recenters on 0 indicating no difference in frequency estimates.  Then, the five ratings will be averaged to create a single index of relative estimation for first versus third letter.  It is conceivable that the distributions for these responses will be unusual and require some adjustment in data preparation prior to inferential testing. Using a t-test, the mean rating will be compared against zero indicating no difference in frequency estimates between first and third position.  Another possibility is to rescale the distribution for each letter so that 0 indicates the actual difference in frequency of 1st and 3rd position, with positive values indicating overestimation of 1st position and negative values indicating overestimation of 3rd position.  The latter would be more definitively an estimate of the magnitude of the misperception. 


##Effect of Order on Responses
#Finally, as a secondary test, we will examine the effect of order on responses.  Is the overestimation of 1st position more extreme for the first estimate compared to the others?  This was not examined in the original research, but it is possible that the effect is dampened with multiple assessments as participants have a meta-experience of realizing that not every letter could be more frequent in the first than third position.

K1<-subset(ML3,ML3$AvailOrderK==1)
K2<-subset(ML3,ML3$AvailOrderK==2)
K3<-subset(ML3,ML3$AvailOrderK==3)
K4<-subset(ML3,ML3$AvailOrderK==4)
K5<-subset(ML3,ML3$AvailOrderK==5)

L1<-subset(ML3,ML3$AvailOrderL==1)
L2<-subset(ML3,ML3$AvailOrderL==2)
L3<-subset(ML3,ML3$AvailOrderL==3)
L4<-subset(ML3,ML3$AvailOrderL==4)
L5<-subset(ML3,ML3$AvailOrderL==5)

N1<-subset(ML3,ML3$AvailOrderN==1)
N2<-subset(ML3,ML3$AvailOrderN==2)
N3<-subset(ML3,ML3$AvailOrderN==3)
N4<-subset(ML3,ML3$AvailOrderN==4)
N5<-subset(ML3,ML3$AvailOrderN==5)

R1<-subset(ML3,ML3$AvailOrderR==1)
R2<-subset(ML3,ML3$AvailOrderR==2)
R3<-subset(ML3,ML3$AvailOrderR==3)
R4<-subset(ML3,ML3$AvailOrderR==4)
R5<-subset(ML3,ML3$AvailOrderR==5)

V1<-subset(ML3,ML3$AvailOrderV==1)
V2<-subset(ML3,ML3$AvailOrderV==2)
V3<-subset(ML3,ML3$AvailOrderV==3)
V4<-subset(ML3,ML3$AvailOrderV==4)
V5<-subset(ML3,ML3$AvailOrderV==5)

list(L1$AvailOrderL)
length(L1$session_id)

K1table<-table(K1$kposition)
K1table
L1table<-table(L1$lposition)
L1table
N1table<-table(N1$nposition)
N1table
R1table<-table(R1$rposition)
R1table
V1table<-table(V1$vposition)
V1table
FirstTable<-K1table+L1table+N1table+R1table+V1table
FirstTable

K2table<-table(K2$kposition)
K2table
L2table<-table(L2$lposition)
L2table
N2table<-table(N2$nposition)
N2table
R2table<-table(R2$rposition)
R2table
V2table<-table(V2$vposition)
V2table
SecondTable<-K2table+L2table+N2table+R2table+V2table
SecondTable

K3table<-table(K3$kposition)
K3table
L3table<-table(L3$lposition)
L3table
N3table<-table(N3$nposition)
N3table
R3table<-table(R3$rposition)
R3table
V3table<-table(V3$vposition)
V3table
ThirdTable<-K3table+L3table+N3table+R3table+V3table
ThirdTable

K4<-subset(K4,K4$kposition<6)
K4table<-table(K4$kposition)
K4table
L4table<-table(L4$lposition)
L4table
N4table<-table(N4$nposition)
N4table
R4table<-table(R4$rposition)
R4table
V4table<-table(V4$vposition)
V4table
FourthTable<-K4table+L4table+N4table+R4table+V4table
FourthTable

K5table<-table(K5$kposition)
K5table
L5table<-table(L5$lposition)
L5table
N5table<-table(N5$nposition)
N5table
R5table<-table(R5$rposition)
R5table
V5table<-table(V5$vposition)
V5table
FifthTable<-K5table+L5table+N5table+R5table+V5table
FifthTable


FirstTable
chisq.test(FirstTable)
sum(FirstTable)
chies(6.9573,3234)

SecondTable
chisq.test(SecondTable)
sum(SecondTable)
chies(0.0152, 3219)

ThirdTable
chisq.test(ThirdTable)
sum(ThirdTable)
chies(6.7772,3232)

FourthTable
chisq.test(FourthTable)
sum(FourthTable)
chies(7.5297,3232)

FifthTable
chisq.test(FifthTable)
sum(FifthTable)
chies(1.4745,3229)



##############################Supplementary Analyses#######################################
AttentionPass<-subset(ML3,ML3$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication

PlusTotal2<-sum(AttentionPass $AvailSign=="+",na.rm=TRUE)
MinusTotal2<-sum(AttentionPass $AvailSign=="-",na.rm=TRUE)
PlusTotal2
MinusTotal2

binom.test(PlusTotal2,(PlusTotal2+MinusTotal2))


###Exploratory analyses of individual letters
#Eliminating errant response
K<-subset(ML3,ML3$kposition<6)
Ktable=table(K$kposition)
Ktable
chisq.test(Ktable)
1906+1319


Ltable=table(ML3$lposition)
Ltable
chisq.test(Ltable)
1822+1420

Ntable=table(ML3$nposition)
Ntable
chisq.test(Ntable)
1453+1783

Rtable=table(ML3$rposition)
Rtable
chisq.test(Rtable)
1589+1650

Vtable=table(ML3$vposition)
Vtable
chisq.test(Vtable)
1597+1644

###With Order Included
Ktable2=table(K$kposition,K$AvailOrderK)
Ktable2
chisq.test(Ktable2)

Ltable2=table(ML3$lposition,ML3$AvailOrderL)
Ltable2
chisq.test(Ltable2)

Ntable2=table(ML3$nposition,ML3$AvailOrderN)
Ntable2
chisq.test(Ntable2)

Rtable2=table(ML3$rposition,ML3$AvailOrderR)
Rtable2
chisq.test(Rtable2)

Vtable2=table(ML3$vposition,ML3$AvailOrderV)
Vtable2
chisq.test(Vtable2)

###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(ML3,ML3$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)


###First 80
FirstTable<-table(First80$AvailSign)
FirstTable
chisq.test(FirstTable)
chies(2.0554,1931)
997+934

###Last 20
LastTable<-table(Last20$AvailSign)
LastTable
chisq.test(LastTable)
chies(0.7067,566)
273+293

###Mixed Effects Model
str(Pool$DaysInComp)
str(Pool$AvailSign)
Pool$AvailSign<-as.factor(Pool$AvailSign)

AvailSign.Uncond<-glmer(AvailSign~1+(1|Site),data=Pool,family=binomial)
summary(AvailSign.Uncond)
4e-14/(pi^2/3)

AvailSign.MEmodel<-glmer(AvailSign~DaysInComp+(1|Site),data=Pool,family=binomial)
summary(AvailSign.MEmodel)

AvailSign.Null<-glmer(AvailSign~1+(1|Site),data=Pool,family=binomial)
AvailSign.MEmodel2<-glmer(AvailSign~DaysInComp+(1|Site),data=Pool,family=binomial)
anova(AvailSign.Null,AvailSign.MEmodel2)
coef(AvailSign.MEmodel2)



###############Moderator and Order Analyses#################
str(Pool$AvailFirst)
###Moderators
str(Pool$AttentionCheck)
str(Pool$AvailSign)
Pool$AvailSign<-as.factor(Pool$AvailSign)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-glm(AvailSign~AttentionCheck,data=Pool,family=binomial)
Anova(AttentionCheck,type="II")
summary(AttentionCheck)
chies(4.9788,2447)
plot(allEffects(AttentionCheck))
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)
t.test(AvailFirst~AttentionCheck,data=Pool)


str(Pool$ReportedAttention)
ReportedAttention<-glm(AvailSign~ReportedAttention,data=Pool,family=binomial)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-glm(AvailSign~ReportedEffort,data=Pool,family=binomial)
Anova(ReportedEffort,type="II")
summary(ReportedEffort)
chies(4.5831,2455)
plot(allEffects(ReportedEffort))
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)
ReportedEffort2<-lm(AvailFirst~ReportedEffort,data=Pool)
Anova(ReportedEffort2,type="II")

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-glm(AvailSign~Genderfactor,data=Pool,family=binomial)
Anova(Gender,type="II")
ci.pvaf(F.value= 0.0033,df.1=1,df.2=2322,N=2326,conf.level=.95)
t.test(AvailFirst~Genderfactor,data=Pool,var.equal=TRUE)
summaryBy(AvailFirst~Genderfactor,data=Pool,FUN=list(mean,sd),na.rm=TRUE)
summaryBy(AvailFirst~Genderfactor,data=Pool,FUN=list(length))
tes(-4.2251,1818,780)

str(Pool$Conscientiousness)
Conscientiousness<-glm(AvailSign~Conscientiousness,data=Pool,family=binomial)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-glm(AvailSign~Mood,data=Pool,family=binomial)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-glm(AvailSign~Stress,data=Pool,family=binomial)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)

AvailSum<-summarise(group_by(ML3,Site),FirstN=sum(AvailSign=="+",na.rm=TRUE),ThirdN=sum(AvailSign=="-",na.rm=TRUE))
AvailSum
setwd("/Users/Charlie/Desktop")
write.csv(AvailSum,file="AvailSum.csv",row.names=FALSE)



########Effect by Site#########
Ashland<-subset(ML3,ML3$Site=="AshlandUniversity")
length(Ashland$session_id)
AshlandTable<-table(Ashland$AvailSign)
AshlandTable
chisq.test(AshlandTable)
chies(chisq.test(AshlandTable)$statistic,sum(AshlandTable))


Bradley<-subset(ML3,ML3$Site=="BradleyUniversity")
length(Bradley$session_id)
BradleyTable<-table(Bradley$AvailSign)
BradleyTable
chisq.test(BradleyTable)
chies(chisq.test(BradleyTable)$statistic,sum(BradleyTable))


Carleton<-subset(ML3,ML3$Site=="CarletonUniversity")
length(Carleton$session_id)
CarletonTable<-table(Carleton$AvailSign)
CarletonTable
chisq.test(CarletonTable)
chies(chisq.test(CarletonTable)$statistic,sum(CarletonTable))


Ithaca<-subset(ML3,ML3$Site=="IthacaCollege")
length(Ithaca$session_id)
IthacaTable<-table(Ithaca$AvailSign)
IthacaTable
chisq.test(IthacaTable)
chies(chisq.test(IthacaTable)$statistic,sum(IthacaTable))

Miami<-subset(ML3,ML3$Site=="MiamiUniversity")
length(Miami$session_id)
MiamiTable<-table(Miami$AvailSign)
MiamiTable
chisq.test(MiamiTable)
chies(chisq.test(MiamiTable)$statistic,sum(MiamiTable))

MichSt<-subset(ML3,ML3$Site=="MichiganStateUniversity")
length(MichSt$session_id)
MichStTable<-table(MichSt$AvailSign)
MichStTable
chisq.test(MichStTable)
chies(chisq.test(MichStTable)$statistic,sum(MichStTable))



Montana<-subset(ML3,ML3$Site=="MontanaStateUniversity")
length(Montana$session_id)
MontanaTable<-table(Montana$AvailSign)
MontanaTable
chisq.test(MontanaTable)
chies(chisq.test(MontanaTable)$statistic,sum(MontanaTable))

Nova<-subset(ML3,ML3$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
NovaTable<-table(Nova$AvailSign)
NovaTable
chisq.test(NovaTable)
chies(chisq.test(NovaTable)$statistic,sum(NovaTable))


OSU<-subset(ML3,ML3$Site=="OSUNewark")
length(OSU$session_id)
OSUTable<-table(OSU$AvailSign)
OSUTable
chisq.test(OSUTable)
chies(chisq.test(OSUTable)$statistic,sum(OSUTable))

PLU<-subset(ML3,ML3$Site=="PacificLutheranUniversity")
length(PLU$session_id)
PLUTable<-table(PLU$AvailSign)
PLUTable
chisq.test(PLUTable)
chies(chisq.test(PLUTable)$statistic,sum(PLUTable))


Penn<-subset(ML3,ML3$Site=="PennStateAbington")
length(Penn$session_id)
PennTable<-table(Penn$AvailSign)
PennTable
chisq.test(PennTable)
chies(chisq.test(PennTable)$statistic,sum(PennTable))

SDSU<-subset(ML3,ML3$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
SDSUTable<-table(SDSU$AvailSign)
SDSUTable
chisq.test(SDSUTable)
chies(chisq.test(SDSUTable)$statistic,sum(SDSUTable))


Texas<-subset(ML3,ML3$Site=="TexasAandM")
length(Texas$session_id)
TexasTable<-table(Texas$AvailSign)
TexasTable
chisq.test(TexasTable)
chies(chisq.test(TexasTable)$statistic,sum(TexasTable))

Davis<-subset(ML3,ML3$Site=="UCDavis")
length(Davis$session_id)
DavisTable<-table(Davis$AvailSign)
DavisTable
chisq.test(DavisTable)
chies(chisq.test(DavisTable)$statistic,sum(DavisTable))

Riverside<-subset(ML3,ML3$Site=="UCRiverside")
length(Riverside$session_id)
RiversideTable<-table(Riverside$AvailSign)
RiversideTable
chisq.test(RiversideTable)
chies(chisq.test(RiversideTable)$statistic,sum(RiversideTable))


Florida<-subset(ML3,ML3$Site=="UniversityOfFlorida")
length(Florida$session_id)
FloridaTable<-table(Florida$AvailSign)
FloridaTable
chisq.test(FloridaTable)
chies(chisq.test(FloridaTable)$statistic,sum(FloridaTable))


Mississippi<-subset(ML3,ML3$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
MississippiTable<-table(Mississippi$AvailSign)
MississippiTable
chisq.test(MississippiTable)
chies(chisq.test(MississippiTable)$statistic,sum(MississippiTable))

Toronto<-subset(ML3,ML3$Site=="UniversityOfToronto")
length(Toronto$session_id)
TorontoTable<-table(Toronto$AvailSign)
TorontoTable
chisq.test(TorontoTable)
chies(chisq.test(TorontoTable)$statistic,sum(TorontoTable))


Virginia<-subset(ML3,ML3$Site=="UniversityOfVirginia")
length(Virginia$session_id)
VirginiaTable<-table(Virginia$AvailSign)
VirginiaTable
chisq.test(VirginiaTable)
chies(chisq.test(VirginiaTable)$statistic,sum(VirginiaTable))

VCU<-subset(ML3,ML3$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
VCUTable<-table(VCU$AvailSign)
VCUTable
chisq.test(VCUTable)
chies(chisq.test(VCUTable)$statistic,sum(VCUTable))

mTurk<-subset(ML3,ML3$Site=="mTurk")
length(mTurk$session_id)
mTurkTable<-table(mTurk$AvailSign)
mTurkTable
chisq.test(mTurkTable)
chies(chisq.test(mTurkTable)$statistic,sum(mTurkTable))

#####Task Order Effects####
head(ML3)
str(ML3$availinstruct_order)
str(ML3$AvailSign)
ML3$AvailSign<-as.factor(ML3$AvailSign)
Order.glm<-glm(AvailSign~availinstruct_order,data=ML3,family=binomial)
Anova(Order.glm,type="II")
summary(Order.glm,type="II")
chies(3.4068,3088)

ML3$availinstruct_ordersquare<-ML3$availinstruct_order^2
OrderQuad.glm<-glm(AvailSign~availinstruct_ordersquare,data=ML3, family=binomial)
Anova(OrderQuad.glm,type="II")
summary(OrderQuad.glm)
chies(2.8296,3088)


Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
ML3<-merge(ML3,Order2,by="session_id",all=TRUE)
head(ML3)

str(ML3$AvailOrder10)
Order.glm<-glm(AvailSign~AvailOrder10,data=ML3,family=binomial)
Anova(Order.glm,type="II")
summary(Order.glm,type="II")
chies(3.1096,3088)

ML3$AvailOrder10square<-ML3$AvailOrder10^2
OrderQuad.glm<-glm(AvailSign~AvailOrder10square,data=ML3, family=binomial)
Anova(OrderQuad.glm,type="II")
summary(OrderQuad.glm)
chies(2.1696,3088)

First<-subset(ML3,AvailOrder10.x==1)
AvailTable<-table(First$AvailSign)
AvailTable
chisq.test(AvailTable)
164+208
chies(5.9896,372)
