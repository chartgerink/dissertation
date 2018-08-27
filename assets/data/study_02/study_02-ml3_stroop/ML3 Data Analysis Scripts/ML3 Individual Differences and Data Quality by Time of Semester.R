#############MANY LABS 3 INDIVIDUAL DIFFERENCES BY TIME##################
#######Charlie Ebersole, Begin January 31, 2014###########

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(lme4)
require(compute.es)

length(unique(ML3$session_id))

Pool<-subset(ML3,ML3$Site!="mTurk")
length(unique(Pool$session_id))

#######################Exploratory Examination of Individual Differences####################




head(Pool)
range(Pool$DaysInComp,na.rm=TRUE)

###Attention###
table(Pool$AttentionCheck)
Fail<-subset(Pool,Pool$AttentionCheck=="Fail")
Pass<-subset(Pool,Pool$AttentionCheck=="Pass")
hist(Fail$DaysInComp)
plot(density(Fail$DaysInComp))
hist(Pass$DaysInComp)
plot(density(Pass$DaysInComp))

#############BIG FIVE###############
###Openness
summary(Pool$Openness)
sd(Pool$Openness,na.rm=TRUE)
hist(Pool$Openness)
cor(Pool$DaysInComp,Pool$Openness,use="complete.obs",method="pearson")

###Conscientiousness
summary(Pool$Conscientiousness)
sd(Pool$Conscientiousness,na.rm=TRUE)
hist(Pool$Conscientiousness)
cor(Pool$DaysInComp,Pool$Conscientiousness,use="complete.obs",method="pearson")

###Extraversion
summary(Pool$Extraversion)
sd(Pool$Extraversion,na.rm=TRUE)
hist(Pool$Extraversion)
cor(Pool$DaysInComp,Pool$Extraversion,use="complete.obs",method="pearson")

###Agreeableness
summary(Pool$Agreeableness)
sd(Pool$Agreeableness,na.rm=TRUE)
hist(Pool$Agreeableness)
cor(Pool$DaysInComp,Pool$Agreeableness,use="complete.obs",method="pearson")

###Neuroticism
summary(Pool$Neuroticism)
sd(Pool$Neuroticism,na.rm=TRUE)
hist(Pool$Neuroticism)
cor(Pool$DaysInComp,Pool$Neuroticism,use="complete.obs",method="pearson")

#############INTRINSIC MOTIVATION###############
summary(Pool$Intrinsic)
sd(Pool$Intrinsic,na.rm=TRUE)
hist(Pool$Intrinsic)
cor(Pool$DaysInComp,Pool$Intrinsic,use="complete.obs",method="pearson")

#############MOOD###############
summary(Pool$Mood)
sd(Pool$Mood,na.rm=TRUE)
hist(Pool$Mood)
cor(Pool$DaysInComp,Pool$Mood,use="complete.obs",method="pearson")

#############NEED FOR COGNITION###############
summary(Pool$NFC)
sd(Pool$NFC,na.rm=TRUE)
hist(Pool$NFC)
cor(Pool$DaysInComp,Pool$NFC,use="complete.obs",method="pearson")

#############REPORTED EFFORT###############
#1(no effort)-5(tried my hardest) scale
summary(Pool$ReportedEffort)
sd(Pool$ReportedEffort,na.rm=TRUE)
hist(Pool$ReportedEffort)
cor(Pool$DaysInComp,Pool$ReportedEffort,use="complete.obs",method="pearson")

#############REPORTED ATTENTION###############
#1(none)-5(I gave my undivided attention) scale
summary(Pool$ReportedAttention)
sd(Pool$ReportedAttention,na.rm=TRUE)
hist(Pool$ReportedAttention)
cor(Pool$DaysInComp,Pool$ReportedAttention,use="complete.obs",method="pearson")

#############SELF-ESTEEM###############
#1(not very true of me)-7(very true of me) scale
#statement is: I have high self-esteem
summary(Pool$SelfEsteem)
sd(Pool$SelfEsteem,na.rm=TRUE)
hist(Pool$SelfEsteem)
cor(Pool$DaysInComp,Pool$SelfEsteem,use="complete.obs",method="pearson")

#############STRESS###############
#Higher scores indicate more stress
summary(Pool$Stress)
sd(Pool$Stress,na.rm=TRUE)
hist(Pool$Stress)
cor(Pool$DaysInComp,Pool$Stress,use="complete.obs",method="pearson")




#####################Pre-Registered Examination of Individual Differences####################
#Begin February 3
########Data Quality Indicators#########

#############REPORTED EFFORT###############
#1(no effort)-5(tried my hardest) scale
str(Pool$ReportedEffort)
mean(Pool$ReportedEffort,na.rm=TRUE)
sd(Pool$ReportedEffort,na.rm=TRUE)

ReportedEffort.Uncond<-lmer(ReportedEffort~1+(1|Site),data=Pool)
summary(ReportedEffort.Uncond)
0.01503/(0.01503+ 0.58694) (2.5%)

ReportedEffort.MEmodel<-lmer(ReportedEffort~DaysInComp+(1|Site),data=Pool)
summary(ReportedEffort.MEmodel)

ReportedEffort.MEmodel.null<-lmer(ReportedEffort~1+(1|Site),data=Pool,REML=FALSE)
ReportedEffort.MEmodel.test<-lmer(ReportedEffort~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(ReportedEffort.MEmodel.null,ReportedEffort.MEmodel.test)
coef(ReportedEffort.MEmodel.test)


#############REPORTED ATTENTION###############
#1(none)-5(I have my undivided attention) scale
mean(Pool$ReportedAttention,na.rm=TRUE)
sd(Pool$ReportedAttention,na.rm=TRUE)

ReportedAttention.Uncond<-lmer(ReportedAttention~1+(1|Site),data=Pool)
summary(ReportedAttention.Uncond)
0.008686/(0.008686+ 0.535001) #1.6%

ReportedAttention.MEmodel<-lmer(ReportedAttention~DaysInComp+(1 |Site),data=Pool)
summary(ReportedAttention.MEmodel)

ReportedAttention.MEmodel.null<-lmer(ReportedAttention~1+(1|Site),data=Pool,REML=FALSE)
ReportedAttention.MEmodel.test<-lmer(ReportedAttention~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(ReportedAttention.MEmodel.null,ReportedAttention.MEmodel.test)
coef(ReportedAttention.MEmodel.test)

cor(Pool$DaysInComp,Pool$ReportedEffort,use="complete.obs",method="pearson")
cor.test(Pool$DaysInComp,Pool$ReportedEffort,alternative="two.sided",method="pearson",conf.level=0.95)
res(-0.1059892,var.r=NULL,2628)

###Attention Check###
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
summary(Pool$AttentionCheck)
978/(978+1643)
Fail<-subset(Pool,AttentionCheck=="Fail")
Pass<-subset(Pool,AttentionCheck=="Pass")
t.test(ReportedAttention~AttentionCheck,data=Pool,var.equal=TRUE)
tes(8.2016,1643,978)
sd(Fail$ReportedAttention,na.rm=TRUE)
sd(Pass$ReportedAttention,na.rm=TRUE)

AttentionCheck.Uncond<-glmer(AttentionCheck~1+(1|Site),data=Pool,family=binomial)
summary(AttentionCheck.Uncond)
0.131/(pi^2/3)

AttentionCheck.MEmodel<-glmer(AttentionCheck~DaysInComp+(1|Site),data=Pool,family=binomial)
summary(AttentionCheck.MEmodel)
plot(allEffects(AttentionCheck.MEmodel))
AttentionCheck.Null<-glmer(AttentionCheck~1+(1|Site),data=Pool,family=binomial)
AttentionCheck.MEmodel2<-glmer(AttentionCheck~DaysInComp+(1|Site),data=Pool,family=binomial)
anova(AttentionCheck.Null,AttentionCheck.MEmodel2)
coef(AttentionCheck.MEmodel2)
chies(6.75,2621)
Check.glm<-glm(AttentionCheck~DaysInComp,data=Pool,family=binomial)
summary(Check.glm)
Anova(Check.glm)
chies(15.863,2621)

cor(Pool$DaysInComp,Pool$ReportedAttention,use="complete.obs",method="pearson")
cor.test(Pool$DaysInComp,Pool$ReportedAttention,alternative="two.sided",method="pearson",conf.level=0.95)
res(-0.0819927,var.r=NULL,2630)

#############BIG FIVE###############
###Openness
Openness.Uncond<-lmer(Openness~1+(1|Site),data=Pool)
summary(Openness.Uncond)
0.02119/(0.02119+ 1.17241)

Openness.MEmodel<-lmer(Openness~DaysInComp+(1|Site),data=Pool)
summary(Openness.MEmodel)

Openness.Null<-lmer(Openness~1+(1|Site),data=Pool,REML=FALSE)
Openness.MEmodel2<-lmer(Openness~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Openness.Null,Openness.MEmodel2)
coef(Openness.MEmodel2)

###Conscientiousness
Conscientiousness.Uncond<-lmer(Conscientiousness~1+(1|Site),data=Pool)
summary(Conscientiousness.Uncond)
0.05965/(0.05965+ 1.35824)

Conscientiousness.MEmodel<-lmer(Conscientiousness~DaysInComp+(1 |Site),data=Pool)
summary(Conscientiousness.MEmodel)

Conscientiousness.MEmodel.null<-lmer(Conscientiousness~1+(1|Site),data=Pool,REML=FALSE)
Conscientiousness.MEmodel.test<-lmer(Conscientiousness~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Conscientiousness.MEmodel.null,Conscientiousness.MEmodel.test)
coef(Conscientiousness.MEmodel.test)

cor(Pool$DaysInComp,Pool$Conscientiousness,use="complete.obs",method="pearson")
cor.test(Pool$DaysInComp,Pool$Conscientiousness,alternative="two.sided",method="pearson",conf.level=0.95)
res(-0.1402307,var.r=NULL,2628)

###Extraversion
Extraversion.Uncond<-lmer(Extraversion~1+(1|Site),data=Pool)
summary(Extraversion.Uncond)
0.05089/(0.05089+ 2.27975)

Extraversion.MEmodel<-lmer(Extraversion~DaysInComp+(1 |Site),data=Pool)
summary(Extraversion.MEmodel)

Extraversion.MEmodel.null<-lmer(Extraversion~1+(1|Site),data=Pool,REML=FALSE)
Extraversion.MEmodel.test<-lmer(Extraversion~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Extraversion.MEmodel.null,Extraversion.MEmodel.test)
coef(Extraversion.MEmodel.test)


###Agreeableness
Agreeableness.Uncond<-lmer(Agreeableness~1+(1|Site),data=Pool)
summary(Agreeableness.Uncond)

Agreeableness.MEmodel<-lmer(Agreeableness~DaysInComp+(1 |Site),data=Pool)
summary(Agreeableness.MEmodel)

Agreeableness.MEmodel.null<-lmer(Agreeableness~1+(1|Site),data=Pool,REML=FALSE)
Agreeableness.MEmodel.test<-lmer(Agreeableness~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Agreeableness.MEmodel.null,Agreeableness.MEmodel.test)
coef(Agreeableness.MEmodel.test)


###Neuroticism
Neuroticism.Uncond<-lmer(Neuroticism~1+(1|Site),data=Pool)
summary(Neuroticism.Uncond)
0.02038/(0.02038+ 1.89776)

Neuroticism.MEmodel<-lmer(Neuroticism~DaysInComp+(1 |Site),data=Pool)
summary(Neuroticism.MEmodel)

Neuroticism.MEmodel.null<-lmer(Neuroticism~1+(1|Site),data=Pool,REML=FALSE)
Neuroticism.MEmodel.test<-lmer(Neuroticism~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Neuroticism.MEmodel.null,Neuroticism.MEmodel.test)
coef(Neuroticism.MEmodel.test)


#############INTRINSIC MOTIVATION###############
str(Pool$Intrinsic)

Intrinsic.Uncond<-lmer(Intrinsic~1+(1|Site),data=Pool)
summary(Intrinsic.Uncond)
0.001858/(0.001858+ 0.151406)

Intrinsic.MEmodel<-lmer(Intrinsic~DaysInComp+(1 |Site),data=Pool)
summary(Intrinsic.MEmodel)

Intrinsic.MEmodel.null<-lmer(Intrinsic~1+(1|Site),data=Pool,REML=FALSE)
Intrinsic.MEmodel.test<-lmer(Intrinsic~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Intrinsic.MEmodel.null,Intrinsic.MEmodel.test)
coef(Intrinsic.MEmodel.test)


#############MOOD###############
str(Pool$Mood)

Mood.Uncond<-lmer(Mood~1+(1|Site),data=Pool)
summary(Mood.Uncond)
0.01619/(0.01619+ 1.34865)

Mood.MEmodel<-lmer(Mood~DaysInComp+(1 |Site),data=Pool)
summary(Mood.MEmodel)

Mood.MEmodel.null<-lmer(Mood~1+(1|Site),data=Pool,REML=FALSE)
Mood.MEmodel.test<-lmer(Mood~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Mood.MEmodel.null,Mood.MEmodel.test)
coef(Mood.MEmodel.test)

cor(Pool$DaysInComp,Pool$Mood,use="complete.obs",method="pearson")
cor.test(Pool$DaysInComp,Pool$Mood,alternative="two.sided",method="pearson",conf.level=0.95)
res(-0.06521653,var.r=NULL,2636)

#############NEED FOR COGNITION###############
str(Pool$NFC)

NFC.Uncond<-lmer(NFC~1+(1|Site),data=Pool)
summary(NFC.Uncond)
0.003473/(0.003473+ 0.316075)

NFC.MEmodel<-lmer(NFC~DaysInComp+(1 |Site),data=Pool)
summary(NFC.MEmodel)

NFC.MEmodel.null<-lmer(NFC~1+(1|Site),data=Pool,REML=FALSE)
NFC.MEmodel.test<-lmer(NFC~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(NFC.MEmodel.null,NFC.MEmodel.test)
coef(NFC.MEmodel.test)


#############SELF-ESTEEM###############
#1(not very true of me)-7(very true of me) scale
#statement is: I have high self-esteem
str(Pool$SelfEsteem)

SelfEsteem.Uncond<-lmer(SelfEsteem~1+(1|Site),data=Pool)
summary(SelfEsteem.Uncond)
0.0095/(0.0095+ 2.4606)

SelfEsteem.MEmodel<-lmer(SelfEsteem~DaysInComp+(1 |Site),data=Pool)
summary(SelfEsteem.MEmodel)

SelfEsteem.MEmodel.null<-lmer(SelfEsteem~1+(1|Site),data=Pool,REML=FALSE)
SelfEsteem.MEmodel.test<-lmer(SelfEsteem~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(SelfEsteem.MEmodel.null,SelfEsteem.MEmodel.test)
coef(SelfEsteem.MEmodel.test)


#############STRESS###############
#Higher scores indicate more stress
str(Pool$Stress)

Stress.Uncond<-lmer(Stress~1+(1|Site),data=Pool)
summary(Stress.Uncond)
0.009408/(0.009408+ 0.495291)

Stress.MEmodel<-lmer(Stress~DaysInComp+(1 |Site),data=Pool)
summary(Stress.MEmodel)

Stress.MEmodel.null<-lmer(Stress~1+(1|Site),data=Pool,REML=FALSE)
Stress.MEmodel.test<-lmer(Stress~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(Stress.MEmodel.null,Stress.MEmodel.test)
coef(Stress.MEmodel.test)

cor(Pool$DaysInComp,Pool$Stress,use="complete.obs",method="pearson")
cor.test(Pool$DaysInComp,Pool$Stress,alternative="two.sided",method="pearson",conf.level=0.95)
res(0.07764112,var.r=NULL,2623)


######################DEMOGRAPHICS######################
###Gender

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)

Genderfactor.Uncond<-glmer(Genderfactor~1+(1|Site),data=Pool,family=binomial)
summary(Genderfactor.Uncond)
0.1278/(0.1278+(pi^2/3))

Genderfactor.MEmodel<-glmer(Genderfactor~DaysInComp+(1 |Site),data=Pool,family=binomial)
summary(Genderfactor.MEmodel)

Genderfactor.MEmodel.null<-glmer(Genderfactor~1+(1|Site),data=Pool,family=binomial)
Genderfactor.MEmodel.test<-glmer(Genderfactor~DaysInComp+(1|Site),data=Pool,family=binomial)
anova(Genderfactor.MEmodel.null,Genderfactor.MEmodel.test)
coef(Genderfactor.MEmodel.test)
summary(Genderfactor.MEmodel.test)
Gender.glm<-glm(Genderfactor~DaysInComp,data=Pool,family=binomial)
Anova(Gender.glm,type="II")
chies(36.686,2598)

###Age
str(Pool$age)
Pool$age<-as.numeric(Pool$age)

age.Uncond<-lmer(age~1+(1|Site),data=Pool)
summary(age.Uncond)
0.3653/(0.3653+ 13.4963)

age.MEmodel<-lmer(age~DaysInComp+(1 |Site),data=Pool)
summary(age.MEmodel)

age.MEmodel.null<-lmer(age~1+(1|Site),data=Pool,REML=FALSE)
age.MEmodel.test<-lmer(age~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(age.MEmodel.null,age.MEmodel.test)
coef(age.MEmodel.test)

###Year in School
str(Pool$year)
Pool$year<-as.integer(Pool$year)

year.Uncond<-lmer(year~1+(1|Site),data=Pool)
summary(year.Uncond)
0.1293/(0.1293+ 0.7796)

year.MEmodel<-lmer(year~DaysInComp+(1 |Site),data=Pool)
summary(year.MEmodel)

year.MEmodel.null<-lmer(year~1+(1|Site),data=Pool,REML=FALSE)
year.MEmodel.test<-lmer(year~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(year.MEmodel.null,year.MEmodel.test)
coef(year.MEmodel.test)

###Ethnicity
str(Pool$ethnicity)
Pool$ethnicity<-as.factor(Pool$ethnicity)
?glmer
ethnicity.Uncond<-glmer(ethnicity~1+(1|Site),data=Pool,family=gaussian)
summary(ethnicity.Uncond)
1.227/(1.227+ 68.981)

ethnicity.MEmodel<-glmer(ethnicity~DaysInComp+(1 |Site),data=Pool)
summary(ethnicity.MEmodel)

ethnicity.MEmodel.null<-glmer(ethnicity~1+(1|Site),data=Pool,REML=FALSE)
ethnicity.MEmodel.test<-glmer(ethnicity~DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(ethnicity.MEmodel.null,ethnicity.MEmodel.test)
coef(ethnicity.MEmodel.test)


