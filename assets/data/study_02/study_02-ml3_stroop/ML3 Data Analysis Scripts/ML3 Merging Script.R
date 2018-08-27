###############ML3 MASTER MERGING SCRIPT###################
###January 21, 2015- Charlie Ebersole###

setwd("/Users/Charlie/Desktop/ML3 Merged Data Sets")

###Reading in Each Site's Data###

Davis<-read.csv(file="UC Davis merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Davis$ClipBoardMaterial<-"Metal"
list(Davis$ClipBoardMaterial)

Riverside<-read.csv(file="UC Riverside merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Riverside$ClipBoardMaterial<-"Plastic"

Florida<-read.csv(file="Florida merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Florida$ClipBoardMaterial<-"Plastic"

Mississippi<-read.csv(file="Southern Mississippi merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Mississippi$ClipBoardMaterial<-"Plastic"

Toronto<-read.csv(file="Toronto merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Toronto$ClipBoardMaterial<-"Plastic"
list(Toronto$tempest1)
Toronto$tempest1<-as.numeric(Toronto$tempest1)
Toronto$tempest1<-Toronto$tempest1*(9/5)+32
Toronto$Temperatureinlab<-as.numeric(Toronto$Temperatureinlab)
Toronto$Temperatureinlab<-Toronto$Temperatureinlab*(9/5)+32

VCU<-read.csv(file="Virginia Commonwealth merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
VCU$ClipBoardMaterial<-"Plastic"

Nova<-read.csv(file="Nova merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Nova$ClipBoardMaterial<-"Plastic"

OSU<-read.csv(file="OSU merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
OSU$ClipBoardMaterial<-"Plastic"

Penn<-read.csv(file="Penn merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Penn$ClipBoardMaterial<-"Plastic"

PLU<-read.csv(file="PLU merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
PLU$ClipBoardMaterial<-"Plastic"

SDSU<-read.csv(file="SDSU merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
SDSU$ClipBoardMaterial<-"Plastic"

Texas<-read.csv(file="Texas merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Texas$ClipBoardMaterial<-"Plastic"

Ashland<-read.csv(file="Ashland merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Ashland$ClipBoardMaterial<-"Plastic"

Carleton<-read.csv(file="Carleton merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Carleton$ClipBoardMaterial<-"Plastic"
list(Carleton$tempest1)
Carleton$tempest1<-as.numeric(Carleton$tempest1)
Carleton$tempest1<-Carleton$tempest1*(9/5)+32
Carleton$Temperatureinlab<-as.numeric(Carleton$Temperatureinlab)
Carleton$Temperatureinlab<-Carleton$Temperatureinlab*(9/5)+32

Bradley<-read.csv(file="Bradley merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Bradley$ClipBoardMaterial<-"Plastic"

MichSt<-read.csv(file="MichiganState merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
MichSt$ClipBoardMaterial<-"Plastic"

Ithaca<-read.csv(file="Ithaca merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Ithaca$ClipBoardMaterial<-"Plastic"

Miami<-read.csv(file="Miami merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Miami$ClipBoardMaterial<-"Metal"

Montana<-read.csv(file="Montana merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Montana$ClipBoardMaterial<-"Plastic"

Virginia<-read.csv(file="Virginia merged.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
Virginia$ClipBoardMaterial<-"Metal"

#not included in master sheet of colelction sites
mTurk<-read.csv(file="mTurk.csv",header=TRUE,na.strings=c("."," "),stringsAsFactors=FALSE)
mTurk$ClipBoardMaterial<-"None"

###Merging Site Data into Total Data###

ML3<-rbind(Davis,Riverside)
ML3<-rbind(ML3,Florida)
ML3<-rbind(ML3,Mississippi)
ML3<-rbind(ML3,Toronto)
ML3<-rbind(ML3,VCU)
ML3<-rbind(ML3,Nova)
ML3<-rbind(ML3,OSU)
ML3<-rbind(ML3,Penn)
ML3<-rbind(ML3,PLU)
ML3<-rbind(ML3,SDSU)
ML3<-rbind(ML3,Texas)
ML3<-rbind(ML3,Ashland)
ML3<-rbind(ML3,Carleton)
ML3<-rbind(ML3,Bradley)
ML3<-rbind(ML3,MichSt)
ML3<-rbind(ML3,Ithaca)
ML3<-rbind(ML3,Miami)
ML3<-rbind(ML3,Montana)
ML3<-rbind(ML3,Virginia)
ML3<-rbind(ML3,mTurk)
head(ML3)

length(unique(ML3$session_id))
test<-ML3$session_id[duplicated(ML3$session_id)]
test


###Adding Persistence Time###
setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
PersistenceData<-read.csv(file="ML3PersistenceClean.csv",header=TRUE)
ML3<-merge(ML3,PersistenceData,by="session_id",all.x=TRUE)
head(ML3)
tail(ML3)


###Adding Task Order###
ML3Order<-read.csv(file="ML3Order.csv",header=TRUE)
head(ML3Order)
ML3<-merge(ML3,ML3Order,by="session_id",all.x=TRUE)
head(ML3)


###Adding Computer Portion Date###
ML3CompDates<-read.csv(file="ML3 Computer Dates.csv",header=TRUE)
head(ML3CompDates)
ML3<-merge(ML3,ML3CompDates,by="session_id",all.x=TRUE)
head(ML3)

###Calculating In-Lab Dates###
head(ML3)
str(ML3$Date.x)
require(reshape)

#assigning everyone a number
ML3$RowNumber<-1:length(ML3$session_id)
list(ML3$RowNumber)
length(ML3$session_id)
length(unique(ML3$session_id))
head(ML3)
tail(ML3)

LabDate<-ML3[,c("RowNumber","Date.x")]
head(LabDate)
LabSplit<-colsplit(LabDate$Date,split="/",c("MonthLab","DayLab","YearLab"))
head(LabSplit)
tail(LabSplit)
LabAll<-cbind(LabDate,LabSplit)
head(LabAll)
tail(LabAll)


###Calculating number of days since August 1
LabAll$DaysSinceMonthLab[LabAll$MonthLab=="8"]<-0
LabAll$DaysSinceMonthLab[LabAll$MonthLab=="9"]<-31
LabAll$DaysSinceMonthLab[LabAll$MonthLab=="10"]<-61
LabAll$DaysSinceMonthLab[LabAll$MonthLab=="11"]<-92
LabAll$DaysSinceMonthLab[LabAll$MonthLab=="12"]<-122

#Adding Days
LabAll$DaysSinceAugLab<-LabAll$DayLab+LabAll$DaysSinceMonthLab
head(LabAll)
tail(LabAll)

#Merging back in
ML3<-merge(ML3,LabAll,by="RowNumber")
head(ML3)
tail(ML3)

#####Calculating Starting Date###

Start<-ML3[,c("RowNumber","StartDate")]
StartSplit<-colsplit(Start$StartDate,split="/",c("MonthStart","DayStart","YearStart"))
head(StartSplit)
tail(StartSplit)
StartAll<-cbind(Start,StartSplit)
head(StartAll)
tail(StartAll)


###Calculating number of days since August 1
StartAll$DaysSinceMonthStart[StartAll$MonthStart=="8"]<-0
StartAll$DaysSinceMonthStart[StartAll$MonthStart=="9"]<-31
StartAll$DaysSinceMonthStart[StartAll$MonthStart=="10"]<-61
StartAll$DaysSinceMonthStart[StartAll$MonthStart=="11"]<-92
StartAll$DaysSinceMonthStart[StartAll$MonthStart=="12"]<-122

#Adding Days
StartAll$DaysSinceAugStart<-StartAll$DayStart+StartAll$DaysSinceMonthStart
head(StartAll)
tail(StartAll)

#Merging back in
ML3<-merge(ML3,StartAll,by="RowNumber")
head(ML3)
tail(ML3)

###Calculating Time into the Term###
ML3$DaysInComp<-(ML3$DaysSinceAug-ML3$DaysSinceAugStart)/ML3$NumberofDays
head(ML3)
tail(ML3)
list(ML3$DaysInComp)

ML3$DaysInLab<-(ML3$DaysSinceAugLab-ML3$DaysSinceAugStart)/ML3$NumberofDays
head(ML3)
tail(ML3)
list(ML3$DaysInLab)

#Correcting Date Errors
range(ML3$DaysInLab,na.rm=TRUE)
mean(ML3$DaysInLab,na.rm=TRUE)
ML3$DaysInLab[ML3$DaysInLab>1]<-1
hist(ML3$DaysInLab)

range(ML3$DaysInComp,na.rm=TRUE)
mean(ML3$DaysInComp,na.rm=TRUE)
ML3$DaysInComp[ML3$DaysInComp>1]<-1
ML3$DaysInComp[ML3$DaysInComp<0]<-0
hist(ML3$DaysInComp)

#########################################################################
####Calculating variables and assigning conditions for certain effects and measures####
#########################################################################

##########################Individual Differences##########################

head(ML3)

###Attention###
list(ML3$attention)
ML3$AttentionCheck[ML3$attentioncorrect!="NA"]<-"Pass"
ML3$AttentionCheck[ML3$attention!="NA"]<-"Fail"
list(ML3$AttentionCheck)

#############BIG FIVE###############
ML3$big5_01<-as.integer(ML3$big5_01)
ML3$big5_02<-as.integer(ML3$big5_02)
ML3$big5_03<-as.integer(ML3$big5_03)
ML3$big5_04<-as.integer(ML3$big5_04)
ML3$big5_05<-as.integer(ML3$big5_05)
ML3$big5_06<-as.integer(ML3$big5_06)
ML3$big5_07<-as.integer(ML3$big5_07)
ML3$big5_08<-as.integer(ML3$big5_08)
ML3$big5_09<-as.integer(ML3$big5_09)
ML3$big5_10<-as.integer(ML3$big5_10)
list(ML3$big5_01)

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
ML3$intrinsic_01<-as.integer(ML3$intrinsic_01)
ML3$intrinsic_02<-as.integer(ML3$intrinsic_02)
ML3$intrinsic_03<-as.integer(ML3$intrinsic_03)
ML3$intrinsic_04<-as.integer(ML3$intrinsic_04)
ML3$intrinsic_05<-as.integer(ML3$intrinsic_05)
ML3$intrinsic_06<-as.integer(ML3$intrinsic_06)
ML3$intrinsic_07<-as.integer(ML3$intrinsic_07)
ML3$intrinsic_08<-as.integer(ML3$intrinsic_08)
ML3$intrinsic_09<-as.integer(ML3$intrinsic_09)
ML3$intrinsic_10<-as.integer(ML3$intrinsic_10)
ML3$intrinsic_11<-as.integer(ML3$intrinsic_11)
ML3$intrinsic_12<-as.integer(ML3$intrinsic_12)
ML3$intrinsic_13<-as.integer(ML3$intrinsic_13)
ML3$intrinsic_14<-as.integer(ML3$intrinsic_14)
ML3$intrinsic_15<-as.integer(ML3$intrinsic_15)
list(ML3$intrinsic_01)

#All questions on 1(never or almost never true of me)-4(Always or almost always true of me) scale
ML3$Intrinsic<-(ML3$intrinsic_01+ML3$intrinsic_02+ML3$intrinsic_03+ML3$intrinsic_04+ML3$intrinsic_05+(5-ML3$intrinsic_06)+(5-ML3$intrinsic_07)+ML3$intrinsic_08+ML3$intrinsic_09+ML3$intrinsic_10+ML3$intrinsic_11+ML3$intrinsic_12+ML3$intrinsic_13+ML3$intrinsic_14+ML3$intrinsic_15)/15
summary(ML3$Intrinsic)
sd(ML3$Intrinsic,na.rm=TRUE)
hist(ML3$Intrinsic)

#############MOOD###############
ML3$mood_01<-as.integer(ML3$mood_01)
ML3$mood_02<-as.integer(ML3$mood_02)

ML3$Mood<-((8-ML3$mood_01)+(8-ML3$mood_02))/2
summary(ML3$Mood)
sd(ML3$Mood,na.rm=TRUE)
hist(ML3$Mood)

#############NEED FOR COGNITION###############
ML3$nfc_01<-as.integer(ML3$nfc_01)
ML3$nfc_02<-as.integer(ML3$nfc_02)
ML3$nfc_03<-as.integer(ML3$nfc_03)
ML3$nfc_04<-as.integer(ML3$nfc_04)
ML3$nfc_05<-as.integer(ML3$nfc_05)
ML3$nfc_06<-as.integer(ML3$nfc_06)
list(ML3$nfc_01)

ML3$NFC<-(ML3$nfc_01+(6-ML3$nfc_02)+(6-ML3$nfc_03)+ML3$nfc_04+ML3$nfc_05+(6-ML3$nfc_06))/6
summary(ML3$NFC)
sd(ML3$NFC,na.rm=TRUE)
hist(ML3$NFC)

#############REPORTED EFFORT###############
ML3$pate_01<-as.integer(ML3$pate_01)

#1(no effort)-5(tried my hardest) scale
ML3$ReportedEffort<-ML3$pate_01
summary(ML3$ReportedEffort)
sd(ML3$ReportedEffort,na.rm=TRUE)
hist(ML3$ReportedEffort)

#############REPORTED ATTENTION###############
ML3$pate_02<-as.integer(ML3$pate_02)

#1(none)-5(I have my undivided attention) scale
ML3$ReportedAttention<-ML3$pate_02
summary(ML3$ReportedAttention)
sd(ML3$ReportedAttention,na.rm=TRUE)
hist(ML3$ReportedAttention)

#############SELF-ESTEEM###############
ML3$selfesteem_01<-as.integer(ML3$selfesteem_01)

#1(not very true of me)-7(very true of me) scale
#statement is: I have high self-esteem
ML3$SelfEsteem<-ML3$selfesteem_01
summary(ML3$SelfEsteem)
sd(ML3$SelfEsteem,na.rm=TRUE)
hist(ML3$SelfEsteem)

#############STRESS###############
ML3$stress_01<-as.integer(ML3$stress_01)
ML3$stress_02<-as.integer(ML3$stress_02)
ML3$stress_03<-as.integer(ML3$stress_03)
ML3$stress_04<-as.integer(ML3$stress_04)

#Higher scores indicate more stress
ML3$Stress<-(ML3$stress_01+(6-ML3$stress_02)+(6-ML3$stress_03)+ML3$stress_04)/4
summary(ML3$Stress)
sd(ML3$Stress,na.rm=TRUE)
hist(ML3$Stress)

########################COMPUTER BASED EFFECTS###########################

############Power and Perspective Taking##############

str(ML3$sarcasm)

##Labeling the conditions##
ML3$PowerCond[ML3$lowpower!="NA"]<-"LowPower"
ML3$PowerCond[ML3$highpower!="NA"]<-"HighPower"
list(ML3$PowerCond)
str(ML3$PowerCond)
ML3$PowerCond<-as.factor(ML3$PowerCond)


############Moral Credentialing##############

##Looking at number of statements endorsed
str(ML3$mcmost1)
ML3$mcmost1<-as.integer(ML3$mcmost1)
ML3$mcmost2<-as.integer(ML3$mcmost2)
ML3$mcmost3<-as.integer(ML3$mcmost3)
ML3$mcmost4<-as.integer(ML3$mcmost4)
ML3$mcmost5<-as.integer(ML3$mcmost5)
ML3$mcsome1<-as.integer(ML3$mcsome1)
ML3$mcsome2<-as.integer(ML3$mcsome2)
ML3$mcsome3<-as.integer(ML3$mcsome3)
ML3$mcsome4<-as.integer(ML3$mcsome4)
ML3$mcsome5<-as.integer(ML3$mcsome5)

ML3$MostEndorse<-(ML3$mcmost1+ML3$mcmost2+ML3$mcmost3+ML3$mcmost4+ML3$mcmost5)-5
list(ML3$MostEndorse)
##For this variable, a score of 5 means that a participant rejected every statement

ML3$SomeEndorse<-(ML3$mcsome1+ML3$mcsome2+ML3$mcsome3+ML3$mcsome4+ML3$mcsome5)-5
list(ML3$SomeEndorse)

##Labeling the conditions##
ML3$CredCond[ML3$MostEndorse!="NA"]<-"Credentials"
ML3$CredCond[ML3$SomeEndorse!="NA"]<-"NoCredentials"
list(ML3$CredCond)
str(ML3$CredCond)
ML3$CredCond<-as.factor(ML3$CredCond)

ML3$Genderfactor[ML3$gender==1]<-"Female"
ML3$Genderfactor[ML3$gender==2]<-"Male"
list(ML3$Genderfactor)

############Self-Esteem and Subjective Distance##############

str(ML3$bestgrade2)
str(ML3$worstgrade2)

ML3$SubDistCond[ML3$bestgrade2!="NA"]<-"BestGrade"
ML3$SubDistCond[ML3$worstgrade2!="NA"]<-"WorstGrade"
list(ML3$SubDistCond)

#################Availability Heuristic######################

head(ML3)

###Assigning Sign 
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
list(ML3$AvailFirst)
range(ML3$AvailFirst,na.rm=TRUE)
ML3$AvailSign[ML3$AvailFirst==0]<-"-"
ML3$AvailSign[ML3$AvailFirst==1]<-"-"
ML3$AvailSign[ML3$AvailFirst==2]<-"-"
ML3$AvailSign[ML3$AvailFirst==3]<-"+"
ML3$AvailSign[ML3$AvailFirst==4]<-"+"
ML3$AvailSign[ML3$AvailFirst==5]<-"+"
list(ML3$AvailSign)

############Warmer Hearts and Rooms##############

head(ML3)
###tempest1 is temperature estimate
#C schools converted to F above

###Assigning Conditions###
ML3$TempCond4[ML3$tempcomm_order!="NA"]<-"CommunalMale"
ML3$TempCond4[ML3$tempcomf_order!="NA"]<-"CommunalFemale"
ML3$TempCond4[ML3$tempagenm_order!="NA"]<-"AgenticMale"
ML3$TempCond4[ML3$tempagenf_order!="NA"]<-"AgenticFemale"
list(ML3$TempCond4)

ML3$TempCond[ML3$TempCond4=="CommunalMale"]<-"Communal"
ML3$TempCond[ML3$TempCond4=="CommunalFemale"]<-"Communal"
ML3$TempCond[ML3$TempCond4=="AgenticMale"]<-"Agentic"
ML3$TempCond[ML3$TempCond4=="AgenticFemale"]<-"Agentic"
head(ML3)
list(ML3$TempCond)

ML3$TargetGender[ML3$TempCond4=="CommunalMale"]<-"MaleTarget"
ML3$TargetGender[ML3$TempCond4=="CommunalFemale"]<-"FemaleTarget"
ML3$TargetGender[ML3$TempCond4=="AgenticMale"]<-"MaleTarget"
ML3$TargetGender[ML3$TempCond4=="AgenticFemale"]<-"FemaleTarget"
list(ML3$TargetGender)

############Elaboration Likelihood Model##############
ML3$elm_01<-as.integer(ML3$elm_01)
ML3$elm_02<-as.integer(ML3$elm_02)
ML3$elm_03<-as.integer(ML3$elm_03)
ML3$elm_04<-as.integer(ML3$elm_04)
ML3$elm_05<-as.integer(ML3$elm_05)

#creating index of argument quality (no reverse coding needed)
ML3$ArgumentQuality=(ML3$elm_01+ML3$elm_02+ML3$elm_03+ML3$elm_04+ML3$elm_05)/5

#Centering Need for Cognition 
ML3$NFCcenter<-ML3$NFC-mean(ML3$NFC,na.rm=TRUE)

#Labeling conditions
head(ML3)
ML3$ELMCond[ML3$elmstrong_order!="NA"]<-"1"
ML3$ELMCond[ML3$elmweak_order!="NA"]<-"-1"
list(ML3$ELMCond)

############Conscientiousness and Persistence##############

#done above

##########################IN-LAB EFFECTS#################################

############Metaphoric Restructuring##############

#all cleaning done in analyses

############Weight as Embodied Importance##############

#ClipBoardMaterial Coded Above
ClipBoardReject<-read.csv(file="Clipboard Rejections.csv",header=TRUE)
head(ClipBoardReject)
ML3<-merge(ML3,ClipBoardReject,by=c("Site","Participant_ID"),all.x=TRUE)
head(ML3)
tail(ML3)

##########################final check for duplicates####################
length(unique(ML3$session_id))
test<-ML3$session_id[duplicated(ML3$session_id)]
test
list(ML3$Site)

#Total Session IDs
length(unique(ML3$session_id))-2-32
length(unlist(ML3$debrief_order==36))

###########################Collecting Numbers of Participants#################
Pool<-subset(ML3,ML3$Site!="mTurk")
length(Pool$session_id)
length(unique(Pool$session_id))-34
test2<-Pool$session_id[duplicated(Pool$session_id)]
test2

DebriefPool<-subset(Pool,Pool$debrief_order!="NA")
length(DebriefPool$session_id)
list(DebriefPool$debrief_order)

mturk2<-subset(ML3,ML3$Site=="mTurk")
head(mturk2)
length(mturk2$consent_order)

DebriefMturk<-subset(mturk2,mturk2$debrief_order!="NA")
length(DebriefMturk$debrief_order)
list(DebriefMturk$debrief_order)

PoolInlab<-subset(Pool,Pool$Participant_ID!="NA")
PoolInlab$Participant_ID<-as.numeric(PoolInlab$Participant_ID)
PoolInlab2<-subset(PoolInlab,PoolInlab$Participant_ID!="NA")
length(PoolInlab2$Participant_ID)
list(PoolInlab2$Participant_ID)

############################Printing Data Set############################
setwd("/Users/Charlie/Desktop/ML3 Final Data")
write.csv(ML3,file="ML3AllSitesandmTurk.csv",row.names=FALSE)