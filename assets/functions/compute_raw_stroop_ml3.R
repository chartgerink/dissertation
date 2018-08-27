# CHJH: This was copied from the ML3 script
# Made a few adjustments to ensure everything worked
# and gave the information needed

############Stroop##############

Stroop<-read.csv(file= ml3_dat_file,header=TRUE,stringsAsFactors=FALSE)
# head(Stroop)
# str(Stroop)

###Cleaning Stroop Data###

###Isolating trials from the Test Block (there's only 1 test block)
JustTest<-subset(Stroop,Stroop$task_name=='stroop')
# head(JustTest)
# list(JustTest$task_name)
# str(JustTest)

###Isolating the word in a given trial###
trialword<-JustTest$trial_name
trialword2<-sub('c.*','',trialword)
trialword2<-substr(trialword2,1,nchar(trialword2)-1)
# trialword2
trialword2<-tolower(trialword2)
JustTest$trial_word<-trialword2
# head(JustTest)

###coding colors as numbers (1=red,2=blue,3=green)
JustTest[JustTest$trial_word=="red",]$trial_word=1
# list(JustTest$trial_word)
JustTest[JustTest$trial_word=="blue",]$trial_word=2
JustTest[JustTest$trial_word=="green",]$trial_word=3
# list(JustTest$trial_word)

# JustTest[JustTest$block_pairing_definition=="red",]$block_pairing_definition=1
# list(JustTest$block_pairing_definition)
# JustTest[JustTest$block_pairing_definition=="blue",]$block_pairing_definition=2
# JustTest[JustTest$block_pairing_definition=="green",]$block_pairing_definition=3
# list(JustTest$block_pairing_definition)
###Now the color and meaning of each word are coded as numbers
# CHJH: were already coded as numbers in the clean set.

###making variable for congruent vs. incongruent trials
JustTest$congruent<-JustTest$block_pairing_definition==JustTest$trial_word
# str(JustTest)
JustTest[JustTest$congruent=="TRUE",]$congruent="Congruent"
JustTest[JustTest$congruent=="FALSE",]$congruent="Incongruent"
# list(JustTest$congruent)
# str(JustTest$congruent)
JustTest$congruent<-as.factor(JustTest$congruent)


####Cleaning Latency Data

###Cleaning Latency Data###
Latency<-JustTest
# head(Latency)
# str(Latency)


###Cleaning data with Dan Martin's dplyr IAT functions (adapted)
###Reading in data from fixing the bug

Raw <- JustTest
Latency <- JustTest
# head(Latency)

names(Latency)[names(Latency)=="session_id"]<-"SESSION_ID"
names(Latency)[names(Latency)=="trial_latency"]<-"TRIAL_LATENCY"
names(Latency)[names(Latency)=="trial_error"]<-"TRIAL_ERROR"
names(Latency)[names(Latency)=="congruent"]<-"CONGRUENT"

myTbl<-dplyr::group_by(dplyr::tbl_df(Latency),SESSION_ID)
myTbl$SUBEXCL<-0
myTblNoLong<-dplyr::filter(myTbl,TRIAL_LATENCY<10000 & TRIAL_LATENCY>=0)

myFastTbl<-dplyr::filter(myTbl) %>%
  dplyr::summarise(FASTM=sum(TRIAL_LATENCY<300)/length(TRIAL_LATENCY))

isTooFast<-dplyr::filter(myFastTbl,FASTM>.10)%>%
  dplyr::select(SESSION_ID)
# if(nrow(isTooFast)>0){
#   myTbl[myTbl$SESSION_ID %in% isTooFast, ]$SUBEXCL<-1
# } # CHJH: This seems like redundant code
# 
myTblNotFast<-dplyr::group_by(myTblNoLong,SESSION_ID,CONGRUENT)

###Replacing error trials with mean of trial type + 600ms
#NOTE: in TRIAL_ERROR, 0=error, 1=correct

meanReplace<-dplyr::filter(myTblNotFast,TRIAL_ERROR==1) %>%
  dplyr::summarise(blockMean=mean(TRIAL_LATENCY)+600)
# meanReplace
# head(meanReplace)
# str(meanReplace)

mergeTbl<-merge(myTblNotFast,meanReplace,by=c("SESSION_ID","CONGRUENT"),all=TRUE)
# head(mergeTbl)
# head(mergeTbl,n=50)
# if(mergeTbl$TRIAL_ERROR==0)mergeTbl$TRIAL_LATENCY=mergeTbl$blockMean
# CHJH: This is incorrect code

mergeTbl$TRIAL_LATENCY[mergeTbl$TRIAL_ERROR==0] <- mergeTbl$blockMean[mergeTbl$TRIAL_ERROR==0]
Correct<-subset(mergeTbl,mergeTbl$TRIAL_ERROR==1)
Incorrect<-subset(mergeTbl,mergeTbl$TRIAL_ERROR==0)
Incorrect$TRIAL_LATENCY<-Incorrect$blockMean
# head(Incorrect)
Corrected<-rbind(Correct,Incorrect)
# head(Corrected,n=50)

Congruent<-subset(Corrected,Corrected$CONGRUENT=="Congruent")
Incongruent<-subset(Corrected,Corrected$CONGRUENT=="Incongruent")
# head(Congruent)
# head(Incongruent)

blockMeans1<-dplyr::summarise(dplyr::group_by(Congruent,SESSION_ID),MC=mean(TRIAL_LATENCY),SDC=sd(TRIAL_LATENCY),NC=length(TRIAL_LATENCY))
tblM1<-blockMeans1

blockMeans2<-dplyr::summarise(dplyr::group_by(Incongruent,SESSION_ID),MI=mean(TRIAL_LATENCY),SDI=sd(TRIAL_LATENCY),NI=length(TRIAL_LATENCY))
tblM2<-blockMeans2

Means<-merge(tblM1,tblM2,by="SESSION_ID")
tmp <- data.frame(SESSION_ID = Latency$SESSION_ID,
                  study_name = Latency$study_name)
tmp <- tmp[!duplicated(tmp),]
Means <- merge(Means, tmp, by="SESSION_ID")
# head(Means)
# list(Means$NC)
# list(Means$NI)

# myTblNotFastCorrect<-dplyr::filter(Corrected, TRIAL_ERROR==1)
# SD<-summarise(dplyr::group_by(myTblNotFastCorrect,SESSION_ID),S=sd(TRIAL_LATENCY))

# Stroop<-merge(Means,SD,by="SESSION_ID")
Stroop <- Means
# head(Stroop)
Stroop$SDPooled <- sqrt(((Stroop$NC - 1) * Stroop$SDC^2 + (Stroop$NI - 1) * Stroop$SDI^2) / 
  ((Stroop$NC + Stroop$NI - 2)))
Stroop$StroopEffect <- (Stroop$MI - Stroop$MC) / Stroop$SDPooled
# mean(Stroop$StroopEffect)
# sd(Stroop$StroopEffect)
# Stroop
# head(Stroop)

write.csv(Stroop, '../data/study_02/ml3_stroop.csv', row.names = FALSE)
