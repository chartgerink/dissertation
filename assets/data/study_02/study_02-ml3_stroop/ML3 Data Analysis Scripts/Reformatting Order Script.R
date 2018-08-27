########ML3 Reformatting Order Script########
###March 4 2015###

setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
ML3Order<-read.csv(file="ML3Order.csv",header=TRUE)
head(ML3Order)

BestGrade<-subset(ML3Order,bestgrade_order!="NA")
WorstGrade<-subset(ML3Order,worstgrade_order!="NA")
BestGrade$grade_order<-BestGrade$bestgrade_order
WorstGrade$grade_order<-WorstGrade$worstgrade_order
Grades<-rbind(BestGrade,WorstGrade)
head(Grades)
GradeOrder<-Grades[,c("session_id","grade_order")]
ML3Order<-merge(ML3Order,GradeOrder,by="session_id",all=TRUE)
head(ML3Order)

JustStart<-ML3Order[,c("session_id",
"elmques_order",
"galinskyvignette_order",
"moninvignette_order",
"grade_order",
"stroop_order",
"anagrams_order",
"availinstruct_order",
"tempfollowup_order",
"inlab_order")]

head(JustStart)

setwd("/Users/Charlie/Desktop/ML3 Final Data")
ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE)
head(ML3)
str(ML3$OrderofTasks)


ML3IdLab<-ML3[,c("session_id","OrderofTasks")]
head(ML3IdLab)


ML3OrderNew<-merge(JustStart,ML3IdLab,by="session_id",all=TRUE)
head(ML3OrderNew)
tail(ML3OrderNew)

IIFirst<-subset(ML3OrderNew,OrderofTasks=="II-SR")
SRFirst<-subset(ML3OrderNew,OrderofTasks=="SR-II")
head(IIFirst)
head(SRFirst)
list(IIFirst$OrderofTasks)
list(SRFirst$OrderofTasks)

IIFirst$II_order<-IIFirst$inlab_order+.2
IIFirst$SR_order<-IIFirst$inlab_order+.5
head(IIFirst)

SRFirst$II_order<-SRFirst$inlab_order+.5
SRFirst$SR_order<-SRFirst$inlab_order+.2
head(SRFirst)

InLab<-rbind(SRFirst,IIFirst)
InLab2<-InLab[,c("session_id","II_order","SR_order")]

setwd("/Users/Charlie/Desktop/ML3 Data Analysis")
ML3OrderNew<-merge(ML3OrderNew,InLab2,by="session_id",all=TRUE)
head(ML3OrderNew)
ML3OrderNew$inlab_order<-NULL
ML3OrderNew$OrderofTasks<-NULL

require(reshape2)
ML3OrderNewLong<-melt(ML3OrderNew,id.vars="session_id")
head(ML3OrderNewLong)

write.csv(ML3OrderNewLong,file="ML3OrderNew.csv",row.names=FALSE)

Order<-read.csv(file="ML3OrderNew.csv",header=TRUE)
head(Order)
for(i in unique(Order$session_id)){
		Order$Ob[Order$session_id==i]=1:sum(Order$session_id==i)
}
head(Order)
range(Order$Ob)
hist(Order$Ob)

Order$value<-NULL
head(Order)

OrderWide<-dcast(Order,session_id~variable,value.var="Ob")
head(OrderWide)

write.csv(OrderWide,file="NewOrderWide.csv",row.names=FALSE)







