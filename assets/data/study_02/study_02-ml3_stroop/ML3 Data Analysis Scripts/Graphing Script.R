############ML3 Aggregate Graphs#############
#######Charlie Ebersole######


require(ggplot2)
require(reshape2)

###Effects with Cohen's d

setwd("/Users/Charlie/Desktop/ML3 Graph Data")
dstats<-read.csv(file="dplot.csv",header=TRUE)
dstats

dflip<-t(dstats)
dflip
write.csv(dflip,file="dflip.csv",row.names=FALSE)

flipped<-read.csv(file="dflip.csv",header=TRUE)
flipped

dlong<-melt(flipped,id.vars="Effect")
head(dlong)
write.csv(dlong,file="dlong.csv",row.names=FALSE)

dlong<-read.csv(file="dlong.csv",header=TRUE)
str(dlong$Effect)
list(levels(dlong$Effect))
dlong$Effect<-factor(dlong$Effect,levels=c("Warmth Perceptions","Weight Embodiment","Power and Perspective","Persistence","Availability Heuristic","Metaphoric Restructuring","Stroop Effect"))

Aggregate<-subset(dlong,SiteorAgg=="Aggregate")
Sites<-subset(dlong,SiteorAgg=="Single Site")
Original<-subset(dlong,SiteorAgg=="Original")
dlong$SiteorAgg<-factor(dlong$SiteorAgg,levels=c("Single Site","Aggregate","Original"))

d<-ggplot(Aggregate,aes(x=Effect,y=value,ymin=lwr,ymax=upr))+geom_point(data=Sites,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=4,size=4)+geom_point(data=Aggregate,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=16,size=6,color="blue",fill="blue")+geom_point(data=Original,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=24,fill="green",size=4)+geom_errorbar(colour='Blue',size=1,width=.35)+coord_flip()+ylab("Cohen's d")+theme_bw()+theme(panel.grid.major=element_blank())+geom_hline(yintercept=0,color="black")+theme(text=element_text(size=15))
d


#Effects with partial eta squared

etastats<-read.csv(file="etaplot.csv",header=TRUE)
etastats

etaflip<-t(etastats)
etaflip
write.csv(etaflip,file="etaflip.csv",row.names=FALSE)

flipped<-read.csv(file="etaflip.csv",header=TRUE)
flipped

etalong<-melt(flipped,id.vars="Effect")
head(etalong)
write.csv(etalong,file="etalong.csv",row.names=FALSE)

etalong<-read.csv(file="etalong.csv",header=TRUE)
str(etalong$Effect)
list(levels(etalong$Effect))
etalong$Effect<-factor(etalong$Effect,levels=c("Credentials and Prejudice (main effect)","Subjective Distance (main effect)","Elaboration Likelihood (main effect)","Credentials and Prejudice (interaction)","Subjective Distance (interaction)","Elaboration Likelihood (interaction)"))

Aggregate<-subset(etalong,SiteorAgg=="Aggregate")
Sites<-subset(etalong,SiteorAgg=="Single Site")
Original<-subset(etalong,SiteorAgg=="Original")
etalong$SiteorAgg<-factor(etalong$SiteorAgg,levels=c("Single Site","Aggregate"))

e<-ggplot(Aggregate,aes(x=Effect,y=value,ymin=lwr,ymax=upr))+geom_point(data=Sites,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=4)+geom_pointrange(colour='Blue',size=1.2)+coord_flip()+ylab("ηp²")+theme_bw()+theme(panel.grid.major=element_blank())+geom_hline(yintercept=0,color="black")+theme(text=element_text(size=15))
e


e<-ggplot(Aggregate,aes(x=Effect,y=value,ymin=lwr,ymax=upr))+geom_point(data=Sites,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=4,size=4)+geom_point(data=Aggregate,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=16,size=6,color="blue",fill="blue")+geom_point(data=Original,aes(x=Effect,y=value,ymin=lwr,ymax=upr),shape=24,fill="green",size=4)+geom_errorbar(colour='Blue',size=1,width=.35)+coord_flip()+ylab("ηp²")+theme_bw()+theme(panel.grid.major=element_blank())+geom_hline(yintercept=0,color="black")+theme(text=element_text(size=15))+ylim(0,.2)
e
