###########################################################################
###########################################################################
############# SCRIPT TO PRODUCE Figure 2 and Figure A.3  ##################
###########################################################################
###########################################################################
###########################################################################

# Author: Junyan Jiang (Asst. Prof, Dept of Government and Public Administration, Chinese University of Hong Kong)
# Version: 1.0
# Tested under:
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32   





## Set the working directory to the folder that contains the replication files ##

setwd("C:/Users/junya/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")

#setwd("D:/Users/jiang/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")

Sys.setlocale(,"CHS")

## Install necessary packages ##
install.packages(c("plyr","reshape","reshape2","ggplot2","igraph","Cairo","data.table","lubridate","foreign","gridExtra","scales"))
source("https://bioconductor.org/biocLite.R")
biocLite("IRanges")



require(plyr)
require(reshape)
require(reshape2)
library(IRanges)
library(ggplot2)
library(igraph)
library(Cairo)
library(data.table)
library(lubridate)
library(foreign)
library(gridExtra)
library(scales)

facTOchar<-function(data,direction="f2c",exclude=NULL){
  if(direction=="f2c"){
    i<-sapply(data,is.factor)
    data[,i]<-lapply(data[,i],as.character)
    return(data)
  }
  if(direction=="c2f"){
    i<-sapply(data,is.character)
    data[,i]<-lapply(data[,i],function(x) factor(x,exclude=exclude))
    return(data)
  }
  
}




theme_set(theme_bw() + theme(text=element_text(family="Times"),legend.position="bottom",legend.direction="horizontal",axis.title.x=element_text(face="bold",vjust=-.5),axis.title.y=element_text(face="bold",vjust=1),title=element_text(face="bold",size=16,vjust=1.5),axis.text.x=element_text(size=12),strip.text=element_text(size=13)))


windowsFonts(Times=windowsFont("TT Times New Roman"))

##---- Functions---##

listdele<-function(x){
  a<-x[complete.cases(x),]
  return(a)
}


##---- Graphical parameters ----##

pd<-position_dodge(width=0.4)

cl<-c("seagreen","red","steelblue")



#################################################################################
########## Figure 2:  Dynamic Effects of Connection on Growth ###################
#################################################################################

do<-listdele(read.csv("para2_overall.csv",stringsAsFactors=F))
do$label<-c("In 4\n years",
            "In 3\n years" ,
            "In 2\n years",
            "In 1\n year",
            "For 1\n year",
            "For 2\n years",
            "For >2\n years",
            "1 year\n ago",
            "2 years\n ago",
            "3 years\n ago")

rect<-data.frame(xstart=c(0,4.5,7.5),xend=c(4.5,7.5,Inf),col=letters[1:3])
rect$x.text<-ifelse(is.infinite(rect$xend),11,rect$xend)
# gtext<-data.frame(txt=c("Will be Connected","Currently Connected","Used to be Connected"),x=(rect$xstart+rect$x.text)/2,y=2.5)
do$xax<-factor(do$label,levels=do$label)



##########  Manufacturing  ###################
dm<-listdele(read.csv("para2_2nd.csv",stringsAsFactors=F))
dm$label<-c("In 4\n years",
            "In 3\n years" ,
            "In 2\n years",
            "In 1\n year",
            "For 1\n year",
            "For 2\n years",
            "For >2\n years",
            "1 year\n ago",
            "2 years\n ago",
            "3 years\n ago")
dm$xax<-factor(dm$label,levels=dm$label)

gtext<-data.frame(txt=c("Will be Connected","Currently Connected","Used to be Connected"),x=(rect$xstart+rect$x.text)/2,y=4)


################  Combine Overall and Manufacturing ##############
dc<-rbind(dm,do)
dc$g<-factor(rep(c("Manufacturing","Overall"),each=10),level=c("Overall","Manufacturing"))

gtrend.2<-ggplot()+
  geom_rect(data=rect,aes(xmin=xstart,xmax=xend,ymin=-Inf,ymax=Inf,fill=col),alpha=.2)+
  geom_point(data=dc,aes(x=xax,y=beta,color=g,shape=g),size=2, position=pd)+
  geom_errorbar(data=dc,aes(x=xax,ymin=lb95,ymax=ub95,y=beta,color=g),width=.1,position=pd,size=.4)+
  geom_hline(yintercept=0,linetype="dashed",size=.1)+
  scale_x_discrete(labels=do$label)+
  scale_fill_manual(values=c("grey60","white","grey60"))+
  scale_color_manual("",values=cl[-1])+
  scale_shape_discrete("")+
  xlab("")+ylab("Effect of Connection")+
  geom_text(data=gtext,aes(label=txt,x=x,y=y),size=5)+
  theme(legend.text=element_text(size=14))+
  guides(fill=F)







#########################################################################################
############ Figure A.3: Over-time Distribution of the Connection Indicator #############
#########################################################################################

dt<-read.dta("distribution.dta",convert.underscore=T)
# the distribution data is produced by applying the following code in the "citypanel_base.dta":
# collapse (count) cityid, by(msec2currentsec mayor2currentsec year) 




dt$lab<-"None"
dt$lab[which(dt$msec2currentsec==1 & dt$mayor2currentsec==0)]<-"Msec only"
dt$lab[which(dt$msec2currentsec==0 & dt$mayor2currentsec==1)]<-"Mayor only"
dt$lab[which(dt$msec2currentsec==1 & dt$mayor2currentsec==1)]<-"Both"
dt$lab<-factor(dt$lab,levels=c("Msec only","Both","Mayor only","None"))
dt<-dt[order(dt$lab),]
mix<-rgb(green=0, red=1, blue=1)

mix<-rgb(green=0, red=255, blue=255,maxColorValue = 255)


mix<-col2rgb("darkblue")+col2rgb("darkred")
c.mix<-rgb(green=0,red=139,blue=139,maxColorValue = 255)




gshare<-ggplot(dt[which(dt$year>=2000 & dt$year<=2011),],aes(x=year,y=cityid, fill=lab))+geom_bar(stat="identity",position="fill",color="black",size=.7)+
  scale_fill_manual("",breaks=c("Msec only","Both","Mayor only","None"),values=rev(c("grey60","blue",c.mix,"red")),labels=c("Connected secretary only","Both","Connected mayor only","Unconnected"))+xlab("")+ylab("")+
  guides(fill=guide_legend(nrow=2))+
  scale_y_continuous(labels=percent_format())+
  scale_x_continuous(breaks=seq(2000,2011,1))+theme(legend.text=element_text(size=12))







