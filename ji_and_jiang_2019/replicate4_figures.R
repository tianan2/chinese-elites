# Ji and Jiang "Enlightened One-Party Rule? Ideological Differences between Chinese Communist Party Members and the Mass Public"
# Replication File -- Figures 2019.4.19

# Required packages
# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("readstata13")
# install.packages("ggridges")
# install.packages("gridExtra")

######################################################################################
# Figure 1 Sample Deviation from Population Statistics: Before and After Reweighting #
######################################################################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)
library(tidyr)

data<-read.xlsx("ji_and_jiang_2019/Fig.xlsx",sheet=1) %>% View()

datatidyr <- data %>% gather(attribute, value, CCP:age)
datatidyr$att <- factor(datatidyr$attribute, levels = c("CCP", "female", "edu", "age"),labels=c("% CCP membership","% of female CCP member","% of college educated CCP member","% of CCP member at age<=35"))
datatidyr$survey1 <- factor(datatidyr$survey, levels = c("ABS4(2015)", "ABS3(2011)", "CGSS2015", "CGSS2013","CGSS2012","CGSS2010","CFPS2014"))

pdf("Figure1.pdf",width=10, height=4)
ggplot(datatidyr,aes(x=value, y=att,group=as.factor(group),
                     shape=as.factor(group),color=as.factor(group)))+
  geom_point(size = 2)+
  facet_grid(. ~ survey1)+
  scale_y_discrete(limits=c("% of CCP member at age<=35","% of college educated CCP member",
                            "% of female CCP member","% CCP membership"))+
  geom_vline(aes(xintercept=0), colour="#62BCE9", linetype="dashed")+
  scale_color_brewer(palette="Set1",
                     name = "",
                     breaks=c("Origin", "Raked"),
                     labels=c("Before weight calibration","After weight calibration"))+
  scale_shape_manual(name = "",
                     breaks=c("Origin", "Raked"),values=c(19,17),
                     labels=c("Before weight calibration","After weight calibration"))+
  xlab("% in Survey - % in Population")+ylab("")+
  scale_x_continuous(limits=c(-0.22,0.1),labels = scales::percent_format(accuracy = 1))+
  theme_bw() + 
  theme(axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=11),
        axis.title.x=element_text(face="bold",vjust=-.5,size=11),
        axis.title.y=element_text(face="bold",vjust=1),
        legend.text = element_text(size=11),
        strip.text=element_text(size=13),
        text=element_text(family="Times"),
        legend.position="bottom",
        legend.direction="horizontal")
dev.off() 

#######################################################
# Figure 2: Main Result: Weighted Difference in Means #
#######################################################

rm(list=ls(all=TRUE))

library(readstata13)
library(ggplot2)
library(ggridges)
library(tidyr)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")

data<-read.dta13("data.dta")

datatidyr <- data %>% gather(attribute, value, social_value:modern_all_value)
datatidyr$values <- factor(datatidyr$attribute, levels = c("social_value", "political_value", "intl_value", "modern_all_value"),labels=c("Social","Political","International","Overall modern value"))

dg_cfps    <-data.frame(values=unique(datatidyr$values),labels1=c("Public:0.093\n[0.082,0.104]","","",""),labels2=c("CCP:0.410\n[0.373,0.447]","","",""))
dg_cgss2010<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.090\n[-0.109,-0.071]","","",""),labels2=c("CCP:0.337\n[0.287,0.386]","","",""))
dg_cgss2012<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.050\n[-0.069,-0.031]","","",""),labels2=c("CCP:0.412\n[0.360,0.463]","","",""))
dg_cgss2013<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.031\n[-0.050,-0.012]","","",""),labels2=c("CCP:0.301\n[0.243,0.360]","","",""))
dg_cgss2015<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.054\n[-0.074,-0.035]","","",""),labels2=c("CCP:0.287\n[0.229,0.345]","","",""))
dg_abs3<-data.frame(values=unique(datatidyr$values),
                    labels1=c("Public:0.012\n[-0.025,0.048]",
                              "Public:0.012\n[-0.025,0.048]",
                              "Public:0.005\n[-0.031,0.041]",
                              "Public:0.012\n[-0.024,0.048]"),
                    labels2=c("CCP:0.230\n[0.150,0.310]",
                              "CCP:0.326\n[0.244,0.407]",
                              "CCP:0.169\n[0.084,0.254]",
                              "CCP:0.347\n[0.265,0.429]"))

dg_abs4<-data.frame(values=unique(datatidyr$values),
                    labels1=c("Public:0.084\n[0.052,0.115]",
                              "Public:0.122\n[0.091,0.153]",
                              "Public:0.041\n[0.009,0.074]",
                              "Public:0.125\n[0.094,0.156]"),
                    labels2=c("CCP:0.421\n[0.327,0.516]",
                              "CCP:0.351\n[0.259,0.444]",
                              "CCP:0.165\n[0.056,0.275]",
                              "CCP:0.458\n[0.368,0.549]"))

pdf("Figure2.pdf",width=10, height=8)
ggplot(weight = datatidyr$wcn2)+
  geom_density_ridges(data=datatidyr,aes(y = str_survey,
                                         x = value,color = paste(str_survey, party),    
                                         fill = paste(str_survey, party),
                                         linetype = paste(str_survey, party)),
                      alpha = .001,na.rm = FALSE,scale = 0.95)+
  facet_grid(. ~ values)+
  scale_fill_cyclical(breaks = c("0", "1"),
                      labels = c("Public","CCP Member"),
                      values = c("#377eb8","#e41a1c"),
                      name = "") +
  scale_color_cyclical(breaks = c("0", "1"),
                       labels = c("Public","CCP Member"),
                       values = c("#377eb8","#e41a1c"),
                       name = "") +
  scale_linetype_cyclical(breaks = c("0", "1"),
                          labels = c("Public","CCP Member"),
                          values = c(1,5),
                          name = "") +
  ylab("")+xlab("")+
  geom_text(data=dg_cfps,    mapping=aes(x=6,y=1.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cfps,    mapping=aes(x=6,y=1.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2010,mapping=aes(x=6,y=2.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2010,mapping=aes(x=6,y=2.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2012,mapping=aes(x=6,y=3.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2012,mapping=aes(x=6,y=3.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2013,mapping=aes(x=6,y=4.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2013,mapping=aes(x=6,y=4.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2015,mapping=aes(x=6,y=5.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2015,mapping=aes(x=6,y=5.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs3,    mapping=aes(x=6,y=6.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs3,    mapping=aes(x=6,y=6.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs4,    mapping=aes(x=6,y=7.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs4,    mapping=aes(x=6,y=7.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  theme_bw() + scale_x_continuous(expand=c(0.01,0))+
  scale_y_discrete(expand=c(0.01,0),
                   limits=c("CFPS2014","CGSS2010","CGSS2012",
                            "CGSS2013","CGSS2015",
                            "ABS3(2011)","ABS4(2015)"))+
  theme(text=element_text(family="Times"),
        legend.position="bottom",legend.direction="horizontal",
        axis.title.x=element_text(face="bold",vjust=-.5),
        axis.title.y=element_text(face="bold",vjust=1),
        axis.text.x=element_text(size=12),
        strip.text=element_text(size=13))
dev.off()  

###################################################
# Figure 3: Citizen-Party Member-Cadre Comparison #
###################################################

rm(list=ls(all=TRUE))

library(readstata13)
library(ggplot2)
library(ggridges)
library(tidyr)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.dta13("data.dta")
data=data[complete.cases(data$cate),]
datatidyr <- data %>% gather(attribute, value, social_value:modern_all_value)
datatidyr$values <- factor(datatidyr$attribute, levels = c("social_value", "political_value", "intl_value", "modern_all_value"),labels=c("Social","Political","International","Overall modern value"))

pdf("Figure3.pdf",width=10, height=8)
ggplot(weight = datatidyr$wcn2)+
  geom_density_ridges(data=datatidyr,aes(y = values,
                                         x = value,
                                         color = paste(values, cate),
                                         linetype=paste(values, cate),
                                         fill = paste(values, cate)),
                      alpha = .001,na.rm = FALSE,scale=0.95)+
  scale_fill_cyclical(breaks = c("0", "1","2"),
                      labels = c("Public","CCP Member","Cadre"),
                      values = c("#377eb8","#e41a1c","#4daf4a"),
                      name = "") +
  scale_color_cyclical(breaks = c("0", "1","2"),
                       labels = c("Public","CCP Member","Cadre"),
                       values = c("#377eb8","#e41a1c","#4daf4a"),
                       name = "") +
  scale_linetype_cyclical(breaks = c("0", "1","2"),
                          labels = c("Public","CCP Member","Cadre"),
                          values = c(1,2,4),
                          name = "") +
  ylab("")+xlab("")+
  scale_y_discrete(expand=c(0.01,0),
                   limits=c("Overall modern value",
                            "International",
                            "Political",
                            "Social"))+
  annotate(geom="text", x=3, y=4.4,hjust = 0,size=4,
           color="#377eb8",lineheight=0.8,
           label="Public:0.011\n[0.004,0.018]")+
  annotate(geom="text", x=3, y=4.25,hjust = 0,size=4,
           color="#e41a1c",lineheight=0.8,
           label="Party:0.328\n[0.305,0.351]")+
  annotate(geom="text", x=3, y=4.1,hjust = 0,size=4,
           color="#4daf4a",lineheight=0.8,
           label="Cadre:0.563\n[0.508,0.618]")+
  annotate(geom="text", x=3, y=3.5,hjust = 0,size=4,
           color="#377eb8",lineheight=0.8,
           label="Public:0.122\n[0.091,0.153]")+
  annotate(geom="text", x=3, y=3.35,hjust = 0,size=4,
           color="#e41a1c",lineheight=0.8,
           label="Party:0.356\n[0.255,0.457]")+
  annotate(geom="text", x=3, y=3.2,hjust = 0,size=4,
           color="#4daf4a",lineheight=0.8,
           label="Cadre:0.317\n[0.116,0.518]")+
  annotate(geom="text", x=3, y=2.5,hjust = 0,size=4,
           color="#377eb8",lineheight=0.8,
           label="Public:0.041\n[0.009,0.074]")+
  annotate(geom="text", x=3, y=2.35,hjust = 0,size=4,
           color="#e41a1c",lineheight=0.8,
           label="Party:0.174\n[0.057,0.291]")+
  annotate(geom="text", x=3, y=2.2,hjust = 0,size=4,
           color="#4daf4a",lineheight=0.8,
           label="Cadre:0.100\n[-0.220,0.420]")+
  annotate(geom="text", x=3, y=1.5,hjust = 0,size=4,
           color="#377eb8",lineheight=0.8,
           label="Public:0.125\n[0.094,0.156]")+
  annotate(geom="text", x=3, y=1.35,hjust = 0,size=4,
           color="#e41a1c",lineheight=0.8,
           label="Party:0.465\n[0.366,0.564]")+
  annotate(geom="text", x=3, y=1.2,hjust = 0,size=4,
           color="#4daf4a",lineheight=0.8,
           label="Cadre:0.409\n[0.206,0.613]")+
  theme_bw()+theme(text=element_text(family="Times"),
                   legend.position="bottom",legend.direction="horizontal",
                   axis.title.x=element_text(face="bold",size=16,vjust=-.5),
                   axis.title.y=element_text(face="bold",size=16,vjust=1),
                   axis.text.x=element_text(size=12),
                   axis.text.y=element_text(size=14),
                   strip.text=element_text(size=13))
dev.off()

#####################################################################################
# Figure 4: Party-Public Comparison in Subsamples Less Affected Social Desirability #
#####################################################################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=4)

data$Value <- factor(data$Value, levels = c("social","political","intl","modern"),labels=c("Social","Political","International","Overall modern value"))
data$question <- factor(data$question, levels = c('Answer reliable.',
                                              'No doubts about the survey.',
                                              '"Government officials often violate law and abuse power"',
                                              '"Officials often conceal information from the public"',
                                              '"Criminal officials often escape punishment"'),
                         labels=c('Answer reliable.',
                                  'No doubts about the survey.',
                                  '"Government officials often \n violate law and abuse power"',
                                  '"Officials often conceal \n information from the public"',
                                  '"Criminal officials often \n escape punishment"'))
pd <- position_dodge(0.6)

pdf("Figure4.pdf",width=9, height=5)
ggplot(data,aes(x=question, y=mean,
                group=as.factor(party),
                shape=as.factor(party),
                color=as.factor(party),
                label = round(mean,3)))+
  geom_point(size = 1.5,position=pd)+
  geom_hline(aes(yintercept=0), colour="#62BCE9", linetype="dashed")+
  geom_text(size=2.8,hjust = 0.5,vjust = -0.6,position=pd,show.legend=FALSE)+
  geom_errorbar(aes(ymin=lb95, 
                    ymax=ub95),width = 0,position=pd)+
  facet_grid(. ~ Value)+
  scale_color_manual(values=c("#377eb8","#e41a1c"),
                     name = "",
                     breaks=c("0","1"),
                     labels=c("Public","CCP member"))+
  scale_shape_manual(name = "",
                     breaks=c("0","1"),
                     labels=c("Public","CCP member"),
                     values=c(19,17))+
  xlab("")+ylab("Weighted Mean Estimator")+
  scale_y_continuous(limits=c(-0.1,0.8),breaks=seq(-0,0.8,0.2))+
  theme_bw()+
  theme(text=element_text(family="Times"),
        legend.position="bottom",legend.direction="horizontal",
        axis.title.x=element_text(face="bold",size=16,vjust=-.5),
        axis.title.y=element_text(face="bold",size=16,vjust=1),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(hjust=0),
        axis.text.y.left  = element_text(size=10),
        strip.text=element_text(size=13),
        legend.text = element_text(size=10))+
  coord_flip()
dev.off() 

########################################
# Figure 5: Heterogenous Party Effects #
########################################

rm(list=ls(all=TRUE))
library(openxlsx)
library(ggplot2)
setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=5)

data$Value <- factor(data$Value, levels = c("social", "political", "intl", "modern"),labels=c("Social","Political","International","Overall modern value"))

pd <- position_dodge(0.8)

pdf("Figure5.pdf", width=9, height=8)
ggplot(data,aes(x=cate,y=mean,label=round(mean,3),
                color=as.factor(party),shape=as.factor(party)))+
  geom_hline(aes(yintercept=0), colour="#62BCE9", linetype="dashed")+
  geom_point(size = 1.8,position=pd)+
  geom_errorbar(aes(ymin=lb95,ymax=ub95),width = 0,position=pd)+
  geom_text(size=2.8,hjust = 0.5,vjust = -0.6,
            position=pd,show.legend=FALSE)+
  facet_grid(~Value)+
  scale_y_continuous(limits=c(-0.6,1.02),breaks=seq(-0.4,1.0,0.4))+
  scale_x_discrete(limits=c("    75%-100% (highest)",
                            "    25%-75% (middle)",
                            "    0%-25% (lowest)",
                            "Income level",
                            "    Old (60)",
                            "    Middle age (45)",
                            "    Young (30)","Age",
                            "    Urban","    Rural","Residency",
                            "    Female","    Male","Gender",
                            "    College",
                            "    No college",
                            "Education"))+
  scale_color_manual(values=c("#377eb8","#e41a1c"),
                     name = "",
                     breaks=c("0","1"),
                     labels=c("Public","CCP member"))+
  scale_shape_manual(name = "",
                     breaks=c("0","1"),
                     labels=c("Public","CCP member"),
                     values=c(19,17))+
  xlab("")+ylab("Predicted Ideology")+theme_bw() +
  theme(text=element_text(family="Times"),
        legend.position="bottom",
        legend.direction="horizontal",
        axis.title.x=element_text(face="bold",size=16,vjust=-.5),
        axis.title.y=element_text(face="bold",size=16,vjust=1),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=c(10,10,10,11,10,10,10,
                                11,10,10,11,10,10,11,10,10,11),
                      face = c('plain','plain','plain','bold',
                               'plain','plain','plain','bold',
                               'plain','plain','bold','plain',
                               'plain','bold','plain','plain',
                               'bold'),hjust = 0),
        strip.text.x = element_text(size=13),
        legend.text = element_text(size=11))+
  coord_flip()
dev.off() 


####################################
# Figure A.1: PCA and Cronbach??s ?? #
####################################

rm(list=ls(all=TRUE))
library(openxlsx)
library(ggplot2)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=6)
data<-subset(data,data$comp<11)

data$Value <- factor(data$Value, levels = c("social", "political",
                                         "intl", "modern"),
                    labels=c("Social","Political","International","Overall modern value"))
data$survey <- factor(data$survey, levels = c("ABS4(2015)", "ABS3(2011)", "CGSS2015", "CGSS2013","CGSS2012","CGSS2010","CFPS2014"))

pdf("Figurea1.pdf",width=10, height=8)
ggplot(data,aes(x=comp,y=pro,label=propt))+
  geom_bar(stat="identity")+geom_line()+geom_point()+
  geom_text(vjust=-0.1,hjust=-0.1)+
  geom_text(data=data,mapping=aes(x=9,y=0.5,label=alpha),
            size=4,lineheight=0.8,hjust = 1,parse = TRUE)+
  facet_grid(survey ~ Value)+
  scale_x_continuous(limits=c(0.5,10.5),breaks=seq(1,10,1))+
  scale_y_continuous(limits=c(0,0.64))+
  ylab("Explained variances share")+xlab("Component Number")+
  theme_bw()+theme(text=element_text(family="Times"),
                   legend.position="bottom",
                   legend.direction="horizontal",
                   axis.title.x=element_text(face="bold",vjust=-.5),
                   axis.title.y=element_text(face="bold",vjust=1),
                   title=element_text(face="bold",size=16,vjust=1.5),
                   axis.text.x=element_text(size=12),
                   strip.text=element_text(size=13))
dev.off() 

############################################################################
# FFigure A.3: Comparing Demographics of CCP Members and Population Census #
############################################################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)
library(tidyr)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=7)

datatidyr <- data %>% gather(census, value, Population.Census:CCP.Census)

theme_set(theme_bw() + theme(text=element_text(family="Times"),legend.position="bottom",legend.direction="horizontal",axis.title.x=element_text(face="bold",vjust=-.5),axis.title.y=element_text(face="bold",vjust=1),title=element_text(face="bold",size=16,vjust=1.5),axis.text.x=element_text(size=12),strip.text=element_text(size=13)))

pdf("Figurea3.pdf",width=6, height=8)
ggplot(datatidyr,aes(x=att, y=value,fill=census))+
  geom_bar(stat = "identity",position="dodge", 
           colour="black",alpha=0.5)+
  scale_fill_brewer(palette="Set1",name = "",
                    breaks=c("CCP.Census", "Population.Census"),
                    labels=c("CCP Census", "Population Census"))+
  scale_x_discrete(limits=c("minority","some university and above",
                            "71 and above","66-70","61-65",
                            "56-60","51-55","46-50","41-45",
                            "36-40","31-35","18-30","female"))+
  xlab("Attribute")+ylab("Proportion")+
  coord_flip()
dev.off() 

###########################################################
# Figure A.4: Party-Public Value Differences (No Weights) #
###########################################################

rm(list=ls(all=TRUE))

library(readstata13)
library(ggplot2)
library(ggridges)
library(tidyr)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")

data<-read.dta13("data.dta")

datatidyr <- data %>% gather(attribute, value, social_value:modern_all_value)
datatidyr$values <- factor(datatidyr$attribute, levels = c("social_value", "political_value", "intl_value", "modern_all_value"),labels=c("Social","Political","International","Overall modern value"))

dg_cfps    <-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.021\n[-0.032,-0.010]","","",""),labels2=c("CCP:0.247\n[0.210,0.284]","","",""))
dg_cgss2010<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.055\n[-0.074,-0.036]","","",""),labels2=c("CCP:0.391\n[0.341,0.440]","","",""))
dg_cgss2012<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.051\n[-0.070,-0.032]","","",""),labels2=c("CCP:0.376\n[0.325,0.427]","","",""))
dg_cgss2013<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.031\n[-0.050,-0.012]","","",""),labels2=c("CCP:0.275\n[0.216,0.334]","","",""))
dg_cgss2015<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.035\n[-0.055,-0.015]","","",""),labels2=c("CCP:0.302\n[0.244,0.360]","","",""))
dg_abs3<-data.frame(values=unique(datatidyr$values),
                    labels1=c("Public:-0.020\n[-0.056,0.016]",
                              "Public:-0.035\n[-0.071,0.001]",
                              "Public:-0.020\n[-0.056,0.016]",
                              "Public:-0.037\n[-0.074,-0.001]"),
                    labels2=c("CCP:0.125\n[0.044,0.206]",
                              "CCP:0.197\n[0.118,0.275]",
                              "CCP:0.113\n[0.029,0.198]",
                              "CCP:0.211\n[0.132,0.290]"))

dg_abs4<-data.frame(values=unique(datatidyr$values),
                    labels1=c("Public:-0.032\n[-0.065,0.000]",
                              "Public:-0.023\n[-0.055,0.010]",
                              "Public:-0.017\n[-0.050,0.015]",
                              "Public:-0.036\n[-0.069,-0.004]"),
                    labels2=c("CCP:0.292\n[0.198,0.387]",
                              "CCP:0.203\n[0.108,0.299]",
                              "CCP:0.096\n[-0.009,0.200]",
                              "CCP:0.296\n[0.204,0.388]"))

pdf("Figurea4.pdf",width=10, height=8)
ggplot()+
  geom_density_ridges(data=datatidyr,aes(y = str_survey,
                                         x = value,color = paste(str_survey, party),    
                                         fill = paste(str_survey, party),
                                         linetype = paste(str_survey, party)),
                      alpha = .001,na.rm = FALSE,scale = 0.95)+
  facet_grid(. ~ values)+
  scale_fill_cyclical(breaks = c("0", "1"),
                      labels = c("Public","CCP Member"),
                      values = c("#377eb8","#e41a1c"),
                      name = "") +
  scale_color_cyclical(breaks = c("0", "1"),
                       labels = c("Public","CCP Member"),
                       values = c("#377eb8","#e41a1c"),
                       name = "") +
  scale_linetype_cyclical(breaks = c("0", "1"),
                          labels = c("Public","CCP Member"),
                          values = c(1,5),
                          name = "") +
  ylab("")+xlab("")+
  geom_text(data=dg_cfps,    mapping=aes(x=6,y=1.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cfps,    mapping=aes(x=6,y=1.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2010,mapping=aes(x=6,y=2.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2010,mapping=aes(x=6,y=2.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2012,mapping=aes(x=6,y=3.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2012,mapping=aes(x=6,y=3.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2013,mapping=aes(x=6,y=4.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2013,mapping=aes(x=6,y=4.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2015,mapping=aes(x=6,y=5.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2015,mapping=aes(x=6,y=5.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs3,    mapping=aes(x=6,y=6.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs3,    mapping=aes(x=6,y=6.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs4,    mapping=aes(x=6,y=7.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs4,    mapping=aes(x=6,y=7.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  theme_bw() + scale_x_continuous(expand=c(0.01,0))+
  scale_y_discrete(expand=c(0.01,0),
                   limits=c("CFPS2014","CGSS2010","CGSS2012",
                            "CGSS2013","CGSS2015",
                            "ABS3(2011)","ABS4(2015)"))+
  theme(text=element_text(family="Times"),
        legend.position="bottom",legend.direction="horizontal",
        axis.title.x=element_text(face="bold",vjust=-.5),
        axis.title.y=element_text(face="bold",vjust=1),
        axis.text.x=element_text(size=12),
        strip.text=element_text(size=13))
dev.off() 

########################################################################
# Figure A.5: Party-Public Value Differences (Original Survey Weights) #
########################################################################

rm(list=ls(all=TRUE))

library(readstata13)
library(ggplot2)
library(ggridges)
library(tidyr)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")

data<-read.dta13("data.dta")

datatidyr <- data %>% gather(attribute, value, social_value:modern_all_value)
datatidyr$values <- factor(datatidyr$attribute, levels = c("social_value", "political_value", "intl_value", "modern_all_value"),labels=c("Social","Political","International","Overall modern value"))

dg_cfps    <-data.frame(values=unique(datatidyr$values),labels1=c("Public:0.093\n[0.082,0.104]","","",""),labels2=c("CCP:0.339\n[0.301,0.376]","","",""))
dg_cgss2010<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.090\n[-0.109,-0.071]","","",""),labels2=c("CCP:0.347\n[0.297,0.396]","","",""))
dg_cgss2012<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.050\n[-0.069,-0.031]","","",""),labels2=c("CCP:0.414\n[0.363,0.466]","","",""))
dg_cgss2013<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.031\n[-0.050,-0.012]","","",""),labels2=c("CCP:0.317\n[0.257,0.376]","","",""))
dg_cgss2015<-data.frame(values=unique(datatidyr$values),labels1=c("Public:-0.054\n[-0.074,-0.035]","","",""),labels2=c("CCP:0.284\n[0.226,0.343]","","",""))
dg_abs3<-data.frame(values=unique(datatidyr$values),
                    labels1=c("Public:0.012\n[-0.025,0.048]",
                              "Public:0.012\n[-0.024,0.048]",
                              "Public:0.005\n[-0.031,0.041]",
                              "Public:0.012\n[-0.024,0.048]"),
                    labels2=c("CCP:0.163\n[0.082,0.244]",
                              "CCP:0.245\n[0.167,0.324]",
                              "CCP:0.142\n[0.057,0.227]",
                              "CCP:0.264\n[0.185,0.343]"))

dg_abs4<-data.frame(values=unique(datatidyr$values),
                    labels1=c("Public:0.084\n[0.052,0.115]",
                              "Public:0.122\n[0.091,0.153]",
                              "Public:0.037\n[0.004,0.069]",
                              "Public:0.122\n[0.091,0.154]"),
                    labels2=c("CCP:0.384\n[0.290,0.479]",
                              "CCP:0.307\n[0.214,0.400]",
                              "CCP:0.132\n[0.025,0.240]",
                              "CCP:0.407\n[0.316,0.497]"))

pdf("Figurea5.pdf",width=10, height=8)
ggplot(weight = datatidyr$wcn)+
  geom_density_ridges(data=datatidyr,aes(y = str_survey,
                                         x = value,color = paste(str_survey, party),    
                                         fill = paste(str_survey, party),
                                         linetype = paste(str_survey, party)),
                      alpha = .001,na.rm = FALSE,scale = 0.95)+
  facet_grid(. ~ values)+
  scale_fill_cyclical(breaks = c("0", "1"),
                      labels = c("Public","CCP Member"),
                      values = c("#377eb8","#e41a1c"),
                      name = "") +
  scale_color_cyclical(breaks = c("0", "1"),
                       labels = c("Public","CCP Member"),
                       values = c("#377eb8","#e41a1c"),
                       name = "") +
  scale_linetype_cyclical(breaks = c("0", "1"),
                          labels = c("Public","CCP Member"),
                          values = c(1,5),
                          name = "") +
  ylab("")+xlab("")+
  geom_text(data=dg_cfps,    mapping=aes(x=6,y=1.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cfps,    mapping=aes(x=6,y=1.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2010,mapping=aes(x=6,y=2.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2010,mapping=aes(x=6,y=2.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2012,mapping=aes(x=6,y=3.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2012,mapping=aes(x=6,y=3.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2013,mapping=aes(x=6,y=4.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2013,mapping=aes(x=6,y=4.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2015,mapping=aes(x=6,y=5.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_cgss2015,mapping=aes(x=6,y=5.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs3,    mapping=aes(x=6,y=6.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs3,    mapping=aes(x=6,y=6.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs4,    mapping=aes(x=6,y=7.5,label=labels1,color="#377eb8"),size=2.8,lineheight=0.8,hjust = 1)+
  geom_text(data=dg_abs4,    mapping=aes(x=6,y=7.8,label=labels2,color="#e41a1c"),size=2.8,lineheight=0.8,hjust = 1)+
  theme_bw() + scale_x_continuous(expand=c(0.01,0))+
  scale_y_discrete(expand=c(0.01,0),
                   limits=c("CFPS2014","CGSS2010","CGSS2012",
                            "CGSS2013","CGSS2015",
                            "ABS3(2011)","ABS4(2015)"))+
  theme(text=element_text(family="Times"),
        legend.position="bottom",legend.direction="horizontal",
        axis.title.x=element_text(face="bold",vjust=-.5),
        axis.title.y=element_text(face="bold",vjust=1),
        axis.text.x=element_text(size=12),
        strip.text=element_text(size=13))
dev.off()  

##################################################################################################
# Figure A.6: Correlations between Social and Political Domains for Party Members and NonMembers #
##################################################################################################

rm(list=ls(all=TRUE))

library(readstata13)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.dta13("data.dta")

party<-subset(data,data$party==1)
public<-subset(data,data$party==0)

p1<-ggplot(party,aes(x=social_value,y=political_value))+
  geom_point(alpha=0.7)+geom_smooth(method='lm')+
  xlab("Social")+ylab("Political")+
  annotate(geom="text", x=-4, y=4,hjust = 0,size=5,
           label  = "paste(Party: rho==0.467, \" *** \")",
           parse = TRUE)+
  scale_x_continuous(limits=c(-4.1,4.1),breaks=seq(-4,4,1))+
  scale_y_continuous(limits=c(-4.1,4.1),breaks=seq(-4,4,1))
p2<-ggplot(public,aes(x=social_value,y=political_value))+
  geom_point(alpha=0.7)+geom_smooth(method='lm')+
  xlab("Social")+ylab("")+
  annotate(geom="text", x=-4, y=4,hjust = 0,size=5,
           label  = "paste(Public: rho==0.492, \" *** \")",
           parse = TRUE)+
  scale_x_continuous(limits=c(-4.1,4.1),breaks=seq(-4,4,1))+
  scale_y_continuous(limits=c(-4.1,4.1),breaks=seq(-4,4,1))

pdf("Figurea6.pdf", width=10, height=5)
grid.arrange(p1,p2, ncol=2, nrow=1)
dev.off() 

###########################################
# Figure A.7: By Year of Party Enrollment #
###########################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)
library(tidyr)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=10)

datatidyr <- data %>% gather(attribute, value, social:modern)

theme_set(theme_bw() + theme(text=element_text(family="Times"),legend.position="bottom",legend.direction="horizontal",axis.title.x=element_text(face="bold",vjust=-.5),axis.title.y=element_text(face="bold",vjust=1),title=element_text(face="bold",size=16,vjust=1.5),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12),strip.text=element_text(size=13)))

pdf("Figurea7.pdf",width=10, height=6)
ggplot(datatidyr,aes(x=yrgroup, y=value,color=attribute,
                     group=attribute,shape=attribute))+
  geom_point(size=2)+
  geom_line(aes(linetype=attribute))+
  scale_color_brewer(palette="Set1",name = "",
                     breaks=c("social", "political","intl","modern"),
                     labels=c("Social",
                              "Political",
                              "International",
                              "Overall modern value"))+
  scale_linetype_manual(name = "",
                        breaks=c("modern", "demo","intl","liberal"),
                        labels=c("Social",
                                 "Political",
                                 "International",
                                 "Overall modern value"),
                        values=c("dashed","twodash","solid","dotted"))+
  scale_shape_manual(name = "",
                     breaks=c("modern", "demo","intl","liberal"),
                     labels=c("Social",
                              "Political",
                              "International",
                              "Overall modern value"),
                     values=c(19,17,18,15))+
  xlab("Year Group")+ylab("")+
  theme(legend.text = element_text(size=11))+
  scale_x_discrete(limits=c("<1950","1950-1965","1966-1976",
                            "1977-1989","1990-2002",
                            "2003-2012",">2012"))
dev.off() 

##################################################################################
# Figure A.8: Comparing China with Other East Asian Countries/Regions (ABS 2015) #
##################################################################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)
library(tidyr)
library(gridExtra)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=11)

data1<-subset(data,data$Value=="social")
data2<-subset(data,data$Value=="political")
data3<-subset(data,data$Value=="intl")
data4<-subset(data,data$Value=="modern")
p1<-ggplot(data1, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("Social")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','#377eb8','black','black','black','black','#e41a1c','black','black','black'),
                                   face = c('plain','plain','plain','plain','plain','plain','bold','plain','plain','plain','plain','bold','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
p2<-ggplot(data2, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("Political")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','black','#377eb8','black','#e41a1c','black','black','black'),
                                   face = c('plain','plain','plain','plain','plain','plain','plain','bold','plain','bold','plain','plain','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
p3<-ggplot(data3, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("International")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','black','black','black','black','#377eb8','black','black','#e41a1c'),
                                   face = c('plain','plain','plain','plain','plain','plain','plain','plain','plain','plain','bold','plain','plain','bold')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
p4<-ggplot(data4, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("Overall modern value")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','black','black','#377eb8','black','#e41a1c','black','black'),
                                   face = c('plain','plain','plain','plain','plain','plain','plain','plain','bold','plain','bold','plain','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

pdf("Figurea8.pdf",width=16, height=8)
grid.arrange(p1,p2,p3,p4, ncol=4, nrow=1)
dev.off() 

##################################################################################
# Figure A.9: Comparing China with Other East Asian Countries/Regions (ABS 2011) #
##################################################################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)
library(tidyr)
library(gridExtra)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=12)

data1<-subset(data,data$Value=="social")
data2<-subset(data,data$Value=="political")
data3<-subset(data,data$Value=="intl")
data4<-subset(data,data$Value=="modern")
p1<-ggplot(data1, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("Social")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','#377eb8','black','black','black','#e41a1c','black','black','black','black','black','black'),
                                   face = c('plain','plain','plain','bold','plain','plain','plain','bold','plain','plain','plain','plain','plain','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
p2<-ggplot(data2, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("Political")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','#377eb8','black','#e41a1c','black','black','black','black','black'),
                                   face = c('plain','plain','plain','plain','plain','plain','bold','plain','bold','plain','plain','plain','plain','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
p3<-ggplot(data3, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("International")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','black','black','black','black','black','#377eb8','#e41a1c','black'),
                                   face = c('plain','plain','plain','plain','plain','plain','plain','plain','plain','plain','plain','bold','bold','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
p4<-ggplot(data4, aes(x=mean, y=reorder(country, mean),
                      color=as.factor(color),
                      shape=as.factor(color))) +
  geom_point(size=3,fill='white') +
  theme_bw() +ylab("")+xlab("")+
  ggtitle("Overall modern value")+
  scale_color_manual(values=c("#000000","#377eb8","#e41a1c"))+
  scale_shape_manual(values=c(21,16,16))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=12,color = c('black','black','black','black','black','black','#377eb8','black','black','#e41a1c','black','black','black','black'),
                                   face = c('plain','plain','plain','plain','plain','plain','bold','plain','plain','bold','plain','plain','plain','plain')),
        legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

pdf("Figurea9.pdf",width=16, height=8)
grid.arrange(p1,p2,p3,p4, ncol=4, nrow=1)
dev.off() 

##################################################################
# Figure A.10: Compare Party with Intellectuals and Middle-Class #
##################################################################

rm(list=ls(all=TRUE))

library(openxlsx)
library(ggplot2)

setwd("C:/Users/JCY/Dropbox/Ideology/replicate")
data<-read.xlsx("Fig.xlsx",sheet=13)

theme_set(theme_bw() + theme(text=element_text(family="Times"),legend.position="bottom",legend.direction="horizontal",axis.title.x=element_text(face="bold",vjust=-.5),axis.title.y=element_text(face="bold",vjust=1),title=element_text(face="bold",size=16,vjust=1.5),axis.text.x=element_text(size=12),strip.text=element_text(size=13)))

data$Value <- factor(data$Value, levels = c("social", "political", "intl", "modern"),labels=c("Social","Political","International","Overall modern value"))
data$group <- factor(data$group, levels = c("Public", "CCP (6.34% of the sample)", "Middle class (14.15% of the sample)", "Intellectuals (11.72% of the sample)"),
                      labels=c("The rest (65.36% of the sample)", "CCP members (6.34% of the sample)", "Middle class (14.15% of the sample)", "Intellectuals (11.72% of the sample)"))
pdf("Figurea10.pdf",width=9, height=4)
ggplot(data,aes(x=mean,y=group,label=round(mean,3)))+
  geom_point(size = 1.2)+
  geom_text(size=4,hjust = 0.5,vjust = -.7, show.legend=FALSE)+
  xlab("Weighted Mean Estimator")+ylab("")+
  scale_x_continuous(limits=c(-0.28,1.1))+
  facet_grid(. ~ Value)
dev.off()
