###########################################################################
###########################################################################
### SCRIPT TO PRODUCE CITY-YEAR PANEL DATA FROM CPED FOR MAIN ANALYSIS ####
###########################################################################
###########################################################################
###########################################################################

# Author: Junyan Jiang (Asst. Prof, Dept of Government and Public Administration, Chinese University of Hong Kong)
# Version: 1.0
# Tested under:
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32   

##### NOTE: BEFORE RUNNING THIS, PLEASE ENSURE YOUR COMPUTER SUPPORTS SIMPLIFIED CHINESE ############



library(plyr)
library(reshape)
library(reshape2)
# Code to install IRange package 
#source("https://bioconductor.org/biocLite.R")
#biocLite("IRanges")
library(IRanges)
library(data.table)
library(lubridate)
library(foreign)
Sys.setlocale(,"CHS")


## Set the working directory to the folder that contains the replication files ##

setwd("C:/Users/junya/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")


########################################################################
##################------ Load the Data ----------#######################
########################################################################

# Load the basic city-year structure
# Encoding of the CSC File = GB1312

structure<-read.csv("cityyear_structure.csv",stringsAsFactors = F) 
names(structure)<-c("cityid","year","cname","citylevel") # give English names to columns
structure$provid<-as.numeric(paste(substring(structure$cityid,1,2),"0000",sep=""))


d<-read.csv("fullbio1106.csv",stringsAsFactors=F)
db<-read.csv("base1106.csv",stringsAsFactors=F)[,1:11]

### Load preprocessing script and core functions  ###

source("(FUN) cleanup.R",encoding="utf-8")

source("(FUN) corefunctions.R",encoding="utf-8")

source("(FUN)_datatool.R",encoding="utf-8")

################################################################
#########----CONSTRUCT COMPREHENSIVE CITY-YEAR DATA------#######
################################################################


citypanel<-structure[-which(structure[,4]=="省直辖县"),]
after11<-citypanel[which(citypanel$year==2013),]
newd<-list()

for(i in 2014:2015){
  after11$year<-i
  newd[[i-2011]]<-after11
}
citypanel<-data.frame(rbind(citypanel,rbindlist(newd)),stringsAsFactors=F)

citypanel$provid<-as.numeric(paste(substring(citypanel$cityid,1,2),"0000",sep=""))
#write.csv(citypanel,"cityyear.csv",row.names=F)
citypanel<-citypanel[order(citypanel$cityid,citypanel$year),]



for(i in 1:nrow(citypanel)){
  cat(paste(citypanel[i,c("cityid","year")],collapse="---"),"\n")
  
  citypanel[i,"province"]<-unique(d[which(d$provid==citypanel[i,"provid"]),"province"])
  ybeg<-as.Date(paste(citypanel[i,"year"],"-01-01",sep=""),format='%Y-%m-%d')
  
  yend<-as.Date(paste(citypanel[i,"year"],"-12-31",sep=""),format='%Y-%m-%d')
  
  
  ##---- Information about the Municipal Leaders ---##
  dmsec<-d[which(d$begin<=yend & d$end>ybeg & d$keypost=="市委书记" & d$cityid==citypanel[i,"cityid"]),]
  
  dmayor<-d[which(d$begin<=yend & d$end>ybeg & d$keypost=="市长" & d$cityid==citypanel[i,"cityid"]),]  

  
  # City secretary
  if(length(unique(dmsec[,"name"]))==0){
    citypanel[i,"msec"]<-NA
  }
  if(length(unique(dmsec[,"name"]))==1){
    citypanel[i,"msec"]<-unique(dmsec[,"name"])
  }
  if(length(unique(dmsec[,"name"]))>1){
    
    citypanel[i,"msec"]<-dmsec[which.max(dmsec[,"begin"]),"name"] # Choose the latest serving one
    
  }
  
  

  # Mayor
  if(length(unique(dmayor[,"name"]))<1){
    citypanel[i,"mayor"]<-NA
  }
  if(length(unique(dmayor[,"name"]))==1){
    citypanel[i,"mayor"]<-unique(dmayor[,"name"])}
  
  if(length(unique(dmayor[,"name"]))>1) {

    citypanel[i,"mayor"]<-dmayor[which.max(dmayor[,"begin"]),"name"] #Choose the latest serving one

  }

  #first year as mayor OR secretary
  fy<-min(na.omit(year(d[which(d$name==citypanel[i,"msec"] & d$keypost%in%c("市长","市委书记")),"begin"])))
  citypanel[i,"fy.msec"]<-ifelse(is.infinite(fy),NA,fy)
  fy<-min(na.omit(year(d[which(d$name==citypanel[i,"mayor"] & d$keypost%in%c("市长","市委书记")),"begin"])))
  citypanel[i,"fy.mayor"]<-ifelse(is.infinite(fy),NA,fy)
  
  #Tenure in the current city
  if(is.na(citypanel[i,"msec"])==F){
    citypanel[i,"msec.tenure"]<-citypanel[i,"year"]-min(na.omit(year(d[which(d$name==citypanel[i,"msec"] &d$cityid==citypanel[i,"cityid"] & d$keypost=="市委书记"),"begin"])))
  }
  if(is.na(citypanel[i,"mayor"])==F){
    citypanel[i,"mayor.tenure"]<-citypanel[i,"year"]-min(na.omit(year(d[which(d$name==citypanel[i,"mayor"] &d$cityid==citypanel[i,"cityid"] & d$keypost=="市长"),"begin"])))
  }

  
  ##---- Information about the Provincial Leaders
  
  dpsec<-d[which(d$keypost%in%"省委书记" & d$provid==citypanel[i,"provid"]),c("name","begin","end")]
  dgvn<-d[which(d$keypost%in%"省长" & d$provid==citypanel[i,"provid"]),c("name","begin","end")]

  # Choose the first one in a year
  citypanel[i,"current.psec"]<-unique(dpsec[which(dpsec$begin<=ybeg & dpsec$end>ybeg),"name"])
  
  ###### Record psec's arrival year #####
  citypanel[i,"psec.arrival"]<-min(year(dpsec$begin[dpsec$name==citypanel[i,"current.psec"]]),year(dgvn$begin[dgvn$name==citypanel[i,"current.psec"]]),na.rm=T)
  
  #/year-month
  citypanel[i,"psec.arrival.YM"]<-min(c(dpsec$begin[dpsec$name==citypanel[i,"current.psec"]],dgvn$begin[dgvn$name==citypanel[i,"current.psec"]]),na.rm=T)

  ##### psec's arrival as psec #####
  citypanel[i,"psec.arrival.aspsec"]<-min(year(dpsec$begin[dpsec$name==citypanel[i,"current.psec"]]),na.rm=T)
  
  #/year-month
  citypanel[i,"psec.arrival.aspsec.YM"]<-min(dpsec$begin[dpsec$name==citypanel[i,"current.psec"]],na.rm=T)
  
  ## Record psec's departure year
  citypanel[i,"psec.departure"]<-max(year(dpsec$end[dpsec$name==citypanel[i,"current.psec"]]))
  
  citypanel[i,"psec.age"]<-citypanel[i,"year"]-year(as.Date(db[which(db$name==citypanel[i,"current.psec"]),"dob"],format="%Y-%m-%d"))

  

  
  plist<-unique(dpsec[order(dpsec$end),"name"])
  citypanel[i,"F1.psec"]<-plist[which(plist==citypanel[i,"current.psec"])-1]
  
  citypanel[i,"F1.psec.birthyear"]<-year(db$dob[which(db$name==citypanel[i,"F1.psec"])])

  ### Governor####
  
  # Choose the first one in a year
  citypanel[i,"current.gvn"]<-unique(dgvn[which(dgvn$begin<=ybeg & dgvn$end>ybeg),"name"])
  
  ## Record governor's year of arrival
  
  citypanel[i,"gvn.arrival"]<-min(year(dgvn$begin[dgvn$name==citypanel[i,"current.gvn"]]))
  
  citypanel[i,"gvn.departure"]<-max(year(dgvn$end[dgvn$name==citypanel[i,"current.gvn"]]),year(dpsec$end[dpsec$name==citypanel[i,"current.gvn"]]),na.rm=T)
  
  citypanel[i,"gvn.age"]<-citypanel[i,"year"]-year(as.Date(db[which(db$name==citypanel[i,"current.gvn"]),"dob"],format="%Y-%m-%d"))
  

  plist<-unique(dgvn[order(dgvn$end),"name"])
  citypanel[i,"F1.gvn"]<-plist[which(plist==citypanel[i,"current.gvn"])-1]
  
}



# -------------Add A Few Essential Covariates --------------- #
yTOym<-function(x,month){
  as.Date(paste(x,month,sep="-"),format='%Y-%m-%d')
  
}

dNS<-d[-grep("学生|学员",d$exactjob),]

# fill in career information

citypanel[,"msec.rank"]<-mapply(function(i){
  if(is.na(citypanel[i,"msec"])){
    rk<-NA
  }else{
    rk<-max(dNS[which(dNS$name==citypanel[i,"msec"] & dNS$begin<=yTOym(citypanel[i,"year"],"12-31") & dNS$end>yTOym(citypanel[i,"year"],"01-01")),"rank"])
  }
  return(rk)
} ,i=1:nrow(citypanel))

citypanel[,"mayor.rank"]<-mapply(function(i){
  if(is.na(citypanel[i,"mayor"])){
    rk<-NA
  }else{
    rk<-max(dNS[which(dNS$name==citypanel[i,"mayor"] & dNS$begin<=yTOym(citypanel[i,"year"],"12-31") & dNS$end>yTOym(citypanel[i,"year"],"01-01")),"rank"])
  }
  return(rk)
} ,i=1:nrow(citypanel))

# Add a City leader's beginning and ending administrative ranks
citypanel<-citypanel[order(citypanel$msec,citypanel$cityid,citypanel$year),]
citypanel$msecid<-factor(paste(citypanel$msec,citypanel$cityid,sep="-"),levels=unique(paste(citypanel$msec,citypanel$cityid,sep="-")))

citypanel<-ddply(citypanel,.(msecid),transform,
                 msec.firstrank=head(msec.rank,1),
                 msec.lastrank=msec.rank[length(msec.rank)]
                 )


citypanel<-citypanel[order(citypanel$mayor,citypanel$cityid,citypanel$year),]
citypanel$mayorid<-factor(paste(citypanel$mayor,citypanel$cityid,sep="-"),levels=unique(paste(citypanel$mayor,citypanel$cityid,sep="-")))


citypanel<-ddply(citypanel,.(mayorid),transform,
                 mayor.firstrank=head(mayor.rank,1),
                 mayor.lastrank=mayor.rank[length(mayor.rank)]
                 )

citypanel<-citypanel[order(citypanel$cityid,citypanel$year),]

###############################################################################################
##################------ Promotion Ties with Prov Sec ----------#######################
###############################################################################################


promunder.mayor<-idGroup(client=unique(na.omit(citypanel$mayor)),type="2all",max.rank=2)

promunder.mayor.maxrank25<-idGroup(client=unique(na.omit(citypanel$mayor)),type="2all",max.rank=2.5)


promunder.sec<-idGroup(client=unique(na.omit(citypanel$msec)),type="2all",max.rank=2)

promunder.sec.maxrank25<-idGroup(client=unique(na.omit(citypanel$msec)),type="2all",max.rank=2.5)

promunder.sec.2sec<-idGroup(client=unique(na.omit(citypanel$msec)),type="2sec",max.rank=2.5)



names(promunder.mayor)<-c("mayor","mayor.by.sec","mayor.by.gvn","mayor.promote.year","mayor.promote.place")

names(promunder.mayor.maxrank25)<-c("mayor","mayor.by.sec.25","mayor.by.gvn.25","mayor.promote.year.25","mayor.promote.place.25")

names(promunder.sec)<-c("msec","msec.by.sec","msec.by.gvn","msec.promote.year","msec.promote.place")

names(promunder.sec.maxrank25)<-c("msec","msec.by.sec.25","msec.by.gvn.25","msec.promote.year.25","msec.promote.place.25")

names(promunder.sec.2sec)<-c("msec","msec.by.sec.2sec","msec.by.gvn.2sec","msec.promote.year.2sec","msec.promote.place.2sec")


data.mayor<-merge_recurse(list(promunder.mayor,promunder.mayor.maxrank25),by="mayor")

data.msec<-merge_recurse(list(promunder.sec,promunder.sec.maxrank25,promunder.sec.2sec),by="msec")

citypanel2<-merge(merge(citypanel,data.msec,by="msec",all.x=T),data.mayor,by="mayor",all.x=T)




# a small function to convert all factors to characters
facTOchar<-function(data,direction="f2c",excludeNA=NULL){
  if(direction=="f2c"){
    i<-sapply(data,is.factor)
    data[,i]<-lapply(data[,i],as.character)
    return(data)
  }
  if(direction=="c2f"){
    i<-sapply(data,is.character)
    data[,i]<-lapply(data[,i],function(x) factor(x,exclude=excludeNA))
    return(data)
  }

}

citypanel2<-facTOchar(citypanel2)


##------- Connected if promoted by current leadership -------#

## City Secretaries ##

# promoted under current psec 1
citypanel2$msec2currentsec<-with(citypanel2,ifelse(msec.by.sec.25==current.psec| msec.by.sec.2sec==current.psec,1,0))
citypanel2$msec2currentsec<-ifelse(is.na(citypanel2$msec2currentsec),0,citypanel2$msec2currentsec)


# promoted under current psec 2: (Including promotion under psec and gvn)
citypanel2$msec2currentsec2<-with(citypanel2,ifelse(msec.by.sec.25==current.psec|msec.by.gvn.25==current.psec, 1, 0))
citypanel2$msec2currentsec2<-ifelse(is.na(citypanel2$msec2currentsec2),0,citypanel2$msec2currentsec2)


# promoted under current gvn
citypanel2$msec2currentgvn<-with(citypanel2,ifelse(msec.by.gvn.25 != current.gvn | is.na(msec.by.gvn),0,1))




##   Mayors    ##


# promoted under current psec 1
citypanel2$mayor2currentsec<-with(citypanel2,ifelse(mayor.by.sec.25==current.psec,1,0))
citypanel2$mayor2currentsec<-ifelse(is.na(citypanel2$mayor2currentsec),0,citypanel2$mayor2currentsec)


# promoted under current psec 2: (Including promotion under psec and gvn)
citypanel2$mayor2currentsec2<-with(citypanel2,ifelse(mayor.by.sec.25==current.psec|mayor.by.gvn.25==current.psec, 1, 0))
citypanel2$mayor2currentsec2<-ifelse(is.na(citypanel2$mayor2currentsec2),0,citypanel2$mayor2currentsec2)


# promoted under current gvn
citypanel2$mayor2currentgvn<-with(citypanel2,ifelse(mayor.by.gvn != current.gvn | is.na(mayor.by.gvn),0,1))







### Calculate city leaders' local time 
citypanel2$msec.localtime<-citypanel2$mayor.localtime<-0

for(i in 1:nrow(citypanel2)){
  cat(i,"\n")
  provid<-citypanel2[i,"provid"]
  sec<-citypanel2[i,"msec"]
  my<-citypanel2[i,"mayor"]
  year<-citypanel2[i,"year"]
  cityid<-citypanel2[i,"cityid"]
  yr<-as.Date(paste(citypanel2[i,"year"],"-01-01",sep=""),format='%Y-%m-%d')
  
  dsec<-na.omit(d[which(d$name==sec & d$begin<=yr & d$cityid==cityid),c("begin","end")])
  if(nrow(dsec)>0){
    dsec[which(dsec$end>yr),"end"]<-yr
    citypanel2[i,"msec.localtime"]<-sum(width(reduce(IRanges(as.numeric(dsec[,"begin"]),as.numeric(dsec[,"end"])),min.gapwidth=0)))/365
  }
  dmayor<-na.omit(d[which(d$name==my & d$begin<=yr & d$cityid==cityid),c("begin","end")])
  if(nrow(dmayor)>0){
    dmayor[which(dmayor$end>yr),"end"]<-yr
    citypanel2[i,"mayor.localtime"]<-sum(width(reduce(IRanges(as.numeric(dmayor[,"begin"]),as.numeric(dmayor[,"end"])),min.gapwidth=0)))/365
  }
  
  
  citypanel2[i,"msec.prov.exp"]<-as.numeric(any(d$name==sec & year(d$end)<= year & d$provid==provid & d$city=="" & d$county=="" & d$rank<=2 & d$rank>=1 & d$basecat %in%c("党委","政府_国务院")))
  
  
  citypanel2[i,"mayor.prov.exp"]<-as.numeric(any(d$name==my & year(d$end)<= year & d$provid==provid & d$natorg=="否" & d$city=="" & d$county=="" & d$rank<=2 & d$rank>=1 & d$basecat %in% c("党委","政府_国务院")))
  
  # Municipal Leaders: work experience in this province
  
  citypanel2[i,"msec.central.exp"]<-as.numeric(any(d$name==sec & year(d$end)<= year & d$province=="中央" & d$city=="" & d$county=="" & d$basecat %in%c("党委","政府_国务院") & d$rank<=2 & d$rank>=1 ))
  
  
  citypanel2[i,"mayor.central.exp"]<-as.numeric(any(d$name==my & year(d$end)<= year & d$province=="中央" & d$city=="" & d$county=="" & d$basecat %in%c("党委","政府_国务院") & d$rank<=2 & d$rank>=1 ))
  
  
}



####################################################################################################
#########################                                      #####################################
#########################          CONTROL VARIABLES           #####################################
#########################                                      #####################################
####################################################################################################

###########-------------------M.Sec Loop -------------------##############


people<-na.omit(unique(citypanel2$msec))

for(i in 1:length(people)){

  
  ###################### Code Prior experience ########################
  before<-min(d[which(d$name==people[i] & d$keypost%in%c("市长","市委书记")),"begin"])
  dt<-d[which(d$name==people[i] & d$end<=before),]
  
  ############### EDUCATION CREDENTIAL #################
  
  dedu<-dt[which(dt$name==people[i] & dt$keypost=="学校" & dt$edu%in%c("本科","硕士","博士")),"begin"]
  if(length(dedu)==0){
    actualcollege<-0
    yrofcollege<-NA
  } else{
    actualcollege<-as.numeric(min(year(dedu))-year(db[db$name==people[i],"dob"])<30)
    yrofcollege<-min(year(dedu))
  }
  
  citypanel2$msec.edu[citypanel2$msec==people[i]]<-actualcollege 
  citypanel2$msec.edu.year[citypanel2$msec==people[i]]<-yrofcollege
  
  ############## BUSINESS MANAGEMENT EXPERIENCE ###################
  
  dsoe<-dt[which(dt$name==people[i] & dt$basecat%in%c("地方企业","中央企业")),]
  citypanel2$msec.soe.exp[citypanel2$msec==people[i]]<-as.numeric(length(grep("经理|董事长|厂长|总裁|历任数职|负责人|行长",dsoe$exactjob))>0)
  
  ############## UNIVERSITY MANAGEMENT ###################
  
  duniv<-dt[dt$basecat=="学校",]
  duniv<-duniv[-grep("党校",duniv$lv1job),]
  citypanel2$msec.univ.exp[citypanel2$msec==people[i]]<-as.numeric(length(intersect(grep("主任|校长|教授|院长|书记",duniv$exactjob),which(duniv$rank>=1.5)))>0)
  
  ################### YOUTH LEAGUE #######################
  citypanel2$msec.league[citypanel2$msec==people[i]]<-as.numeric(any(dt$basecat=="共青团" & dt$rank>=1))

  
  ################### COUNTY LEADERSHIP EXPERIENCE #################
  citypanel2$msec.county.exp[citypanel2$msec==people[i]]<-as.numeric(any(dt$county!="" & dt$rank<2 & dt$rank>0 & dt$basecat %in%c("党委","政府_国务院")))
  
  
  
  ################### FINANCE  EXPERIENCE ################
  citypanel2$msec.finance[citypanel2$msec==people[i]]<-as.numeric(any(dt$lv1job %in%c("财政部/厅/局","发展改革/计划委员会","地方税务局")))
  

  
  ############ First year as city leader ###################

  citypanel2$msec.firstyearml[citypanel2$msec==people[i]]<-min(year(d$begin[which(d$name==people[i] &d$keypost%in%c("市长","市委书记"))]),na.rm=T)
  
  citypanel2$msec.firstYMml[citypanel2$msec==people[i]]<-min(d$begin[which(d$name==people[i] & d$keypost%in%c("市长","市委书记"))],na.rm=T)
  
}


###########-------------------Mayor Loop -------------------##############

people<-na.omit(unique(citypanel2$mayor))

for(i in 1:length(people)){

  
  ###################### Code Prior experience ########################
  before<-min(d[which(d$name==people[i] & d$keypost%in%c("市长","市委书记")),"begin"])
  dt<-d[which(d$name==people[i] & d$end<=before),]
  ############### EDUCATION CREDENTIAL #################
  
  dedu<-dt[which(dt$name==people[i] & dt$keypost=="学校" & dt$edu%in%c("本科","硕士","博士")),"begin"]
  if(length(dedu)==0){
    actualcollege<-0
    yrofcollege<-NA
  } else{
    actualcollege<-as.numeric(min(year(dedu))-year(db[db$name==people[i],"dob"])<30)
    yrofcollege<-min(year(dedu))
  }
  
  citypanel2$mayor.edu[citypanel2$mayor==people[i]]<-actualcollege 
  citypanel2$mayor.edu.year[citypanel2$mayor==people[i]]<-yrofcollege
  
  ############## BUSINESS MANAGEMENT EXPERIENCE ###################
  
  dsoe<-dt[which(dt$name==people[i] & dt$basecat%in%c("地方企业","中央企业")),]
  citypanel2$mayor.soe.exp[citypanel2$mayor==people[i]]<-as.numeric(length(grep("经理|董事长|厂长|总裁|历任数职|负责人|行长",dsoe$exactjob))>0)
  
  
  ############## UNIVERSITY MANAGEMENT ###################
  duniv<-dt[dt$basecat=="学校",]
  duniv<-duniv[-grep("党校",duniv$lv1job),]
  citypanel2$mayor.univ.exp[citypanel2$mayor==people[i]]<-as.numeric(length(intersect(grep("主任|校长|教授|院长|书记",duniv$exactjob),which(duniv$rank>=1.5)))>0)
  
  ################### YOUTH LEAGUE #######################
  citypanel2$mayor.league[citypanel2$mayor==people[i]]<-as.numeric(any(dt$basecat=="共青团" & dt$rank>=1))

  ################### COUNTY LEADERSHIP EXPERIENCE #################
  citypanel2$mayor.county.exp[citypanel2$mayor==people[i]]<-as.numeric(any(dt$county!="" & dt$rank<2 & dt$rank>0 & dt$basecat %in%c("党委","政府_国务院")))

  
  ################### FINANCE  EXPERIENCE ################
  citypanel2$mayor.finance[citypanel2$mayor==people[i]]<-as.numeric(any(dt$lv1job %in%c("财政部/厅/局","发展改革/计划委员会","地方税务局")))
  


  
  ################## First year to city leader ############
  
  citypanel2$mayor.firstyearml[citypanel2$mayor==people[i]]<-min(year(d$begin[which(d$name==people[i] & d$keypost%in%c("市长","市委书记"))]),na.rm=T)

  citypanel2$mayor.firstYMml[citypanel2$mayor==people[i]]<-min(d$begin[which(d$name==people[i] & d$keypost%in%c("市长","市委书记"))],na.rm=T)
  
  
}



###################################################################################################
#########################                                      ####################################
#########################          COMBINE AND OUTPUT          ####################################
#########################                                      ####################################
###################################################################################################

X<-citypanel2[order(citypanel2$cityid,citypanel2$year),]
X$order<-seq_along(X$cityid)

db$birthyear<-year(db$dob)
db$highest.edu<-ifelse(db$highest.edu=="",NA,db$highest.edu)
db$ethnicity<-ifelse(db$ethnicity=="",NA,db$ethnicity)


msecbase<-mayorbase<-db
msecbase<-msecbase[,c("name","sex","ethnicity","birthyear","province","city","dob","highest.edu","party","status")]
names(msecbase)<-paste("msec",c("name","sex","ethnicity","birthyear","province","city","dob","highest.edu","party","status"),sep=".")

mayorbase<-mayorbase[,c("name","sex","ethnicity","birthyear","province","city","dob","highest.edu","party","status")]
names(mayorbase)<-paste("mayor",c("name","sex","ethnicity","birthyear","province","city","dob","highest.edu","party","status"),sep=".")




X2<-merge(merge(X,msecbase,by.x="msec",by.y="msec.name",all.x=T,incomparables=NA),mayorbase,by.x="mayor",by.y="mayor.name",all.x=T,incomparables=NA)
X2<-facTOchar(X2,"c2f",NA)
X2<-X2[order(X2$cityid,X2$year),]

X2$minor.region<-as.numeric(X2$provid%in%c(150000,450000,540000,640000,650000))
X2$western<-as.numeric(X2$provid%in%c(540000,630000,640000,650000))
X2$east.region<-as.numeric(X2$provid %in% c(370000,130000,320000,350000,440000,330000,460000))
X2$auto.city<-as.numeric(X2$cname %in% grep("自治州|盟",X2$cname,value=T))
X2$farwest<-as.numeric(X2$provid %in% c(540000,650000,630000))


lead<-c("cityid","cname","year","msec","mayor","province","provid","current.psec","current.gvn")
X2<-X2[,c(lead,setdiff(names(X2),lead))]
names(X2)<-gsub("\\.","_",names(X2))
X2$psec_arrival_aspsec_YM<-as.numeric(X2$psec_arrival_aspsec_YM) 




write.csv(X2,"citypanel_base.csv",row.names=F,na="",fileEncoding = "UTF-8")

