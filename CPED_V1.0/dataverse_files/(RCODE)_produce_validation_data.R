###########################################################################
###########################################################################
### SCRIPT TO PRODUCE PERSON-PANEL DATA FROM CPED FOR VALIDATION TESTS ####
###########################################################################
###########################################################################
###########################################################################

# Author: Junyan Jiang (Asst. Prof, Dept of Government and Public Administration, Chinese University of Hong Kong)
# Version: 1.0
# Tested under:
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32   



##### NOTE: BEFORE RUNNING THIS, PLEASE ENSURE YOUR COMPUTER SUPPORTS SIMPLIFIED CHINESE ############


## Set the working directory to the folder that contains the replication files ##

setwd("C:/Users/junya/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")

#setwd("D:/Users/jiang/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")

Sys.setlocale(,"CHS")
# source("https://bioconductor.org/biocLite.R")
# biocLite("IRanges")
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



##################################################################################
##################------ Load and Clean Data ----------#######################
##################################################################################

#读取人员经历信息
d<-read.csv("fullbio1106.csv",stringsAsFactors=F)

names(d)<-c("ID","name","expid","begin","end","natorg","centorg","provid","province","cityid","city","comment1","cityrank","county","basecat","jobid","lv1job","comment2","lv2job","lv3job","exactjob","rank","keypost","edu","order")
d$begin<-as.Date(d$begin,format='%m/%d/%Y')
d$end<-as.Date(d$end,format='%m/%d/%Y')

rankname<-unique(d$rank)
ranknum<-c(0,0.5,1,1.5,2,2.5,0,3,3.5,4)
sapply(1:length(rankname),function(x) d$rank[d$rank==rankname[x]]<<-ranknum[x])
d$rank<-as.numeric(d$rank)



#读取人员基本信息
db<-read.csv("base1106.csv",stringsAsFactors=F,na.string="")


names(db)<-c("ID","name","sex","ethnicity","dob","province","city","county","highest.edu","party","status","expel","purge.cause","purge.cause.comment","purge.back","purge.time","purge.agency","prosecuted","charges","charge.money","trial1.time","trial1.province","trial1.city","trial1.result","appeal","trial2.time","trial2.result")
db$dob<-as.Date(db$dob,format='%m/%d/%Y')
db$party<-as.Date(db$party,format='%m/%d/%Y')
db$purge.time<-as.Date(db$purge.time,format='%m/%d/%Y')
db$trial1.time<-as.Date(db$trial1.time,format='%m/%d/%Y')
db$trial2.time<-as.Date(db$trial2.time,format='%m/%d/%Y')


# Add new provincial bureau keypost
d[which(d$city=="" & d$county=="" & d$provid<=650000 & d$keypost=="无" & d$natorg=="否" & d$centorg=="否" & d$rank>=2 & d$basecat%in% c("政府_国务院","党委","法院_检察院")),"keypost"]<-"省机关领导"




### Load Core Functions for Overlap Analysis ###


source("(FUN) corefunctions.R",encoding="utf-8")

source("(FUN)_datatool.R",encoding="utf-8")

pvmat<-read.csv("pvmat.csv",stringsAsFactors=F)



##################################################################################
##################------ Construct the Person-Year Panel ----------###############
##################################################################################



## Identify all keyposts ##
mlpost<-c("市长","市委书记")
plpost<-c("省长","省委书记")
vppost<-setdiff(grep("省",unique(d$keypost),value=T),c(plpost,"副省长（非常委）"))
centpost<-c("政治局委员","政治局常委")


## Sample restrictions: people who have held key party or government posts between 2000 and 2012 ##

person<-na.omit(unique(d$name[which(d$keypost %in% c(mlpost,plpost,vppost,centpost) & d$end>="2000-01-01" & d$begin<="2012-12-31")]))

dtlist<-list()
data<-d[-grep("学员|学生",d$exactjob),]
for(i in 1:length(person)){
  cat(person[i],"\n")
  #create personal dataset
  dt<-data[which(data$name==person[i] & is.na(data$begin)==F & is.na(data$end)==F),]
  
  #Check for early educational attainment
  dedu<-d[which(d$name==person[i] & d$keypost=="学校" & d$edu%in%c("本科","硕士","博士")),"begin"]
  if(length(dedu)==0){
    actualcollege<-0
  } else{
    actualcollege<-as.numeric(year(dedu[1])-year(db[db$name==person[i],"dob"])<30)
  }

  actualcollege<-ifelse(is.na(actualcollege),0,actualcollege)
  
  #obtain the first year as leader
  note<-"m"
  if(any(dt$keypost%in%c("市长","市委书记") & dt$end>="2000-01-01",na.rm=T)){
    firstleader<-min(year(dt[which(dt$keypost%in%c("市长","市委书记")),"begin"]),na.rm=T)
  }else{
    # provincial leader
    if(any(dt$keypost %in% plpost & dt$end>="2000-01-01")){
      firstleader<-min(year(dt[which(dt$keypost%in%plpost),"begin"]),na.rm=T)
      note<-"p"
    }else{
      if(any(dt$keypost %in% vppost & dt$end>="2000-01-01")){
        firstleader<-min(year(dt[which(dt$keypost%in%vppost),"begin"]),na.rm=T) 
        
        note<-"vp"
        
      }else{
        if(any(dt$keypost %in% centpost & dt$end>="2000-01-01")){
          firstleader<-min(year(dt[which(dt$keypost%in%centpost),"begin"]),na.rm=T) 
          note<-"c"
        }else{
          next
        }

  }
  }
  }

  #Total number of records
  totrecord<-nrow(dt)
  
  
  #first and last year of the personal data
  if(note=="m"|note=="vp"){
    ybeg<-max(firstleader,2000)
  }else{
    if(note=="p"){
      ybeg<-max(firstleader,2000)
    } else{
      ybeg<-max(firstleader,2000)
    }
  }
  
  yend<-2015
  
  #basic structure of personal panel
  pn<-data.frame(name=person[i],year=seq(ybeg,yend,1))
  pn$leadertype<-note
  pn$firstleader<-firstleader
  pn$league<-as.numeric(any(dt$basecat=="共青团" & dt$rank>=2))
  pn$actualcollege<-actualcollege
  pn$totrecord<-totrecord

  # Obtain basic info record
  baseinfo<-db[which(db$name==person[i]),]
  # Obtain retirement year
  if(baseinfo$status=="在职"){
    retyear<-2020
  }else{
    retyear<-max(year(dt$end),na.rm=T)
  } 
  ############ Fill in Different Status ###############
  #--------- Purged -----------#
  if(baseinfo$status%in%c("立案查处","降/辞/撤职")){

    # All purge considered
    if(is.na(baseinfo$purge.time)==F){
      purgeyear<-year(baseinfo$purge.time)
    }else{
      purgeyear<-max(year(d$end[which(d$name==person[i])]))
    }
    # Only endogenous investigations
    if(!baseinfo$purge.cause%in%c("责任事故","群体性事件","生活作风问题")){
      
      # if purge time exists, use the purge time
      if(is.na(baseinfo$purge.time)==F){
        acyear<-year(baseinfo$purge.time)
      }else{
        #other wise, use the last year of career record
        acyear<-max(year(d$end[which(d$name==person[i])]),na.rm=T)
      }

    }else{
      acyear<-2020
    }
    }else{
    purgeyear<-2020
    acyear<-2020
  }
  

  pn$purged<-pn$anticorruption<-pn$ac.central<-pn$ac.local<-0
  if(any(d$name==person[i] & d$keypost%in%c("政治局委员","政治局常委"))){
    pn$firstpb<-min(year(d$begin[which(d$name==person[i] & d$keypost%in%c("政治局委员","政治局常委"))]),na.rm=T)
  }else{
    pn$firstpb<-9999
  }

  #fill in career information  
  for(j in 1:nrow(pn)){
    start<-as.Date(paste(pn$year[j],"-01-01",sep=""),format='%Y-%m-%d')
    end<-as.Date(paste(pn$year[j],"-12-31",sep=""),format='%Y-%m-%d')
  
    if(pn[j,"year"]==purgeyear){
      pn$purged[j]<-1
    }
    if(pn[j,"year"]==acyear){
      pn$anticorruption[j]<-1
      pn$ac.central[j]<-length(grep("中纪委|最高人民检察院|中央",baseinfo$purge.agency,value=T))
      pn$ac.local[j]<- 1-pn$ac.central[j]

    }
    if(pn[j,"year"]<=retyear){
      
      #Rank: maximum rank during this period
      if(any(dt$begin<=end & dt$end>=start)==F){
        pn[j,"rank"]<-pn[j-1,"rank"]
      }else{
        pn[j,"rank"]<-max(dt[which(dt$begin<=end & dt$end>=start),"rank"])
      }
      
      # Temporary person data (given rank)
      dtemp<-dt[which(dt$begin<=end & dt$end>=start & dt$rank==pn[j,"rank"]),]
      
      
      keylist<-c("学校","军队","无","秘书","副市长（非常委）","市常委（其他）","副市长（常委）","市组织部长","市委副书记（专职）","市长","市委书记","副省长（非常委）","省常委（其他）","副省长（常委）","省组织部长","省委副书记（非省长）","省长","省委书记","政治局委员","政治局常委")
      
      for(k in 1:length(keylist)){    
        if(length(unique(dtemp$keypost))>1){
          if(is.element(keylist[k],dtemp$keypost)==T){
            dtemp<-dtemp[-which(dtemp$keypost==keylist[k]),]
          }
        }else{
          break
        }
      }
      #选择结束时间最晚的经历
      dtemp<-dtemp[which.max(dtemp$end),]
      

        pn[j,"job"]<-paste(apply(dtemp[,c("lv1job","lv2job","lv3job","exactjob")],1,paste,collapse=""),collapse="#")
        pn[j,"jobcat"]<-paste(dtemp[,"basecat"],collapse="#")
        pn[j,"keypost"]<-paste(unique(dtemp$keypost),collapse="#")
        
        #Fill in province and city (if no record, use the previous year's)
        pn[j,"province"]<-ifelse(length(unique(dtemp[,"province"]))==0,pn[j-1,"province"],unique(dtemp[,"province"]))
        pn[j,"city"]<-ifelse(length(unique(dtemp[,"city"]))==0,pn[j-1,"city"],unique(dtemp[,"city"]))


    }else{
      #When hit retirement age
      pn[j,"jobcat"]<-pn[j,"job"]<-pn[j,"keypost"]<-"退休"
      pn[j,"rank"]<-pn[j-1,"rank"]
    
    }

    
  }
  if(note=="m"){
    pn$firstcity<-pn[1,"city"]
  }else{
    if(note%in%c("p","vp")){
      pn$firstcity<-pn[1,"province"]
    }else{
      pn$firstcity<-"中央"
    }
  }
  pn$firstprovince<-pn[1,"province"]
  pn$firstkypost<-pn[1,"keypost"]
  pn$firstrank<-pn[1,"rank"]
  pn$expid<-seq_along(rownames(pn))
  dtlist[[i]]<-pn
  
}


# Remove post-mortem exp of dead people
deadppl<-db$name[which(db$status=="死亡")]
dtlist2<-lapply(dtlist,function(x){
  cat(as.character(unique(x[,"name"])),"\n")
  if(is.null(x)==F){
    x[,"dead"]<-0
  if(as.character(unique(x[,"name"]) %in% deadppl)){
    deadyear<-max(year(d$end[which(d$name==as.character(unique(x[,"name"])))]),na.rm=T)
    x<-x[which(x[,"year"]<=deadyear),]
    x[nrow(x),"dead"]<-1
  }
  return(x)
  }else{
    return(x)
  }
}
)
z<-as.data.frame(rbindlist(dtlist2))



########### Fill in information of the provincial leader ##########
prov<-unique(d[d$provid%in%pvmat$pid,"province"])
year<-unique(z$year)
for(i in 1:length(year)){
  for(j in 1:length(prov)){
    p<-prov[j]
    y<-year[i]
    ybeg<-as.Date(paste(y,"-01-01",sep=""),format='%Y-%m-%d')
    
    #yend<-as.Date(paste(y,"-12-31",sep=""),format='%Y-%m-%d')
    
      currentgvn<-d$name[which(d$province==p & d$begin<=ybeg & d$end>ybeg & d$keypost=="省长")]

   
      currentpsec<-d$name[which(d$province==p & d$begin<=ybeg & d$end>ybeg & d$keypost=="省委书记")]

    
    #former secretary
    
    dpsec<-d[which(d$province==p & d$keypost=="省委书记"),]
    dpsec<-dpsec[order(dpsec$begin),]
    formerpsec<-dpsec$name[which(dpsec$name==currentpsec)-1]
    # former governor
    dgvn<-d[which(d$province==p & d$keypost=="省长"),]
    dgvn<-dgvn[order(dgvn$begin),]
    formergvn<-dgvn$name[which(dgvn$name==currentgvn)-1]
    
    z[which(z$province==p & z$year==y),"current.gvn"]<-ifelse(length(currentgvn)==0,NA,currentgvn)
    z[which(z$province==p & z$year==y),"current.psec"]<-ifelse(length(currentpsec)==0,NA,currentpsec)
    z[which(z$province==p & z$year==y),"former.psec"]<-ifelse(length(formerpsec)==0,NA,formerpsec)
    z[which(z$province==p & z$year==y),"former.gvn"]<-ifelse(length(formergvn)==0,NA,formergvn)

  }
}


#################### Defining Promotions ###################

### Definition: 提拔至副部级实权部门任职  ####
z$real.power<-0

z$real.power[grep("政府_国务院|党委|中央企业|共青团|人民法院|人民检察院",z$jobcat)]<-1

z$vp.realpower<-as.numeric(z$real.power==1 & z$rank>=2.5)
z$pro2vp.trueprize<-as.numeric(unlist(tapply(z$vp.realpower, z$name, function(x) cumsum(cumsum(x))))==1 & z$firstrank<=2)




####--------- List of Central Leaders -----------####
# Identify Central Leaders
pbkw<-grep("政治局",unique(d$keypost),value=T)

#SC and PB members
years<-seq(1992,2015)
sc.leader<-list()
pb.leader<-list()

targeted<-list()
##### Create a list of fallen officials and exclude them #####
fall<-list()
for(i in 1:length(years)){
  fall[[i]]<-"无"
  if(years[i]>=1997) fall[[i]]<-'陈希同'
  if(years[i]>=2006 & years[i]<2012) fall[[i]]<-'陈良宇'
  if(years[i]>=2012) fall[[i]]<-c("薄熙来","令计划","李源潮","周永康")
}
for(i in 1:length(years)){
  begin<-as.Date(paste(years[i],"-01-01",sep=""),format="%Y-%m-%d")
  sc.leader[[i]]<-setdiff(unique(d[d$keypost%in%"政治局常委" & d$begin<=begin & d$end>=begin,"name"]),fall[[i]])
  pb.leader[[i]]<-setdiff(unique(d[d$keypost%in%"政治局委员" & d$begin<=begin & d$end>=begin,"name"]),fall[[i]])

}

# gs<-as.list(as.data.frame(t(read.csv("top4.csv")[2]),stringsAsFactors=F))
# top2<-as.list(as.data.frame(t(read.csv("top4.csv")[2:3]),stringsAsFactors=F))
# top3<-as.list(as.data.frame(t(read.csv("top4.csv")[2:4]),stringsAsFactors=F))
# top4<-as.list(as.data.frame(t(read.csv("top4.csv")[2:5]),stringsAsFactors=F))


names(sc.leader)<-names(pb.leader)<-names(fall)<-years



#########################################################################
##################------ Promotion Ties ----------#######################
#########################################################################

# To Vice bureau positions
promunder.2vb<-idGroup(client=unique(as.character(z$name)),type="2vb",data=d)
names(promunder.2vb)<-c("name",paste(c("patron1","patron2","promo.year","promo.place"),"2vb",sep="."))


# To Bureau positions
promunder.2bureau<-idGroup(client=unique(as.character(z$name)),type="2bureau",data=d)
names(promunder.2bureau)<-c("name",paste(c("patron1","patron2","promo.year","promo.place"),"2bureau",sep="."))


# To all positions, secretary or mayor, for the first time
promunder.2all<-idGroup(client=unique(as.character(z$name)),type="2all",data=d,max.rank=2.5)
names(promunder.2all)<-c("name",paste(c("patron1","patron2","promo.year","promo.place"),"2all",sep="."))

# To secretary position
promunder.2sec<-idGroup(client=unique(as.character(z$name)),type="2sec",data=d,max.rank=2.5)
names(promunder.2sec)<-c("name",paste(c("patron1","patron2","promo.year","promo.place"),"2sec",sep="."))


promo.merged<-merge_recurse(list(promunder.2vb, promunder.2bureau, promunder.2all, promunder.2sec),by="name")


pd<-merge(z,promo.merged,by="name",all.x=T)

###############################################################################
##################------Overlap-based Measure ----------#######################
###############################################################################



explist<-expand.grid(person,unique(d[d$keypost%in%pbkw & d$end>="2000-01-01","name"]),stringsAsFactors=F)
explist<-explist[-which(explist[,1]==explist[,2]),]

#---- Work overlap between municipal and central leaders ----#
# This may take about 10-15 mins...
edge.work<-Overlap.group(client=explist[,1],patron=explist[,2],type="work",level="province",minduration=90,rankdiff=c(0,2),data=d,dbase=db,noisily=T)


#---- Home overlap between municipal and central leaders ----#
edge.home<-Overlap.group(client=explist[,1],patron=explist[,2],type="home",level="province",data=d,dbase=db,noisily=F)

#---- School overlap between municipal and central leader ----#
edge.school<-Overlap.group(client=explist[,1],patron=explist[,2],type="school",level="city",edulevel="college",data=d,dbase=db,noisily=F)

#---- Merge ---#
edge.cb<-merge(merge(edge.work,edge.home,by=c("client","patron")),edge.school,by=c("client","patron"))


###################################################################################
#------ Calculate Total Number of Connections to Central Leaders per Person-Year ---------#
###################################################################################


#-----------------------------------------------------------------------------#
#NOTE: Sitting provincial secretaries are excluded from PB member calculations#
#-----------------------------------------------------------------------------#


overlap.ties<-list()

for(i in 1:nrow(pd)){
  
  cat(as.character(pd$name[i]),"---",pd$year[i],"\n")
  # Set connection time to BEFORE entry to the PB
  begin<-as.Date(paste(min(pd$year[i],pd$firstpb[i]),"-01-01",sep=""))
  
  # record overlapped individuals
  overlap.ties[[i]]<-c(unique(edge.cb[which(edge.cb$client==pd$name[i] & (edge.cb$work.op==1) & edge.cb$client.rank<=pd[i,"rank"] & edge.cb$work.start<=begin),"patron"]),"Unknown")
  
}


pd[,c("conn2zhouNother","conn2zhouNother.op")]<-0

years<-2000:2015
for(i in 1:length(years)){
  
  dtemp<-pd[which(pd$year==years[i]),]
  
  op.tie<-overlap.ties[which(pd$year==years[i])]

  inc.sc<-sc.leader[[as.character(years[i])]]
  inc.pb<-pb.leader[[as.character(years[i])]]

  
  
  cat("year---",years[i],"\n")
  
  ###### New way of calculating promotion ties ####
  
  # Type 1:  b ml and sec
  promoties1<-as.list(as.data.frame(t(dtemp[,c("patron1.2bureau","patron1.2all","patron1.2sec")]),stringsAsFactors=F))
  
  # Type 2: vb, b, ml, and sec
  promoties2<-as.list(as.data.frame(t(dtemp[,c("patron1.2vb","patron1.2bureau","patron1.2all","patron1.2sec")]),stringsAsFactors=F))


  #####################################
  #-------- Connection to PB ---------#
  #####################################
  pd[which(pd$year==years[i]),"conn2currentpb1"]<-sapply(promoties1, function(x) sum(unique(x) %in% inc.pb))
  #pd[which(pd$year==years[i]),"conn2currentpb2"]<-sapply(promoties2, function(x) sum(unique(x) %in% inc.pb))

  # overlap: work
  pd[which(pd$year==years[i]),"conn2currentpb.op"]<-sapply(op.tie,function(x) sum(unique(x)%in%inc.pb))
  # overlap: all
  #pd[which(pd$year==years[i]),"conn2currentpb.all"]<-sapply(op.tie.all,function(x) sum(unique(x)%in%inc.pb))
  
  
  #####################################
  #--------- Connection to SC --------#
  #####################################
  pd[which(pd$year==years[i]),"conn2currentsc1"]<-sapply(promoties1,function(x) sum(unique(x) %in% inc.sc))
  #pd[which(pd$year==years[i]),"conn2currentsc2"]<-sapply(promoties2,function(x) sum(unique(x) %in% inc.sc))

  # overlap: work
  pd[which(pd$year==years[i]),"conn2currentsc.op"]<-sapply(op.tie,function(x) sum(unique(x)%in%inc.sc))

  
  #---------- To High-level anticorruption targets ---------#  
  
  pd[which(pd$year==years[i]),"conn2zhouNother"]<-sapply(promoties2,function(x) sum(unique(x) %in% c("周永康","令计划","薄熙来")))
  
  pd[which(pd$year==years[i]),"conn2zhouNother.op"]<-sapply(op.tie,function(x) sum(unique(x) %in% c("周永康","令计划","薄熙来")))
  
  pd[which(pd$year==years[i]),"conn2chen"]<-sapply(promoties1,function(x) sum(unique(x) %in% c("陈良宇")))
  
  pd[which(pd$year==years[i]),"conn2chen.op"]<-sapply(op.tie,function(x) sum(unique(x) %in% c("陈良宇")))
  
  pd[which(pd$year==years[i]),"conn2chenxitong2"]<-sapply(promoties1,function(x) sum(unique(x) %in% c("陈希同")))

  
  
 

}


###############################################
####-----Merge with the Base Info Data-----####
###############################################

db2<-db
names(db2)<-c("ID","name","sex","ethnicity","dob","birth.province","birth.city","birth.county","highest.edu","party","status","expel","purge.cause","purge.cause.comment","purge.back","purge.time","purge.agency","prosecuted","charges","charge.money","trial1.time","trial1.province","trial1.city","trial1.result","appeal","trial2.time","trial2.result")


pd2<-merge(pd,db2[,c("name","sex","ethnicity","dob","birth.province","birth.city","birth.county","highest.edu","party","status")],by.x="name",by.y="name")


pd2$age<-as.numeric(as.character(pd2$year))-year(pd2$dob)

pd2$firstcity<-gsub("^$","无",pd2$firstcity)
pd2$city<-gsub("^$","无",pd2$city)
pd2$birth.province<-gsub("^$","无",pd2$birth.province)
pd2$birth.city<-gsub("^$","无",pd2$birth.city)
pd2$birth.county<-gsub("^$","无",pd2$birth.county)
pd2$ethnicity<-gsub("^$","汉族",pd2$ethnicity)
pd2$highest.edu[is.na(pd2$highest.edu)]<-"不详"
pd2$highest.edu<-factor(gsub("^$","不详",pd2$highest.edu),levels=c("不详","初中","中专","高中","专科","本科","硕士","博士","博士后"))

pd2$leadertype<-factor(pd2$leadertype,levels=c("m","vp","p","c"))
pd2$partyage<-year(pd2$party)-year(pd2$dob)

pd2$conn2targets<-as.numeric((pd2$conn2zhouNother==1 & pd2$year>=2012)|
                               (pd2$conn2chen==1 & pd2$year %in% c(2006:2009))|
                               (pd2$birth.city%in%"运城市" & pd2$year>=2012) # Ling's hometown
                             )




pd2$conn2targets.op<-as.numeric((pd2$conn2zhouNother.op>=1 & pd2$year>=2012)|
                                (pd2$conn2chen.op==1 & pd2$year %in% c(2006:2009))|
                                (pd2$birth.city%in%"运城市" & pd2$year>=2012) # Ling's hometown
                                )





pd2$leadertype<-factor(pd2$leadertype,levels=c("m","vp","p","c"))
pd2$firstprovince<-factor(pd2$firstprovince)
names(pd2)<-gsub("\\.","_",names(pd2))


write.csv(pd2,"validation_data.csv",na="",fileEncoding = "UTF-8",row.names=F)

