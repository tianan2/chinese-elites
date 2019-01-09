###########################################################################
###########################################################################
### SCRIPT TO EXAMINE THE RELATIONSHIP BETWEEN PERFORMANCE AND PROMOTION  ####
###########################################################################
###########################################################################
###########################################################################

# Author: Junyan Jiang (Asst Prof, Dept of Government and Public Administration, Chinese University of Hong Kong)
# Version: 1.0
# Tested under:
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32   


##### NOTE: BEFORE RUNNING THIS, PLEASE ENSURE YOUR COMPUTER SUPPORTS SIMPLIFIED CHINESE ############


## Set the working directory to the folder that contains the replication files ##

setwd("C:/Users/junya/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")

#setwd("D:/Users/jiang/Dropbox/Academic/Research/Dissertation/Chapters/[A] Patronage and Development/Draft/Submissions/AJPS Final Draft/Replication files")

Sys.setlocale(,"CHS")

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



########################################################################
##################------ Load the Data ----------#######################
########################################################################


#读取人员经历信息
d<-read.csv("fullbio1106.csv",stringsAsFactors=F)
names(d)<-c("ID","name","expid","begin","end","natorg","centorg","provid","province","cityid","city","comment1","cityrank","county","basecat","jobid","lv1job","comment2","lv2job","lv3job","exactjob","rank","keypost","edu")
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

source("(FUN) corefunctions.R",encoding="utf-8")

source("(FUN)_datatool.R",encoding="utf-8")

pvmat<-read.csv("pvmat.csv",stringsAsFactors=F)


################################################################################################
################################################################################################
################## Construct Person-Year Panel with Connection Weighted By Residuals############
################################################################################################
################################################################################################

X2<-read.dta("citypanel_forR.dta",convert.underscore=T)
X2<-X2[-which(X2$year==1999 | X2$year>2013),]

####--------- Municipal Leaders -----------####
## Sample: all officials who have served as city leaders between 2000 and 2012.
mlpost<-c("市长","市委书记")
person<-na.omit(unique(c(as.character(X2$msec[which(X2$msec.firstrank==2)]),as.character(X2$mayor[which(X2$mayor.firstrank==2)]))))

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
  
  ######## Obtain First Year As Leader and Other Related Information  #######
  mldate<-min(X2[which(X2$msec==person[i]|X2$mayor==person[i]),"year"])
  

  lastml<-year(max(dt[which(dt$keypost%in%c("市长","市委书记")),"end"],na.rm=T))
  
  ### 2000-01-01 =  10975
  ml.cutyear1<-as.Date(paste(mldate,"-01-01",sep=""),origin="1970-01-01")
  ml.cutyear2<-as.Date(paste(mldate,"-12-31",sep=""),origin="1970-01-01")
  
  
  ml.keypost<-dt[which(dt$begin<=ml.cutyear2 & dt$end>=ml.cutyear1 & dt$keypost %in% mlpost),"keypost"]
  
  if(length(ml.keypost)>1) ml.keypost<-"市委书记"
  


  
  #Total number of records
  totrecord<-nrow(dt)
  
  
  ybeg<-max(mldate)
  yend<-min(year(max(dt$end,na.rm=T)),2013)
  
  
  #basic structure of personal panel
  pn<-data.frame(name=person[i],year=seq(ybeg,yend,1))
  pn$leadertype<-ml.keypost
  pn$league<-as.numeric(any(dt$basecat=="共青团" & dt$rank>=2))
  pn$centralleague<-as.numeric(any(dt$basecat=="共青团" & dt$province=="中央" & dt$rank>=2))
  pn$actualcollege<-actualcollege
  pn$totrecord<-totrecord
  
  # Year attaining corresponding ranks (regardless of positions) #
  pn$firstbureau<-ifelse(any(dt$rank>=2),year(min(dt$begin[which(dt$rank>=2)])),9999)
  
  
  
  # Obtain basic info record
  baseinfo<-db[which(db$name==person[i]),]
  # Obtain retirement year
  
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
    dtemp<-dtemp[which(dtemp$end==max(dtemp$end)),]
    
    pn[j,"job"]<-paste(apply(dtemp[,c("lv1job","lv2job","lv3job","exactjob")],1,paste,collapse=""),collapse="#")
    pn[j,"lv1job"]<-paste(dtemp[,"lv1job"],collapse="#")
    pn[j,"jobcat"]<-paste(dtemp[,"basecat"],collapse="#")
    pn[j,"keypost"]<-paste(unique(dtemp$keypost),collapse="#")
    
    #Fill in province and city (if no record, use the previous year's)
    pn[j,"province"]<-ifelse(length(unique(dtemp[,"province"]))==0,pn[j-1,"province"],unique(dtemp[,"province"]))
    pn[j,"city"]<-ifelse(length(unique(dtemp[,"city"]))==0,pn[j-1,"city"],unique(dtemp[,"city"]))
    pn[j,"cityid"]<-ifelse(length(unique(dtemp[,"city"]))==0,pn[j-1,"cityid"],unique(dtemp[,"cityid"]))
    
    
    if(pn[j,"job"]==""){
      pn[j,c("job","lv1job","jobcat","keypost","province","city","rank")]<-pn[(j-1),c("job","lv1job","jobcat","keypost","province","city","rank")]
    }
    
    
  }
  pn$firstcity<-pn[1,"city"]
  pn$firstprovince<-pn[1,"province"]
  pn$expid<-seq_along(rownames(pn))
  pn$postml<-as.numeric(pn$year>lastml)
  
  dtlist[[i]]<-pn
  
}



##### Remove post-mortem exp of dead people
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
z$cityid[which(is.na(z$cityid))]<-999999



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
    
    
    z[which(z$province==p & z$year==y),"current.gvn"]<-ifelse(length(currentgvn)==0,NA,currentgvn)
    z[which(z$province==p & z$year==y),"current.psec"]<-ifelse(length(currentpsec)==0,NA,currentpsec)

    
  }
}


#################### Define Promotion ###################

### Definition: 提拔至中央部委或副省级实权部门 ####
z$real.power<-0

z$real.power[grep("政府_国务院|党委|中央企业|共青团|人民法院|人民检察院",z$jobcat)]<-1

z$vp.realpower<-as.numeric(z$real.power==1 & z$rank>=2.5)
z$pro2vp.trueprize<-as.numeric(unlist(tapply(z$vp.realpower, z$name, function(x) cumsum(cumsum(x))))==1)




##############################################################
####--------- Create a List of Central Leaders -----------####
##############################################################

# Identify Central Leaders
pbkw<-grep("政治局",unique(d$keypost),value=T)

#SC and PB members
years<-seq(1992,2015)
sc.leader<-list()
pb.leader<-list()
##### Create the Fallen List to Exclude #####
fall<-list()

for(i in 1:length(years)){
  fall[[i]]<-"无"
  if(years[i]>=1997) fall[[i]]<-'陈希同'
  if(years[i]>=2006 & years[i]<2012) fall[[i]]<-'陈良宇'
  if(years[i]>=2012) fall[[i]]<-c("薄熙来","令计划","李源潮","周永康")
  if(years[i]>=2007) fall[[i]]<-'黄菊'
  if(years[i]>=2013) fall[[i]]<-'周永康'
}

for(i in 1:length(years)){
  begin<-as.Date(paste(years[i],"-01-01",sep=""),format="%Y-%m-%d")
  sc.leader[[i]]<-setdiff(unique(d[d$keypost%in%"政治局常委" & d$begin<=begin & d$end>=begin,"name"]),fall[[i]])
  pb.leader[[i]]<-setdiff(unique(d[d$keypost%in%"政治局委员" & d$begin<=begin & d$end>=begin,"name"]),fall[[i]])
}

names(sc.leader)<-names(pb.leader)<-years


#########################################################################
##################------ Promotion Ties ----------#######################
#########################################################################

# To all positions, secretary or mayor, for the first time
promunder.2all<-idGroup(client=unique(as.character(z$name)),type="2all",data=d,max.rank=2.5)
names(promunder.2all)<-c("name",paste(c("patron1","patron2","promo.year","promo.place"),"2all",sep="."))

# To secretary positions
promunder.2sec<-idGroup(client=unique(as.character(z$name)),type="2sec",data=d,max.rank=2.5)
names(promunder.2sec)<-c("name",paste(c("patron1","patron2","promo.year","promo.place"),"2sec",sep="."))



# write.csv(merge(promunder,promunder.2sec,by="name"),"mleaderPT.csv",row.names=F)
promo.merged<-merge(promunder.2all, promunder.2sec,by="name")


pd<-merge(z,promo.merged,by="name",all.x=T)

pd<-merge(pd,pvmat,by.x=c("province","year"),by.y=c("pname","year"),all.x=T)


############################################################################
###################### Add Performance Data ################################
############################################################################

pd<-pd[order(pd$name,pd$year),]

pd$name<-as.character(pd$name)

years<-seq(2000,2013)
for(i in 1:length(years)){
  dtemp<-pd[which(pd$year==years[i]),]
  
  inc.sc<-sc.leader[[as.character(years[i])]]
  inc.pb<-pb.leader[[as.character(years[i])]]

  
  cat("year---",years[i],"\n")

  promoties<-lapply(as.list(as.data.frame(t(dtemp[,c("patron1.2all","patron2.2all","patron1.2sec","patron2.2sec")]),stringsAsFactors=F)),unique)

  # Group rows by year for looping
  id<-which(pd$year==years[i])

  # Connection to PB (unweighted)
  pd[id,"conn2currentpb"]<-mapply(function(x) sum(x %in% inc.pb),x=promoties)

  # Connection to SC (unweighted)
  pd[id,"conn2currentsc"]<-mapply(function(x) sum(x %in% inc.sc),x=promoties)
  
  # Connection to SC and PB (unweighted)
  pd[id,"conn2currenttop"]<-mapply(function(x) sum(x %in% c(inc.sc,inc.pb)),x=promoties)

}



########################################################################## 
############### Connection Weighted by Performance #######################
########################################################################## 


vars<-grep("pf.",names(X2),value=T)
allvars<-paste(rep(vars,3),rep(1:3,each=length(vars)),sep="")
pd[,allvars]<-0


for(i in 1:nrow(pd)){

    client<-as.character(pd$name[i])
    pt<-unique(as.character(pd[i,c("patron1.2all","patron2.2all")]))
    yr<-pd$year[i]
    inc.sc<-sc.leader[[as.character(yr)]]
    inc.pb<-pb.leader[[as.character(yr)]]
    cty<-pd$city[i]
    

    cat(cty,"---",yr,"\n")
    
    # 1. Cumulative performance under patron #
    pd[i,paste(vars,"1",sep="")]<-apply(X2[which(X2$year<=yr & (X2$msec==client|X2$mayor==client) & X2$current.psec%in%pt ),vars],2,function(x){
      ifelse(length(x)==0,0,mean(x,na.rm=T))
    }) 
    
    # 2. Cumulative performance under none  patron #
    pd[i,paste(vars,"2",sep="")]<-apply(X2[which(X2$year<=yr & (X2$msec==client|X2$mayor==client) & !X2$current.psec%in%pt),vars],2,function(x){
      ifelse(length(x)==0,0,mean(x,na.rm=T))
    })
    
    # 3. Cumulative performance under all #
    pd[i,paste(vars,"3",sep="")]<-apply(X2[which(X2$year<=yr & (X2$msec==client|X2$mayor==client) ),vars],2,function(x){
      ifelse(length(x)==0,0,mean(x,na.rm=T))
    })
    
    
}




###############################################
####-----Merge with the Base Info Data-----####
###############################################


nm<-c("ID","name","sex","ethnicity","dob","birth.prov","birth.city","birth.county","highest.edu","status","party")
db2<-db[,1:length(nm)]
names(db2)<-nm

pd2<-merge(pd,db2,by.x="name",by.y="name")


pd2$age<-as.numeric(as.character(pd2$year))-year(pd2$dob)

pd2$firstcity<-gsub("^$","无",pd2$firstcity)
pd2$city<-gsub("^$","无",pd2$city)
pd2$birth.prov<-gsub("^$","无",pd2$birth.prov)
pd2$birth.city<-gsub("^$","无",pd2$birth.city)
pd2$birth.county<-gsub("^$","无",pd2$birth.county)
pd2$birth.year<-year(pd2$dob)
pd2$ethnicity<-gsub("^$","汉族",pd2$ethnicity)
pd2$highest.edu<-gsub("^$","不详",pd2$highest.edu)
pd2$party<-gsub("^$|中国共产党",NA,pd2$party)


pd2$highest.edu<-factor(pd2$highest.edu,levels=c("不详","高中","专科","本科","硕士","博士","博士后"))

names(pd2)<-gsub("\\.","_",names(pd2))

write.csv(pd2,"performance_and_promotion.csv",na="",row.names=F,fileEncoding = "UTF-8")






