##################################
##########Base Functions##########
##################################

#####1. Find the longest connected period given a certain attribute
##########
findConnected<-function(startidx=1,labelvar,begin="begin",end="end",data){
  label<-data[startidx,labelvar]
  begindate<-data[startidx,begin]
  dtemp<-na.omit(data[which(data[,labelvar]==label & data[,begin]>=data[startidx,begin]),c(begin,end)])
  ir<-IRanges(as.numeric(dtemp[,begin]),as.numeric(dtemp[,end]))
  rd<-reduce(ir,min.gapwidth=0)
  return(as.Date(end(rd)[which(start(rd)==begindate)],origin="1970-01-01"))
}



###############################
####Individual Modules#########
###############################


#############################################
#########MODULE A: School Overlap############
#############################################


#Detecting 
#PARAMETERS:
#time:whether time is considered in 
#client: the low-level person
#patron: the high-level person
#jobcat: a 2/4 column matrix indicating the customized categories each of the job belons. Use the level 1 job keyword if NULL
#time: whether simultaneuous attendence is required
#excludeparty: Party schools excluded?
#level: which level of education? values allowed: all, college
#minduration: Minimum overlapping duration required: in days


overlapSchool.core<-function(client,patron,jobcat=NULL,edulevel="all",time=F,minduration=60,data=d,noisily=F,personid="name"){
  
  if(noisily){
    cat(client,"---",patron,"\n")
  }
  if(is.null(jobcat)==F){#If customized job categories are provided, use level 1 job 
    #To be developed
  }
  
  #obtain client and patron data
  
  dclient<-data[which(data[,personid]==client),]
  dpatron<-data[which(data[,personid]==patron),]
  
  ##define level of education to focus on
  if(edulevel=="college") {
    dclient<-dclient[which(dclient$edu=="本科")[1],]
    dpatron<-dpatron[which(dpatron$edu=="本科")[1],]
  }


  #Construct the output data frame
  varname<-c("school.op","school","school.start","school.end","client.edu","patron.edu")
  overlapmat<-data.frame(matrix(c(0,rep(NA,length(varname)-1)),nrow=1))
  names(overlapmat)<-varname
  
  #if simultaneuous attendance is required
  if(time){
    dclient<-dclient[which(is.na(dclient$begin)==F & is.na(dclient$end)==F),]
    dpatron<-dpatron[which(is.na(dpatron$begin)==F & is.na(dpatron$end)==F),]
    

    ovp<-na.omit(intersect(dclient[,"lv1job"],dpatron[,"lv1job"]))
    
    
    if(length(ovp)==0){#If either person does not have any school-related experience, return null results
      return(overlapmat)
    }else{
      for(i in 1:nrow(dclient)){
        for(j in 1:nrow(dpatron)){
          #calculate duration
          duration<-min(findConnected(startidx=j,labelvar="lv1job",data=dpatron),findConnected(startidx=i,labelvar="lv1job",data=dclient))-max(dclient[i,"begin"],dpatron[j,"begin"])
          
          
          breakflag<-(dclient[i,"lv1job"]==dpatron[j,"lv1job"])&(dclient[i,"begin"]<=dpatron[j,"end"]&dclient[i,"end"]>=dpatron[j,"begin"]&duration>=minduration )
          if(breakflag) {
            overlapmat[1,1]<-1
            
            overlapmat[1,2]<-dclient[i,"lv1job"]
            
            overlapmat[1,3]<-max(dclient[i,"begin"],dpatron[j,"begin"])
            
            overlapmat[1,4]<-min(findConnected(startidx=j,labelvar="lv1job",data=dpatron),findConnected(startidx=i,labelvar="lv1job",data=dclient))
            
            overlapmat[1,5]<-dclient[i,"edu"]
            overlapmat[1,6]<-dpatron[j,"edu"]
            break
          }else{
            overlapmat[1,]<-c(0,rep(NA,5))
          }
        }
        if(breakflag)
          break
      }
      
      return(overlapmat)
    }
  } else { 
    #if simultaneuous attendance is NOT required
    
    ovpschool<-na.omit(intersect(unique(dclient[,"lv1job"]),unique(dpatron[,"lv1job"])))

    if(length(ovpschool)){
      overlapmat[1,1]<-1
      overlapmat[1,2]<-paste(ovpschool,collapse="#")
      #client's education
      overlapmat[1,5]<-paste(unique(dclient[dclient$lv1job%in%ovpschool,"edu"]),collapse="#")
      #patron's education
      overlapmat[1,6]<-paste(unique(dpatron[dpatron$lv1job%in%ovpschool,"edu"]),collapse="#")
    }
    return(overlapmat)
  }
}


###########################################
#########MODULE B: Work Overlap############
###########################################

#PARAMETERS:

#regioncat: province or city. city is the default
#jobcat: as in school overlap
#rankdiff: the first argument is the minimum difference, the second the maximum
#minduration: minimum duration of overlap
#changwei:whether changwei should be automatically considered as overlap
#oneout: If true, only yields the earliest overlap (if multiple, priority given to local overlap)
#####


overlapWork.core<-function(client,patron,level="city",minduration=60,rankdiff=c(0.5,2),CW=T,oneout=T,data=d,noisily=F,personid="name"){
  if(length(rankdiff)!=2){
    warning("Rank difference requires two arguments. c(0.5, 2) is assumed")
    rankdiff<-c(0.5, 2)
  }

  if(noisily){
    cat(client,"--",patron,"\n")
  }
  
########################################
#######1. Subset the Data
########

######Subset the main data into the client data and the patron data


  dclient<-data[which(data[,personid]==client),]
  
  dpatron<-data[which(data[,personid]==patron),]

##########################################
  #########2. Divide into two datasets, one for natioanl organization and one for local organization
  #############
  
  dc.nat<-dclient[which(dclient[,"natorg"]=="是"|dclient[,"centorg"]=="是"),]
  dc.loc<-dclient[which(dclient[,"natorg"]=="否"),]
  
  dp.nat<-dpatron[which(dpatron[,"natorg"]=="是"|dpatron[,"centorg"]=="是"),]
  dp.loc<-dpatron[which(dpatron[,"natorg"]=="否"),]
  

  #########################################
  #######3. Construct the output data frame
  ###########
  varname<-c("work.op","region","job","work.start","work.end","client.rank","patron.rank","client.keyjob","patron.keyjob")
  overlapmat<-data.frame(matrix(rep(c(0,rep(NA,length(varname)-1)),3),nrow=3,byrow=T))
  names(overlapmat)<-varname
  
  #set initial rank difference
  client.rank<-patron.rank<-rkdf<-0

####################################
  #######4-a. Analysis: Local overlap
  ####################################
  k<-1
  ovp<-intersect(unique(dc.loc[,"jobxregionid"]),unique(dp.loc[,"jobxregionid"]))

  

  if(length(ovp)>0){
    
    dc.loc<-dc.loc[which(dc.loc[,"jobxregionid"]%in%ovp),]
    
    dp.loc<-dp.loc[which(dp.loc[,"jobxregionid"]%in%ovp),]

    for(i in 1:nrow(dc.loc)){
      for(j in 1:nrow(dp.loc)){
        #calculate duration
        #first date
        firstdate<-max(dc.loc[i,"begin"],dp.loc[j,"begin"])
        
        lastdate<-min(findConnected(startidx=i,labelvar="jobxregionid",data=dc.loc),findConnected(startidx=j,labelvar="jobxregionid",data=dp.loc))

        
        duration<-lastdate-firstdate
                
        #Calculate rank difference based on maximum rank at the moment of potential overlap
        if(firstdate<lastdate){
          client.rank<-max(dclient[which(dclient$begin<=firstdate & dclient$end>=firstdate),"rank"])
          
          patron.rank<-max(dpatron[which(dpatron$begin<=firstdate & dpatron$end>=firstdate),"rank"])
          
          rkdf<-abs(patron.rank-client.rank)
          
        }
       
        #Set the breaking condition
        breakflag<-dc.loc[i,"jobxregionid"]==dp.loc[j,"jobxregionid"]&(dc.loc[i,"begin"]<=dp.loc[j,"end"]&dc.loc[i,"end"]>=dp.loc[j,"begin"]& duration>=minduration&(rkdf>=rankdiff[1] & rkdf<=rankdiff[2]) )
        if(breakflag) {
          
          overlapmat[k,"work.op"]<-1
          overlapmat[k,"region"]<-dc.loc[i,"regionid"]
          overlapmat[k,"job"]<-dc.loc[i,"lv1job"]
          
          overlapmat[k,"work.start"]<-firstdate
          
          overlapmat[k,"work.end"]<-lastdate
          
          overlapmat[k,"client.rank"]<-client.rank
          overlapmat[k,"patron.rank"]<-patron.rank
          overlapmat[k,"client.keyjob"]<-dc.loc[i,"keypost"]
          overlapmat[k,"patron.keyjob"]<-dp.loc[j,"keypost"]
          break
        }
      }
      if(breakflag) break
    }
  }
  
  ######################################
  #####4-b. Analysis: Central overlap###
  ###########
  k<-2
  ovp<-intersect(unique(dc.nat[,"lv1job"]),unique(dp.nat[,"lv1job"]))
  if(length(ovp)>0){
    

    dc.nat<-dc.nat[which(dc.nat[,"lv1job"]%in%ovp),]
    dp.nat<-dp.nat[which(dp.nat[,"lv1job"]%in%ovp),]
    
    for(i in 1:nrow(dc.nat)){
      for(j in 1:nrow(dp.nat)){
        #calculate duration
        #first date
        firstdate<-max(dc.nat[i,"begin"],dp.nat[j,"begin"])
        
        lastdate<-min(findConnected(startidx=i,labelvar="lv1job",data=dc.nat),findConnected(startidx=j,labelvar="lv1job",data=dp.nat))
        
        
        duration<-lastdate-firstdate
        
        #Calculate rank difference based on maximum rank at the moment of potential overlap
        if(firstdate<lastdate){
          client.rank<-max(dclient[which(dclient$begin<=firstdate & dclient$end>=firstdate),"rank"])
          
          patron.rank<-max(dpatron[which(dpatron$begin<=firstdate & dpatron$end>=firstdate),"rank"])
          
          rkdf<-abs(patron.rank-client.rank)
          

        }
        
        #Set the breaking condition
        breakflag<-dc.nat[i,"lv1job"]==dp.nat[j,"lv1job"]&(dc.nat[i,"begin"]<=dp.nat[j,"end"]&dc.nat[i,"end"]>=dp.nat[j,"begin"]&duration>=minduration &(rkdf>=rankdiff[1] & rkdf<=rankdiff[2]))

        if(breakflag) {
          overlapmat[k,"work.op"]<-1
          overlapmat[k,"region"]<-paste(unique(c(dc.nat[i,"province"],dp.nat[j,"province"])),collapse="#")
          
          overlapmat[k,"job"]<-dc.nat[i,"lv1job"]
          
          overlapmat[k,"work.start"]<-firstdate
          
          overlapmat[k,"work.end"]<-lastdate
          
          overlapmat[k,"client.rank"]<-client.rank
          overlapmat[k,"patron.rank"]<-patron.rank
          overlapmat[k,"client.keyjob"]<-dc.nat[i,"keypost"]
          overlapmat[k,"patron.keyjob"]<-dp.nat[j,"keypost"]
          break
        }
      }
      if(breakflag) break
    } 
  }
  
  
########################################
########4-c. Analysis: Changwei overlap
#######
  k<-3
  if(CW){
    #If changwei overlap is TRUE
    dc<-dclient[which(dclient$cwid==1),]
    dp<-dpatron[which(dpatron$cwid==1),]
    
    ovp<-intersect(unique(dc[,"regionid"]),unique(dp[,"regionid"]))
    
    if(length(ovp)>0){
      for(i in 1:nrow(dc)){
        for(j in 1:nrow(dp)){
          #calculate duration
          #first date
          firstdate<-max(dc[i,"begin"],dp[j,"begin"])
          
          lastdate<-min(findConnected(startidx=i,labelvar="regionid",data=dc),findConnected(startidx=j,labelvar="regionid",data=dp))
          
          duration<-lastdate-firstdate
          #Calculate rank difference based on maximum rank at the moment of potential overlap
          if(firstdate<lastdate){
            client.rank<-max(dclient[which(dclient$begin<=firstdate & dclient$end>=firstdate),"rank"])
            
            patron.rank<-max(dpatron[which(dpatron$begin<=firstdate & dpatron$end>=firstdate),"rank"])
            
            rkdf<-abs(patron.rank-client.rank)
          }
          
          
          #Set the breaking condition for Chang wei overlap. Note: the criteria are more complicated at the provincial level--primary leaders are considered connected to both higher and lower level secondary leaders
          if(level=="city"){
        
          breakflag<-dc[i,"regionid"]==dp[j,"regionid"]&(dc[i,"begin"]<=dp[j,"end"]&dc[i,"end"]>=dp[j,"begin"]&duration>=minduration&(rkdf>=rankdiff[1] & rkdf<=rankdiff[2])) 
          }
          
          if(level=="province"){
            
            breakflag<-(dc[i,"begin"] <= dp[j,"end"] & dc[i,"end"] >= dp[j,"begin"] & duration >= minduration & rkdf >= rankdiff[1] & rkdf<=rankdiff[2]) & ((dc[i,"cwgrp1"]==1 & dp[j,"cwgrp1"]==1 & dc[i,"province"]==dp[j,"province"])|(dc[i,"cwgrp2"]==1 & dp[j,"cwgrp2"]==1 & dc[i,"city"]==dp[j,"city"]))
           
          }

          if(breakflag) {
            #Once the desired overlap is found
            overlapmat[k,"work.op"]<-1
            overlapmat[k,"region"]<-dc[i,"regionid"]
            overlapmat[k,"job"]<-paste(dc[i,"lv1job"],dp[j,"lv1job"],sep="#")
            names(d)
            
            overlapmat[k,"work.start"]<-firstdate
            
            overlapmat[k,"work.end"]<-lastdate
            
            overlapmat[k,"client.rank"]<-client.rank
            overlapmat[k,"patron.rank"]<-patron.rank
            overlapmat[k,"client.keyjob"]<-dc[i,"keypost"]
            overlapmat[k,"patron.keyjob"]<-dp[j,"keypost"]
            break
          }
        }
        if(breakflag) break
      }
    }
    
    
    
  }
########################################
########5. Combine and output
########
   overlapmat$type<-c("local","national","cw")
  if(oneout){
    if(all(overlapmat[,"work.op"]==0)){
      overlapmat[1,"type"]<-NA
      return(overlapmat[1,])
    } else{
      
      #if there is at least one overlap
      
      opid<-which(overlapmat[,"work.op"]==1)
      if(length(opid)==1){
        return(overlapmat[opid,])
      }
      if(length(opid>1)){
        minstart<-which.min(overlapmat[opid,"work.start"])
        if(length(minstart)==1){
          return(overlapmat[opid,][minstart,])
        }else{
          return(overlapmat[min(opid),])
        }
      }
    }
  }else{
    return(overlapmat)
    
  }
  #######  
}





###########################################
######MODULE C: Birthplace/Home Overlap#########
###########################################

#PARAMETERS
#personid: identifier, to prevent same name
#client,patron: as before
#level: the level of resolution

overlapHome.core<-function(client,patron,personid="name",level="city",data=db,noisily=F){
  if(noisily){
    cat(client,"---",patron,"\n")
  }
  
  overlapmat<-data.frame(matrix(c(0,rep(NA,3)),nrow=1))
  names(overlapmat)<-c("home.op","home.type","client.home","patron.home")
  if(is.na(client)==F & is.na(patron)==F & patron!=""){
    overlapmat[1,"home.op"]<-as.numeric(data[which(data[,personid]==client),"regionid"]==data[which(data[,personid]==patron),"regionid"])
    overlapmat[1,"home.type"]<-level
    overlapmat[1,"client.home"]<-data[which(data[,personid]==client),"regionid"]
    overlapmat[1,"patron.home"]<-data[which(data[,personid]==patron),"regionid"]
  }
  return(overlapmat)
}



#############################################
######MODULE D: Work-Home cross ties#########
#############################################
#PARAMETERS
#client, patron: as before
#clientjob: if "all" consider everyone who worked in that locality; if "leaders" only those with keyposts; if "main" only mayor and secretary; if "sec" secretary only
#level: the level at which comparison is made


overlapCross.core<-function(client,patron,clientjob="leaders",level="city",between,dexp=d,dbase=db,personid="name",noisily=F){
  if(noisily){
    cat(client,"---",patron,"\n")
  }
  
  #The period of influence
  ybeg<-as.Date(between[1],format='%Y-%m-%d')
  yend<-as.Date(between[2],format='%Y-%m-%d')
  
  stopifnot(is.na(ybeg)==F & is.na(yend)==F & yend>ybeg)


  
  #find the patron's hometown
  if(level=="province"){
    home<-paste(dbase[dbase[,personid]==patron,"province"],"",sep="#")    
  }
  if(level=="city"){
    home<-paste(dbase[dbase[,personid]==patron,"province"],dbase[dbase[,personid]==patron,"city"],sep="#")  
  }
  

  #construct the overlap matrix
  varname<-c("cross.op","cross.place","cross.job","cross.start","cross.end","cross.type")
  overlapmat<-data.frame(matrix(c(0,rep(NA,length(varname)-1)),nrow=1))
  names(overlapmat)<-varname
  
  #Client's job for the overlap
  clientjob<-match.arg(clientjob,c("all","leaders","main","sec"))
  if(clientjob=="all"){
    dclient<-dexp[which(dexp[,personid]==client  & dexp[,"regionid"]==home) ,]
  }
  if(clientjob=="leaders"){
    dclient<-dexp[which(dexp[,personid]==client & (!dexp[,"keypost"]%in%c("秘书","无","军队")) & dexp[,"regionid"]==home) ,]
  }
  if(clientjob=="main"){
    dclient<-dexp[which(dexp[,personid]==client & (dexp[,"keypost"]%in%c("市长","市委书记","省长","省委书记")) & dexp[,"regionid"]==home) ,]
  }
  if(clientjob=="sec"){
    dclient<-dexp[which(dexp[,personid]==client & (dexp[,"keypost"]%in%c("市委书记","省委书记")) & dexp[,"regionid"]==home) ,]
  }
  
  #判断重合
  dclient<-dclient[which(dclient[,"begin"]<yend & dclient[,"end"]>ybeg),]

  if(nrow(dclient)>0){
    overlapmat[1,"cross.op"]<-1
    overlapmat[1,"cross.place"]<-home
    overlapmat[1,"cross.job"]<-paste(dclient[which.max(dclient[,"rank"]),c("lv1job","exactjob")],collapse="#")
    overlapmat[1,"cross.start"]<-max(min(dclient[,"begin"]),ybeg)
    overlapmat[1,"cross.end"]<-min(max(dclient[,"end"]),yend)
    overlapmat[1,"cross.type"]<-level
    
  }
  return(overlapmat)
}





#############################################
#########MODULE E: Promotion ties############
#############################################
# PARAMETERS
# from: the the rank from which the client is promoted

# Note: if from=2 look at provincial leaders, if from#>2, look at central leaders.

# Note 2: Currently only able to identify provincial leader when promotion to 副部 or above is happening. More functionalities to be developed.  

overlapPromo.core<-function(client, patron,from=2 ,data=d,noisily=F,personid="name"){
  
  if(noisily) cat(client,"---",patron,"\n")
  
  
  #Construct the overlap matrix
  varname<-c("promo.op","promo.time","promo.prov","level.from","level.to","patron.job")
  overlapmat<-data.frame(matrix(c(0,rep(NA,length(varname)-1)),nrow=1))
  names(overlapmat)<-varname
    
  if(from==2){
    
    #promotion from prefecture to vice-provincial, look at provincial leaders in the same province   
    dpatron<-data[which(data[,personid]==patron & data[,"keypost"]%in%c("省委书记","省长")),]
  }
  
  if(from>2){
    
    #promotion from vice-provincial level or above,look at national leaders
    dpatron<-data[which(data[,personid]==patron & data[,"rank"]==4),]  
  }

  
  #Identify client's promotion timing  
  dclient<-data[which(data[,personid]==client),]

  #If no promotion, exclude directly
  if(any(dclient[,"rank"]>from) &any(dclient[,"rank"]<=from) ){    
    
    #If there's indeed promotion, then:
    
    #Find the promotion timing
    high.beg<-min(dclient[which(dclient[,"rank"]>from),"begin"])
    
    
    #Find the next level promoted to
    nextlv<-min(dclient[which(dclient[,"begin"]==high.beg & dclient[,"rank"]>from),"rank"])
    
    
    #Find the province promoted to
    prov.promo<-unique(dclient[which(dclient[,"begin"]==high.beg) ,"province"])
    
    
    if(length(prov.promo)>1){
      prov.promo<-paste(prov.promo,collapse="#")
    }
    
    
    #Find the level of the previous job (defined as the rank of the last job before promotion)
        
    prevlv<-max(dclient[dclient[,"begin"]==max(dclient[which(dclient[,"begin"]<high.beg & dclient[,"rank"]<nextlv),"begin"]),"rank"],na.rm=T)

    
    #Find the the location of the last job
#     prov.prev<-dclient[max(which(dclient[,"begin"]<high.beg & dclient[,"rank"]<=from)),"province"]
    
    prov.prev<-dclient[which.max(dclient[,"begin"]<high.beg & dclient[,"rank"]==prevlv & dclient[,"province"]!="中央"),"province"]
    
    if(from==2){
      #within province promotion requires consistency in province before and after promotion
      if(prov.promo==prov.prev){
        #If within province promotion
        dpatron<-dpatron[which(dpatron[,"begin"]<high.beg & dpatron[,"end"]>high.beg &dpatron[,"province"]==prov.promo),]
        if(nrow(dpatron)>0){
          #If such experience exists
          overlapmat[1,"promo.op"]<-1
          overlapmat[1,"promo.time"]<-high.beg
          overlapmat[1,"promo.prov"]<-prov.promo
          overlapmat[1,"level.from"]<-prevlv
          overlapmat[1,"level.to"]<-nextlv
          overlapmat[1,"patron.job"]<-dpatron[1,"keypost"]
          
        }
      }
    } 
    if(from>2){
      #If promotion is from vice-provincial level and the center is invovled
      
      dpatron<-dpatron[which(dpatron[,'begin']<high.beg & dpatron[,'end']>high.beg & dpatron[,'rank']==4),]
      if(nrow(dpatron)>0){
        #If such experience exists
        overlapmat[1,"promo.op"]<-1
        overlapmat[1,"promo.time"]<-high.beg
        overlapmat[1,"promo.prov"]<-prov.promo
        overlapmat[1,"level.from"]<-prevlv
        overlapmat[1,"level.to"]<-nextlv
        overlapmat[1,"patron.job"]<-paste(dpatron[1,"lv1job"],dpatron[1,"exactjob"],sep="#")
      }
      
    }
    return(overlapmat)
    
  } else{
    return(overlapmat)
  }
  
}


#############################################
#########MODULE E: Homophily ties############
#############################################

overlapHomo.core<-function(client,patron,before="2014-01-01",level="city",personid="name",minrank=0,data=d,noisily=F){
  if(noisily){
    client<-as.character(client)
    patron<-as.character(patron)
    cat(client,"---",patron,"\n")
  }
  varname<-c("homo.op","city.homo","cities","pf.homo","prov.funs")
  overlapmat<-data.frame(matrix(c(0,0,NA,0,NA),nrow=1))
  names(overlapmat)<-  c("homo.op","city.homo","cities","pf.homo","prov.funs")
  

  dclient<-data[which(data[,personid]==client & (data[,"begin"]<=as.Date(before,format='%Y-%m-%d')|is.na(data[,"end"])==T|is.na(data[,"begin"])==T) & data[,"rank"]>=minrank),]
  
  dpatron<-data[which(data[,personid]==patron  & (data[,"begin"]<=as.Date(before,format='%Y-%m-%d')|is.na(data[,"end"])==T|is.na(data[,"begin"])==T) & data[,"rank"]>=minrank),]
  

  ##########################################
  #########2. Divide into four datasets, 1) city,2) provincial functional organization, 3) central organization and 4) central enterprises
  #############
  
  ########  City Homo ##########
  dc.city<-dclient[which(dclient[,"natorg"]=="否" &  (dclient[,"city"]!=""|dclient[,"province"]%in%c("北京市","上海市","天津市","重庆市"))),]
  dp.city<-dpatron[which(dpatron[,"natorg"]=="否" &  (dpatron[,"city"]!=""|dpatron[,"province"]%in%c("北京市","上海市","天津市","重庆市"))),]
  
  city.homo<-intersect(unique(dc.city[,"regionid"]),unique(dp.city[,"regionid"]))

  ####### Provincial Function Homo ############
  dc.pf<-dclient[which(!dclient[,"lv1job"] %in% c("党委常委会/政治局 ","政府/国务院（综合）","人大常委会","政协常委会") & dclient[,"basecat"]!="共青团" & dclient[,"rank"]>=.5),]
  
  dp.pf<-dpatron[which(!dpatron[,"lv1job"] %in% c("党委常委会/政治局 ","政府/国务院（综合）","人大常委会","政协常委会") & dpatron[,"basecat"]!="共青团"& dpatron[,"rank"]>=.5),]

  pf.homo<-intersect(unique(dc.pf[,"lv1job"]),unique(dp.pf[,"lv1job"]))
  
  
#   pf.homo<-intersect(unique(dc.pf[,"provfunid"]),unique(dp.pf[,"provfunid"]))

  ####### Central Function Homo ######
#   dc.cf<-dclient[which((dclient[,"natorg"]=="是" | dclient[,"centorg"]=="是") & !dclient[,"lv1job"] %in% c("党委常委会/政治局 ","政府/国务院（综合）","人大常委会","政协常委会") & dclient[,"basecat"]!="共青团"),]
#   dp.cf<-dpatron[which((dpatron[,"natorg"]=="是" | dpatron[,"centorg"]=="是") & !dpatron[,"lv1job"] %in% c("党委常委会/政治局 ","政府/国务院（综合）","人大常委会","政协常委会") & dpatron[,"basecat"]!="共青团"),]
#   
#   cf.homo<-intersect(unique(dc.cf[,"lv1job"]),unique(dp.cf[,"lv1job"]))
#   
#   ####### Enterprises Homo ########
#   dc.soe<-dclient[which(dclient[,"basecat"]=="中央企业"),]
#   dp.soe<-dpatron[which(dpatron[,"basecat"]=="中央企业"),]
#   
#   soe.homo<-intersect(unique(dc.soe[,"lv1job"]),unique(dp.soe[,"lv1job"]))


  if(length(city.homo)==0 & length(pf.homo)==0){
    return(overlapmat)
  } else{
    overlapmat[1,"homo.op"]<-1
    
    overlapmat[1,"city.homo"]<-length(city.homo)
    
    overlapmat[1,"cities"]<-ifelse(length(city.homo)>0,paste(city.homo,collapse="||"),NA)
    
    overlapmat[1,"pf.homo"]<-length(pf.homo)
    
    overlapmat[1,"prov.funs"]<-ifelse(length(pf.homo)>0,paste(pf.homo,collapse="||"),NA)
    
#     overlapmat[1,"cf.homo"]<-length(cf.homo)
#     
#     overlapmat[1,"central.funs"]<-ifelse(length(cf.homo)>0,paste(cf.homo,collapse="||"),NA)
#     
#     overlapmat[1,"soe.homo"]<-length(soe.homo)
#     
#     overlapmat[1,"soes"]<-ifelse(length(soe.homo)>0,paste(soe.homo,collapse="||"),NA)
    
    return(overlapmat)
  
#   dc.nat<-dclient[which((dclient[,"natorg"]=="是"|dclient[,"centorg"]=="是") & dclient[,"rank"]<2),]
#   dc.loc<-dclient[which(dclient[,"natorg"]=="否" & dclient[,"rank"]<2),]
#   
# 
#   
#   dp.nat<-dpatron[which((dpatron[,"natorg"]=="是"|dpatron[,"centorg"]=="是") & dpatron[,"rank"]<=3),]
#   dp.loc<-dpatron[which(dpatron[,"natorg"]=="否"),]

  ####Regional Homophily#####
#   rh<-intersect(unique(dc.loc[,"regionid"]),unique(dp.loc[,"regionid"]))
#   ####Functional Homophily###
#   fh<-setdiff(intersect(unique(dc.nat[,"lv1job"]),unique(dp.nat[,"lv1job"])),c("党委常委会/政治局 ","政府/国务院（综合）"))
#   
#   if(length(rh)==0 & length(fh)==0){
#     return(overlapmat)
#   } else{
#     overlapmat[1,"homo.op"]<-1
#     
#     overlapmat[1,"region.intensity"]<-length(rh)
#     
#     overlapmat[1,"region"]<-ifelse(length(rh)>0,paste(rh,collapse="||"),NA)
#     
#     overlapmat[1,"function.intensity"]<-length(fh)
#     
#     overlapmat[1,"function"]<-ifelse(length(fh)>0,paste(fh,collapse="||"),NA)
#     
#     return(overlapmat)
  }
}


#############################################
#####################################################
##########Synthetic overlap analysis function########
#####################################################


#PARAMETERS
#client: a list of client names, must of equal length as the patron list
#patron: a list of patron names, must of equal length as the client list
#data: data
#personid: id used to identify person (prevent identical name)
#type: type of overlap criteria--school, home, work, cross, promo...etc
#edulevel (SCHOOL OVERLAP ONLY): if ="college", only college education experience will be compared 
#level: for geography-based overlap, identify the level of geographic unit to be compared (e.g. province, city...)
Overlap.group<-function(client,patron,type,data=d,dbase=db,excludeparty=T,level="city",before="2014-01-01", personid="name",...){
  
  stopifnot(length(client)==length(patron))
  tp<-match.arg(type,c("school","home","work","promo","cross","homo"))
  
  #if(is.null(before)==F)  
  #data<-data[data[,"begin"]<=before,]
  
  if(tp=="school"){
    
    #reorder by date
    data<-data[order(data[,personid],data[,"begin"]),]
    data<-data[intersect(which(data[,"basecat"]=="学校"),grep("学生|学员",data[,"exactjob"])),]
    
    
    if(excludeparty){
      if(length(grep("党校",data[,"lv1job"]))>0){
        data<-data[-grep("党校",data[,"lv1job"]),]
      }
    }

    #Analysis
    z<-as.data.frame(cbind(client,patron,rbindlist(mapply(function(i) overlapSchool.core(client=client[i],patron=patron[i],data=data,...),i=1:length(client),SIMPLIFY=F))))
  }
  if(tp=="work"){
    
    #if work overlap
    remove<-union(which(is.na(data[,"begin"])==T| is.na(data[,"end"]==T) | (data[,"lv1job"]=="不详" | data[,"province"]=="不详")|(data[,"province"] =="中央" & data[,"lv1job"]=="党委常委会/政治局 ")|data[,"lv1job"]=="中共中央书记处"|data[,"basecat"]%in%c("政协","人大")), intersect(which(data[,"basecat"]=="学校"),grep("学生|学员",data[,"exactjob"])))

    data<-data[-remove,]
        
    
    ####Create region identifier (regionvar)
    regionvar<-match.arg(level,c("province","city"))
    if(regionvar=="city"){
      data$regionid<-paste(data[,"province"],data[,"city"],sep="#")      
    }

    if(regionvar=="province"){
      data$regionid<-data[,"province"]
    }
    
    data[,"jobxregionid"]<-apply(data[,c("regionid","lv1job")],1,paste,collapse="#")
    
    ####create Changwei identifier (cw)
    
    #group 0: all leadership positions
    cw<-grep("省|市",unique(data[,"keypost"]),value=T)
    
    #group 1: mayor, secretary and all provincial leader
    cwgrp1<-grep("^市长$|^市委书记$|省",unique(data[,"keypost"]),value=T)
    
    #group 2: mayor secretary and municipal leaders
    
    cwgrp2<-grep("市",unique(data[,"keypost"]),value=T)
    
    data$cwid<-ifelse(data[,"keypost"]%in%cw,1,0)
    data$cwgrp1<-ifelse(data[,"keypost"] %in% cwgrp1,1,0)
    data$cwgrp2<-ifelse(data[,"keypost"] %in% cwgrp2,1,0)
    
    
    
    #ANALYSIS
    z<-as.data.frame(cbind(client,patron,rbindlist(mapply(function(i) overlapWork.core(client=client[i],patron=patron[i],data=data,level=level,...),i=1:length(client),SIMPLIFY=F))))
    
    
  }
  if(tp=="home"){
    regionvar<-match.arg(level,c("province","city"))
    if(regionvar=="province"){
      dbase[,"regionid"]<-paste(dbase[,"province"],"",sep="#")  
    }else{
      #If municipality, overlap at the municipality level is sufficient for city-level overlap
      muni<-c("北京市","上海市","重庆市","天津市")
      dbase[,"regionid"]<-ifelse(dbase[,"province"]%in%muni,paste(dbase[,"province"],"",sep="#"),paste(dbase[,"province"],dbase[,"city"],sep="#"))
    }
    
    #ANALYSIS
    z<-as.data.frame(cbind(client,patron,rbindlist(mapply(function(i) overlapHome.core(client=client[i],patron=patron[i],level=level,data=dbase,...),i=1:length(client),SIMPLIFY=F))))
    
    #Use rbindlist to rewrite the conversion
    
    
    
    
  }
  if(tp=="cross"){
    citykeypost<-unique(grep("市",data[,"keypost"],value=T))
    pvkeypost<-unique(grep("省",data[,"keypost"],value=T))
    
    #the level at which overlap happens
    oplevel<-match.arg(level,c("province","city"))
    
    if(oplevel=="province"){
      
      #if overlap is at the provincial level, confine to provincial-level jobs only (exclude municipal jobs within the province)
      
      data<-data[-which(data[,"keypost"]%in%citykeypost|data[,"city"]!=""),]
      
      data[,"regionid"]<-paste(data[,"province"],"",sep="#")
      
      dbase[,"regionid"]<-paste(dbase[,"province"],"",sep="#")
    }
    if(oplevel=="city"){
      
      #if overlap is at the city level, confine to city jobs
      data<-data[-which(data[,"keypost"]%in%pvkeypost|data[,"city"]==""),]
      
      data[,"regionid"]<-paste(data[,"province"],data[,"city"],sep="#")
      
      dbase[,"regionid"]<-paste(dbase[,"province"],dbase[,"city"],sep="#")
    }
    
    z<-as.data.frame(cbind(client,patron,rbindlist(mapply(function(i) overlapCross.core(client=client[i],patron=patron[i],level=level,dexp=data,dbase=dbase,...),i=1:length(client),SIMPLIFY=F))))
    
  }
  if(tp=="promo"){
    remove<-union(which(is.na(data[,"begin"])==T| is.na(data[,"end"]==T) | (data[,"lv1job"]=="不详" | data[,"province"]=="不详") ), intersect(which(data[,"basecat"]=="学校"),grep("学生|学员",data[,"exactjob"])))
    
    z<-as.data.frame(cbind(client,patron,rbindlist(mapply(function(i) overlapPromo.core(client=client[i],patron=patron[i],data=data,...),i=1:length(client),SIMPLIFY=F))))
    
    
  }
  if(tp=="homo"){
    remove<-union(which((data[,"lv1job"]=="不详" | data[,"province"]=="不详")), intersect(which(data[,"basecat"]=="学校"),grep("学生|学员",data[,"exactjob"])))
    
    data<-data[-remove,]
    
    data[,"regionid"]<-paste(data[,"province"],data[,"city"],sep="#")
    data[,"provfunid"]<-paste(data[,"province"],data[,"lv1job"],sep="#")
    
#     regionvar<-match.arg(level,c("province","city"))
#     if(regionvar=="province"){
#       data[,"regionid"]<-paste(data[,"province"],"",sep="#")  
#     }else{
#       data[,"regionid"]<-paste(data[,"province"],data[,"city"],sep="#")
#     }
#     
    if(is.null(before)){
      before<-"2014-06-01"
    } else {
      if(length(before)==1){
        before<-rep(before,length(client))
      } else{
        stopifnot(length(before)==length(client))
      }
    }
    z<-as.data.frame(cbind(client,patron,rbindlist(mapply(function(i) overlapHomo.core(client=client[i],patron=patron[i],before=before[i],data=data,...),i=1:length(client),SIMPLIFY=F))))
  }
  return(z)
}



#####################################################
#################Additional Functions################
#####################################################

#################################################################
#########Identifying Promoter at the Provincial Level############
#################################################################

min.na<-function(x){
  if(length(x)>0){
    return(min(x))
  }else{
    return(character(0))
  }
}
max.na<-function(x){
  if(length(x)>0){
    return(max(x))
  }else{
    return(character(0))
  }
}


#@type: 2bureau=to bureau level ; 2all=to mayor or secretary; 2sec= to secretary only
idPromo<-function(client,type="2bureau",data=d,max.rank=4,min.rank=1.5,noisily=F,exclude.vpcity=T){
  if(noisily){
    cat(client,"\n")
  }
  tp<-match.arg(type,c("2bureau","2all","2sec","2vp","2vb","2fp"))
  if(tp=="2vb"){
    datepro<-min.na(data[which(data[,"name"]==client & data[,"rank"]>=1.5),"begin"])
    #datepre<-max.na(data[which(data[,"name"]==client & data[,"begin"]<datepro),"begin"])
  }
    
  if(tp=="2bureau"){
    datepro<-min.na(data[which(data[,"name"]==client & data[,"rank"]>=2),"begin"])
    #datepre<-max.na(data[which(data[,"name"]==client & data[,"begin"]<datepro),"begin"])
  }
  if(tp=="2all"){
    if(max.rank>=2.5 & exclude.vpcity){ # If promotion to vice provincial city leader is included, the promoted must have bureau-level experience in the promoted province
      datepro<-min.na(data[which(data[,"name"]==client & data[,"keypost"]%in%c("市长","市委书记") & data[,"rank"]<=max.rank & data[,"rank"]>=min.rank),"begin"])
      
      first.rank<-min(data[which(data[,"name"]==client & data[,"keypost"]%in%c("市长","市委书记")),"rank"])
      pro.pv<-unique(setdiff(data[which(data[,"name"]==client & data[,"begin"]==datepro),"province"],"中央"))

      if(first.rank>=2.5 & any(data[,"name"]==client & data[,"province"]%in%pro.pv & data[,"rank"]==2)==F){ 
        datepro<-as.Date("3000-12-31",format='%Y-%m-%d')
      }
      
    }else{
      datepro<-min.na(data[which(data[,"name"]==client & data[,"keypost"]%in%c("市长","市委书记") & data[,"rank"]<=max.rank & data[,"rank"]>=min.rank),"begin"])
    }
  }
  if(tp=="2sec"){  
    datepro<-min.na(data[which(data[,"name"]==client & data[,"keypost"]=="市委书记" & data[,"rank"]<=max.rank & data[,"rank"]>=min.rank),"begin"])
    #datepre<-max.na(data[which(data[,"name"]==client & data[,"begin"]<datepro),"begin"])
  }
  if(tp=="2vp"){
    datepro<-min.na(data[which(data[,"name"]==client & data[,"rank"]>=2.5),"begin"])
    #datepre<-max.na(data[which(data[,"name"]==client & data[,"begin"]<datepro),"begin"])
  }
  
  if(tp=="2fp"){
    datepro<-min.na(data[which(data[,"name"]==client & data[,"rank"]==3),"begin"])
    #datepre<-max.na(data[which(data[,"name"]==client & data[,"begin"]<datepro),"begin"])
  }
  
  if(length(datepro)==0){
    datepro<-as.Date("3000-12-31",format='%Y-%m-%d')
  }

  dprior<-data[which(data[,"name"]==client & data[,"begin"]<datepro & data[,"end"]>=datepro-365*2),]
  dpromo<-data[which(data[,"name"]==client & data[,"begin"]==datepro),]

  priorplace<-unique(dprior[,"province"])
  promoplace<-unique(dpromo[,"province"])

  if(length(priorplace)==0) priorplace<-promoplace
  
  if(any(dprior[,"natorg"]=="是"|dprior[,"centorg"]=="是")) priorplace<-c("中央",priorplace)
  
  pvs<-intersect(priorplace,promoplace)
  
  #-- Output Matrix --#
  
  out<-data.frame(no1="Unknown",no2="Unknown",promo.year=1900,promo.place="Unknown",stringsAsFactors=F)
  
  if(length(pvs)==0 & nrow(dpromo)>0 & nrow(dprior)>0){
    if(tp=="2bureau"|tp=="2vp"|tp=="2vb"|tp=="2fp"){
 
    out[,"promo.year"]<-year(datepro)
    out[,"promo.place"]<-"cross"
    
    prior.leader<-list()
    post.leader<-list()

    for(i in 1:nrow(dprior)){
    # Find all leaders in non-central place
    if(dprior[i,"province"]!="中央"){
      if(tp!="2fp"){ 
        prior.leader[[i]]<-unique(d[which(d$province==dprior[i,"province"] &  d$end<=datepro & d$end>=(datepro-365) & d$keypost%in%c("省委书记","省长")),"name"])
      }else{ # if type= full provincial, disregard regional boss
        prior.leader[[i]]<-"Unknown"
      }

    }else{
      # Prior experience in 中央
      if(tp=="2fp"){ # if type != full provincial, set boss rank to 4 (PSC member)
        bossrank<-4
      }else{ # if type is promote to vice provincial or lower
        bossrank<-3
      }
      dhead<-d[which(d$lv1job%in%dprior[i,"lv1job"] & d$begin<datepro &  d$end<=datepro & d$end>=(datepro-365) & d$province=="中央" & d$lv2job=="" & d$lv3job=="" & d$rank>=bossrank),]
      prior.leader[[i]]<-dhead[grep("\\b(局长|部长|主任|书记|书记处第一书记|政法委书记|纪委书记)\\b",dhead$exactjob),"name"]
      
    }
    
  }

  
  for(i in 1:nrow(dpromo)){ # Loop through the post-promotion experience
    # Find all leaders in destination place
    if(dpromo[i,"province"]!="中央"){
      # Prior experience NOT in 中央
      if(tp!="2fp"){ 
        post.leader[[i]]<-unique(d[which(d$province==dpromo[i,"province"] & d$begin<=datepro & d$end>datepro & d$keypost%in%c("省委书记","省长")),"name"])
      }else{ # If promotion type is full provincial, disregard regional boss
        post.leader[[i]]<-"Unknown"
      }
      
    }else{
      
      # Prior experience in 中央
      if(tp=="2fp"){ # if type is not to full provincial
        bossrank<-4
      }else{ # if type is promote to vice provincial or lower
        bossrank<-3
      }
      
      # Prior experience in 中央
      dhead<-d[which(d$lv1job%in%dpromo[i,"lv1job"] & d$begin<=datepro &  d$end>datepro & d$province=="中央" & d$lv2job=="" & d$lv3job=="" & d$rank>=bossrank),]
      post.leader[[i]]<-dhead[grep("\\b(局长|部长|主任|书记|书记处第一书记|组长|政法委书记|纪委书记)\\b",dhead$exactjob),"name"]
    }
  }

  ld<-unique(intersect(unlist(prior.leader),unlist(post.leader)))
  if(length(ld)==1){
    out[1,1]<-ld
  }
  if(length(ld)==2){
    out[1,2]<-ld
  }
    }
  
}


  if(length(pvs)==1){

    # If the unique province is NOT "中央"
    if(pvs!="中央"){
      if(tp!="2fp"){
        p.sec<-unique(d[which(d$province==pvs & d$begin<datepro & d$end>=datepro & d$keypost=="省委书记"),"name"])
        
        p.gvn<-unique(d[which(d$province==pvs & d$begin<datepro & d$end>=datepro & d$keypost=="省长"),"name"])
        
        out[1,1]<-ifelse(length(p.sec)>0,p.sec,"Unknown")
        out[1,2]<-ifelse(length(p.gvn)>0,p.gvn,"Unknown")
        out[1,3]<-year(datepro)
        out[1,4]<-pvs
      }
      
    }else{
      # If the unique province is "中央"
      if(tp=="2bureau"|tp=="2vp"|tp=="2vb"|tp=="2fp"){ # Exclude 中央 patron when movement is to local sec or mayor positions
        toprank<-ifelse(tp=="vb",1.5,ifelse(tp=="2bureau",2,ifelse(tp=="2vp",2.5,3))) 
        uniquejob<-unique(dprior[which((dprior$province=="中央"|dprior$natorg=="是"|dprior$centorg=="是") & dprior$rank<=toprank),"lv1job"])

        # Prior experience in 中央
        if(tp=="2fp"){ # if type is not to full provincial
          bossrank<-4
        }else{ # if type is promote to vice provincial or lower
          bossrank<-3
        }
        
        dhead<-d[which(d$lv1job%in%uniquejob & d$begin<datepro &  d$end>=datepro & d$province=="中央" & d$lv2job=="" & d$lv3job=="" & d$rank>=bossrank),]
        dhead<-dhead[grep("\\b(局长|部长|主任|书记|书记处第一书记|组长|政法委书记|纪委书记)\\b",dhead$exactjob),"name"]
        
        if(length(dhead)>0){
          out[1,1]<-paste(dhead,collapse="")
          out[1,2]<-"Unknown"
          out[1,3]<-year(datepro)
          out[1,4]<-"中央"
         
        }  
      }
      
    }
  } 




    return(out)

}

idGroup<-function(client,data=d,...){
  remove<-union(which(is.na(data[,"begin"])==T| is.na(data[,"end"]==T) | (data[,"lv1job"]=="不详" | data[,"province"]=="不详")), intersect(which(data[,"basecat"]=="学校"),grep("学生|学员",data[,"exactjob"])))

  data<-data[-remove,]
  z<-as.data.frame(cbind(client,rbindlist(mapply(function(i) {
    idPromo(client=client[i],data=data,...)
  },i=1:length(client),SIMPLIFY=F))))
  return(z)
}

#################################################################
#########           Leader-based Overlap           ##############
#################################################################
yTOym<-function(x,month){
  as.Date(paste(x,month,sep="-"),format='%Y-%m-%d')
  
}

idLeader<-function(client,before="2014-01-01",id="locid",data=d,minrank=1.5, maxrank=2, type="all",keypostonly=F,provonly=F){
  if(keypostonly==F){
    dclient<-data[which(data[,"name"]==client & data[,"rank"]>=minrank & data[,"rank"]<=maxrank),] 
  }else{
    dclient<-data[which(data[,"name"]==client & data[,"keypost"] %in% c("市长","市委书记")),] 
    
  }
  if(any(dclient$begin<before)==F){
    # if the earliest record start after the first day of that year, use the earliest starting time
    dclient<-dclient[which(dclient[,"begin"]==min(dclient[,"begin"])),]
  } else {
    dclient<-dclient[which(dclient[,"begin"]<before),]
  }
  timelist<-lapply(split.data.frame(dclient[,c("begin","end",id)],dclient[,id]),function(x) reduce(IRanges(start=as.numeric(x[,"begin"]),end=as.numeric(x[,"end"])),min.gapwidth=60))
  
  typelist<-lapply(split.data.frame(dclient[,c("province",id)],dclient[,id]), function(x) unique(x[,"province"]))
  
  provjoblist<-lapply(split.data.frame(dclient[,c("begin","end",id)],dclient[,id]), function(x) unique(x[,id]))
  
  if(type=="all"){
    ld<-c("省委书记","省长")
  }
  if(type=="psec"){
    ld<-"省委书记"
  }
  
  pts<-mapply(function(x,y,z) {
    if(z!="中央"){
      
      return(unique(unlist(mapply(function(start,end,prov){
        dhead<-unique(dneo[which(dneo[,"begin"]<end & dneo[,"end"]>=start & dneo[,"keypost"]%in% ld & dneo[,id]==prov ),"name"])
        if(length(dhead)>0){
          return(dhead)
        }else{
          return("Unknown")
        }
      } , start=start(x),end=end(x),prov=y,SIMPLIFY=F))))
      
      #       return(unique(data[which(data[,"begin"]<end(x) & data[,"end"]>=start(x) & data[,"keypost"]%in%ld& data[,"province"]==y),"name"]))
    }else{
      if(provonly==F){
        center<-mapply(function(start,end,job){
          dhead<-data[which(data[,"begin"]<end & data[,"end"]>=start & data[,id]==job  & data[,"lv2job"]=="" & data[,"lv3job"]=="" & data[,"rank"]>=3),]
          dhead<-dhead[grep("\\b(局长|部长|主任|书记|书记处第一书记)\\b",dhead$exactjob),"name"]
          if(length(dhead)>0){
            return(dhead)
          }else{
            return("Unknown")
          }
          
        },start=start(x),end=end(x),job=y,SIMPLIFY=F)
        return(unique(unlist(center)))
      } else{
        center<-mapply(function(job) return("Unknown"),job=y,SIMPLIFY=F)
        return(unique(unlist(center)))
      }
    }
  },x=timelist,y=provjoblist,z=typelist,SIMPLIFY=F)
  
  if(length(pts)==0){
    return("Unknown")
  }else{
    return(pts)
  }

}


