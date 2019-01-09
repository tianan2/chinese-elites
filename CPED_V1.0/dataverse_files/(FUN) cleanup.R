##### NOTE: BEFORE RUNNING THIS, PLEASE ENSURE YOUR COMPUTER SUPPORTS SIMPLIFIED CHINESE ############


### CLEANING UP THE BIO DATA ####


names(d)<-c("ID","name","expid","begin","end","natorg","centorg","provid","province","cityid","city","comment1","cityrank","county","basecat","jobid","lv1job","comment2","lv2job","lv3job","exactjob","rank","keypost","edu")
d$begin<-as.Date(d$begin,format='%m/%d/%Y')
d$end<-as.Date(d$end,format='%m/%d/%Y')

rankname<-unique(d$rank)
ranknum<-c(0,0.5,1,1.5,2,2.5,0,3,3.5,4)
sapply(1:length(rankname),function(x) d$rank[d$rank==rankname[x]]<<-ranknum[x])
d$rank<-as.numeric(d$rank)




#读取人员基本信息
names(db)<-c("ID","name","sex","ethnicity","dob","province","city","county","highest.edu","party","status")
db$dob<-as.Date(db$dob,format='%m/%d/%Y')


#调整90年代前海外留学经历
#Identify overseas experience
domestic<-grep("省|区|市|不详|中央",d[,"province"])
af90<-which(d$begin>=as.Date("1990-01-01",format='%Y-%m-%d'))
d[-union(domestic,af90),"lv1job"]<-paste(d[-union(domestic,af90),"province"],"留学",sep="")



