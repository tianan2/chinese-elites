################ Some Data Tools ######################


### Create Lags and Forwards with Panel Data ####
panel.lag<-function(var,region,year,data,by=1,type="l"){
  d<-data[order(data[,region],data[,year]),]
  out<-list()
  if(type=="l"){
      out<-unlist(tapply(data[,var],data[,region],function(x){
        
        if((length(x)-by)>=1){
          return(c(rep(NA,by),x[1:(length(x)-by)]))
        } else{
          
          return(rep(NA,length(x)))
        }
        
      } ))
    
    out<-unlist(out)
    return(out)
  }
  
  if(type=="f"){
      out<-unlist(tapply(data[,var],data[,region],function(x){
        
        if((length(x)-by)>=1){
          return(c(x[(1+by):length(x)],rep(NA,by)))
        } else{
          return(rep(NA,length(x)))
                  }
        
      } ))
    
    out<-unlist(out)
    return(out)
  }
  
  
}
