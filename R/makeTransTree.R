makeTransTree <- function(lambda,duration=1,NumCases=50) {
  lambda<- c(lambda,0) # required because of < instead of <= in simNHP.fun
  epirecord<-matrix(0,1,4) # infectee, infector, infection time, recovery time
  cis<-1 #cis for current and pending infectors
  Infectee<-1; 
  rec<-duration; # note fixed duration of infection. 
  epirecord[1,]<-c(1,0,0, rec); 
  while (nrow(epirecord)<=NumCases & length(cis)>0) {
    itimes<-simNHP.fun(lambda)$posNH
    itimes<-itimes/(length(lambda)-1); # scaled so in [0,1]
    ci<-cis[1] # current infector. 
    NumInfected<- length(itimes) 
    if (NumInfected>0) {
      for (n in 1:NumInfected){
        Infectee<-Infectee+1; 
        epirecord<-rbind(epirecord,c(Infectee,ci, epirecord[ci,3]+itimes[n],epirecord[ci,3]+itimes[n]+rec))
        cis<-c(cis,Infectee)
      }
    }
    cis<-cis[-1] # 
  }
  colnames(epirecord)<-c("Infectee","Infector","InfnTime","RecTime")
  return(epirecord)
}
