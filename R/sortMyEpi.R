sortMyEpi<-function(cmjepi) {
  sortinf<- sort(cmjepi[,"InfnTime"],decreasing=TRUE,index.return=TRUE)
  IND<-sortinf$ix
  return(cmjepi[IND,])	
}