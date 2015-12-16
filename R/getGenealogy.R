getGenealogy <- function(epirecord,epsilon=0.01) {
  # make genealogy from the sorted tree info produced by sortmyepi.  
  
  sortedtreeinfo<-sortmyepi(epirecord)
  
  Infectors <- sortedtreeinfo[,"Infector"];
  Infectees <- sortedtreeinfo[,"Infectee"];
  InfTimes <- sortedtreeinfo[,"InfnTime"];
  SortedRecTimes <- sortedtreeinfo[,"RecTime"]; 
  
  
  SortedRecTimes[which(SortedRecTimes==InfTimes)] <- SortedRecTimes[which(SortedRecTimes==InfTimes)] + epsilon
  
  NN <- length(InfTimes)-1; 
  NumLeaves <- NN+1; 
  BranchNums <- (NumLeaves+1):(2*NumLeaves-1); # IDs for internal branches of the genealogy
  LengthstoInternals <- 0*(1:NN)
  TipLengths <-0 *(1:length(Infectees))
  B <- matrix(0,nrow=NN, ncol=2) # use a 2-col [desc desc] format; convert afterwards
  
  
  for (n in 1:NN ) {
    #### PART 1 : connecting the internal branches / tips
    if (n ==1 ) { 
      B[n,]<-c(Infectors[n], Infectees[n]);
    } else { 
      
      # 1: did the infectEE go on to infect anyone else? 
      Onward <- is.element(Infectees[n],Infectors[1:(n-1)])
      if (Onward) {
        Desc1 <- BranchNums[max(which(Infectors[1:(n-1)] == Infectees[n]))]
      }else {
        Desc1 <- Infectees[n]
      }
      
      # 2: did the infectOR go on to infect anyone else? 
      Onward <- is.element(Infectors[n],Infectors[1:(n-1)])
      if (Onward){
        Desc2 <- BranchNums[max(which(Infectors[1:(n-1)]==Infectors[n]))]
      } else {
        Desc2 <- Infectors[n]
      }
      B[n,] <- c(Desc1,Desc2) # if n is NN, then this branch is the root. 
    } 
    
    
    ### PART 2: lengths of internal branches	
    HasInfBefore <- is.element(Infectors[n],Infectors[(n+1):length(Infectors)])
    if (HasInfBefore){
      IND=n+ min(which(Infectors[(n+1):length(Infectors)]==Infectors[n])) # most recently
      LengthstoInternals[n]=InfTimes[n]-InfTimes[IND]; # corresponds to int pt n, 
    } else {
      IND<- which(Infectees==Infectors[n]) # index when this guy got infected
      LengthstoInternals[n]=InfTimes[n]-InfTimes[IND] 
    }
  } 
  
  ### now lengths of tip branches
  for (n in 1:length(Infectees)) {
    HasInfBefore<-is.element(Infectees[n],Infectors)  
    if (HasInfBefore) {
      IND=min(which(Infectors==Infectees[n])) 
      TipLengths[n] <- SortedRecTimes[n]-InfTimes[IND] 
    } else {
      TipLengths[n]<-SortedRecTimes[n]-InfTimes[n]
    }
  }
  ## now create the genealogy: change format
  
  Edges=matrix(0,nrow=2*NN, ncol=2); Lengths<-0*(1:(2*NN))
  
  # the root is the chronologically first internal branch
  for (n in 1:NN) {
    Edges[2*n-1,1]<- n+NN+1; Edges[2*n,1]=n+NN+1; Edges[2*n-1,2]<-B[n,1]; Edges[2*n,2]<-B[n,2]
  }
  Lengths[1] <- 0;
  for (n in 2:(2*NN)) {
    IsDescTip <- (Edges[n,2]<= NN)
    if (IsDescTip) {
      Lengths[n]<- TipLengths[Edges[n,2]]		
    } else {
      Lengths[n]<- LengthstoInternals[Edges[n,2]-NN]
    }
  } 
  Lengths[Lengths==0]=epsilon
  Edges[Edges>NumLeaves]= 3*NumLeaves - Edges[Edges>NumLeaves]; # so branches start at ROOT
  Genealogy <- makephylotree(Edges,Lengths,NumLeaves+1) # NOW root is first branch, not last
  return(Genealogy)
}