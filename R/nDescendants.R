nDescendants <- function(tree) {
  nodelist<- seq(nTips(tree)+1,2*nTips(tree)-1) 
  desc<-apply(as.array(nodelist),1,function(x,tree) descendants(tree,x,type="all"),tree)
  #  desc <- descendants(tree,nodeId(tree),type='all') # old version
  descNo <- apply(as.array(1:length(desc)),1,function(x){length(desc[[x]])})
  return(descNo)
}