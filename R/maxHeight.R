maxHeight<-function(tree){
  edgeLength(tree)[2:length(edgeLength(tree))]=1
  alldepths<-apply(as.matrix(seq(1:nTips(tree))),1,function(x,tree) nodeDepth(tree,x), tree)
  return(max(alldepths))
}