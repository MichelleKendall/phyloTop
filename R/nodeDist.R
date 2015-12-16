nodeDist <- function(tree,node) {
  edgeLength(tree) <- rep(1,nEdges(tree))
  return(nodeDepth(tree,node)-1) #-1 as the root has dist=1
}