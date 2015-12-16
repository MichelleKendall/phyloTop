splitTop <- function(tree,dist) {
  nodeDists <- dists(tree)
  if (dist > max(nodeDists)) {stop('dist too large')}
  nodes <- as.array(nodeId(tree)[nodeDists==dist])
  splits <- apply(nodes,1,function(x){length(descendants(tree,x,type='tips'))}) 
  return(sort(splits))
}