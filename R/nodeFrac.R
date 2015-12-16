nodeFrac <- function(tree,func,threshold) {
  nodeVals <- nodeApply(tree,func)
  count <- sum(nodeVals >= threshold)
  return(count/length(nodeVals))
}