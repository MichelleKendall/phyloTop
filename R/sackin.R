sackin <- function(tree) {
  edgeLength(tree) <- rep(1, nEdges(tree))
  arr <- as.array(1:nTips(tree))
  nodeDep <- apply(arr, 1, function(x) {
    nodeDepth(tree, x) - 1
  })
  return(sum(nodeDep))
}