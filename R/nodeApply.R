nodeApply <- function(tree, func, showId = TRUE) {
  ndTy <- nodeType(tree)
  intNds <- ndTy[which(ndTy!='tip')]
  arr <- as.array(1:length(intNds))
  f <- function(n) {
    node <- as(names(intNds)[n],'numeric')
    func(subset(tree, node.subtree = node))
  }
  output <- apply(arr,1,f)
  if (showId == TRUE) {names(output) <- names(intNds)}
  return(output)
}