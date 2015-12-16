widths <- function(tree) {
  nodeDists <- dists(tree)
  arr <- as.array(0:max(nodeDists))
  wids <- apply(arr,1,function(x){sum(nodeDists==x)})
  return(wids)
}