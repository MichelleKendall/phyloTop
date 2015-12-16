treeListSummary <- function(treeList,topList,loadingBar=TRUE) {
  l1 <- length(treeList)
  l2 <- length(topList)
  output <- matrix(0,l1,l2)
  if (loadingBar) {print('Number of topology profiles computed:')}
  for (i in 1:l1) {
    output[i,] <- topSumm(treeList[[i]],topList)
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Complete')}
  return(data.frame(output))
}