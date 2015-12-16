modelSummary <- function(model,topList,n, loadingBar=TRUE) {
  l <- length(topList)
  output <- matrix(0,n,l)
  if (loadingBar) {print('Number of topologies computed:')}
  for (i in 1:n){
    tree <- model() # Create new tree from model
    output[i,] <- topSumm(tree,topList)
    if (loadingBar) {print(i)}
  }
  output <- data.frame(output)
  if (loadingBar) {print('Complete')}
  return(output)
}