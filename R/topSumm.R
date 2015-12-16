topSumm <- function(tree,topList) {
  l <- length(topList)
  if (l==1) {topList <- list(topList)}
  mat <- matrix(0,1,l)
  for (i in 1:l) {
    mat[1,i] <- topList[[i]](tree)
  }
  return(mat)
}