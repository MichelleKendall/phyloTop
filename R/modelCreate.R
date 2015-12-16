modelCreate <- function(model,n,loadingBar=FALSE) {
  output <- list(rep(0,n))
  if (loadingBar) {print('Number of trees created:')}
  for (i in 1:n) {
    output[[i]] <- model()
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Complete')}
  return(output)
}