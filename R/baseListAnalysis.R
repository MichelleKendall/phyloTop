#' Analysis of a list of basic tree functions for a list of trees
#' 
#' Takes a base list for a list of trees, and analyses it. 
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' Name columns of output so you can see which topFuncs were used
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param baseList an object of the type created by baseListCreate.
#' @param topFuncs a list of functions that operate on elements of baseList.
#' @param loadingBar option to include a loading bar. Defaults to \code{FALSE}.
#' @return Analysis of a list of base functions evaluated on each tree.
#'   
#' @examples
#' ## Find the average ladder size and number of cherries in a random tree
#' testlist <- baseListCreate(c(rtree4(10),rtree4(12)),c(avgLadder,cherries))
#' baseListAnalysis(testlist,c(mean,max))
#' 
#' @export
baseListAnalysis <- function(baseList,topFuncs,loadingBar = FALSE) {
  l1 <- length(baseList)
  l2 <- length(topFuncs)
  output <- matrix(0,l1,l2)
  if (loadingBar) {print('Number of topology profiles computed:')}
  for (i in 1:l1) {
    output[i,] <- baseAnalysis(baseList[[i]],topFuncs)
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Topology profiles computed')}
  return(data.frame(output))
}