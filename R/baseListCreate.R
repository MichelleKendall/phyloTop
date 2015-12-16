#' List of basic tree functions for a list of trees
#' 
#' Creates a base list for each tree in a list.
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param treeList a list of trees of class \code{phylo4}
#' @param baseFuncs a list of base functions
#' @return A list of base functions evaluated on each tree.
#'   
#' @examples
#' ## Find the average ladder size and number of cherries in a random tree
#' baseListCreate(c(rtree4(10),rtree4(12)),c(avgLadder,cherries))
#' 
#' 
#' @export
baseListCreate <- function(treeList,baseFuncs,loadingBar = TRUE) {
  l <- length(treeList)
  output <- list(rep(0,l))
  arr <- as.array(1:length(baseFuncs))
  if (loadingBar) {print('Number of topology bases computed:')}
  for (i in 1:l) {
    output[[i]] <- lapply(arr,function(x){baseFuncs[[x]](treeList[[i]])})    
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Bases computed')}
  return(output)
}