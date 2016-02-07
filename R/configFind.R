#' Find configurations of a given size
#' 
#' Find the internal node IDs whose number of tip descendants is equal to configSize. 
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @param configSize integer giving size of configurations of interest
#' @return The internal node IDS with the given number of tip descendants
#'
#' @importFrom phylobase nodeId   
#' 
#' @keywords internal
#' 
#' @export
configFind <- function(tree,configSize) {
  tree <- as(tree, "phylo4")
  nDes <- nTipDescendants(tree)
  return(nodeId(tree, type="internal")[nDes==configSize])
}
