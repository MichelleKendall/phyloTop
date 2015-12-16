#' Find configurations of a given size
#' 
#' I THINK: find the internal node IDs whose number of tip descendants is equal to configSize. 
#' However, it didn't do this at first; I have added "type="internal"" to make it work as I expected...
#' Come back to this.
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return An integer representing the number of cherries in the tree.
#'
#' @importFrom phylobase nodeId   
#'   
#' @examples
#' configFind(rtree4(10),2)
#' 
#' 
#' @export
configFind <- function(tree,configSize) {
  nDes <- nTipDescendants(tree)
  return(nodeId(tree, type="internal")[nDes==configSize])
}
