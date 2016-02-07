#' Plot a tree highlighting configurations
#' 
#' Plots the tree with configurations of a given size highlighted.
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @param configSize an integer giving the configuration size of interest.
#' @return A plot of the tree, highlighting the configurations of the given size.
#' 
#' @importFrom phylobase nodeId
#' 
#' @examples
#' ## Highlight pitchforks in a random tree:
#' \dontrun{
#' configShow(rtree(20),3)
#' }
#' 
#' @export
configShow <- function(tree,configSize) {
  tree <- as(tree, "phylo4")
  nTipDes <- nTipDescendants(tree)
  # Finds the nodes of the correct configsize
  confFind <- nodeId(tree, type="internal")[which(nTipDes==configSize)] 
  # Prints the tree
  subtreeShow(tree,confFind)
}