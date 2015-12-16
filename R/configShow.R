#' Plot a tree highlighting configurations
#' 
#' Plots the tree with configurations of a given size highlighted.
#' CURRENTLY BROKEN.
#' Probably needs argument "type="internal"" in nodeId?
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @param configSize an integer giving the configuration size of interest.
#' @return An integer representing the number of cherries in the tree.
#' 
#' @importFrom phylobase nodeId
#' 
#' @examples
#' ## Highlight cherries in a random tree:
#' configShow(rtree4(10),2)
#' 
#' 
#' @export
configShow <- function(tree,configSize) {
  stop("subtreeShow is currently broken, sorry")
  nTipDes <- nTipDescendants(tree)
  # Finds the nodes of the correct configsize
  confFind <- nodeId(tree)[which(nTipDes==configSize)] 
  # Prints the tree
  subtreeShow(tree,confFind)
}