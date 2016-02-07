#' Highlight a subtree
#' 
#' Plot a tree, highlighting the clade(s) descending from a specified node or nodes
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}.
#' @param nodeList a list of nodes in the tree
#' @return A plot of the tree, with the specified clade(s) highlighted.
#' 
#' @import ape
#' @importFrom phylobase nEdges
#' @importFrom phylobase descendants
#'   
#' @examples
#' ## Highlight the clade descending from node 13 in a random tree on 10 tips:
#' \dontrun{
#' subtreeShow(rtree(10),13)
#' }
#' 
#' @export
subtreeShow <- function(tree,nodeList) {
  tree <- as(tree, "phylo4")
  colNums <- numeric()
  col <- rep('black',nEdges(tree))
  # Finds the descendants of the nodes in nodeList
  for (node in nodeList) {
    colNums <- c(colNums,descendants(tree,node,type='all'))
  }
  E <- edges(tree)
  for (i in 2:nEdges(tree)) { # Colors the correct edges
    if (any(colNums==E[i,2])) {col[i] <- 'red'}
  }
  plot(tree,edge.color=col) # Prints the tree
}