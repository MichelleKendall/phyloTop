#' Node imbalance
#' 
#' For a specified internal node, this function gives the number of tips descending from each of its two descending branches, as a measure of imbalance.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param node a node index between 1 and 2n-1, where n is the number of tips in the tree
#' @return Two integers, corresponding to the number of tip descendants of each of the node's descending branches.
#' 
#' @import ape
#' @importFrom phangorn Children
#'   
#' @examples
#' ## Find the imbalance of node 16 in a random tree with 10 tips:
#' nodeImb(rtree(10),16)
#' 
#' 
#' @export
nodeImb <- function(tree,node) {
  tree <- phyloCheck(tree)
  ntips <- length(tree$tip.label)
  if (node > (2*ntips-1)) {stop(paste0("Please supply a valid node number between 1 and ",(2*ntips-1)))}
  if (node <= ntips) {return(c(0,0))}
  else {
    configs <- nConfig(tree)$allsizes
    children <- Children(tree,node)
    left <- configs[[children[[1]]]]
    right <- configs[[children[[2]]]]
    return(c(left,right))
  }
}
