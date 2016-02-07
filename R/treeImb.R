#' Tree imbalance
#' 
#' Finds the imbalance of descendants: for each node it gives the number of descendants from of each descendant branch.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @return A matrix where rows correspond to nodes of the tree and columns correspond to each of the two descendant branches
#'
#' @import ape
#' @importFrom phangorn Children
#' 
#' @examples
#' treeImb(rtree(10))
#' 
#' @export
treeImb <- function(tree) {
  tree <- phyloCheck(tree)
  ntips <- length(tree$tip.label)
  configs <- nConfig(tree)$allsizes
  imbalance <- t(sapply(1:(2*ntips-1), function(node) {
  if (node <= ntips) {return(c(0,0))}
  else {
    children <- Children(tree,node)
    left <- configs[[children[[1]]]]
    right <- configs[[children[[2]]]]
    return(c(left,right))
  }
  }))
  return(imbalance)
}
