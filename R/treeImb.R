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
#' 
#' @examples
#' treeImb(rtree(10))
#' 
#' @export
treeImb <- function(tree) {
  tree <- phyloCheck(tree)
  ntips <- length(tree$tip.label)
  nn=tree$Nnode
  Ancs=(ntips+1):(ntips+nn) 
  
  # for each internal node, find its immediate children 
  Pointers=t(vapply(Ancs, function(x) tree$edge[tree$edge[,1]==x,2], FUN.VALUE=c(1,2))) 
  
  configs <- nConfig(tree)$cladeSizes
  
  imbalance <- t(sapply(c(1:ntips,Ancs), function(node) {
  if (node <= ntips) {return(c(0,0))}
  else {
    children <- Pointers[node-ntips,]
    left <- configs[[children[[1]]]]
    right <- configs[[children[[2]]]]
    return(c(left,right))
  }
  }))
  return(imbalance)
}
