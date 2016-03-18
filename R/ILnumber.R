#' IL number
#' 
#' Computes the number of internal nodes with a single tip child. 
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param normalise option to normalise the result, default is \code{FALSE}
#' @return The integer number of internal nodes with a single tip child.
#' 
#' @import ape
#' 
#' @examples
#' tree <- rtree(10)
#' ILnumber(tree)
#' ILnumber(tree, normalise=TRUE)
#' 
#' @export
ILnumber <- function(tree, normalise=FALSE) {
  tree <- phyloCheck(tree)
  N <- length(tree$tip.label)
  NDs <- treeImb(tree)[(N+1):(2*N-1),]
  if (normalise==FALSE) { return(sum(apply(NDs,1, function(x) sum(x==1)==1))) }
  else { return(sum(apply(NDs,1, function(x) sum(x==1)==1))/(N-2)) }
}
