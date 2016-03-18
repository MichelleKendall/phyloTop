#' Maximum tree height
#' 
#' Find the maximum height of tips in the tree.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#' @author Caroline Colijn \email{c.colijn@imperial.ac.uk}
#'   
#' @param tree a tree of class \code{phylo4}
#' @param normalise option to normalise the result, default is \code{FALSE}
#' @return An integer giving the maximum height of tips in the tree.
#' 
#' @import ape
#' @importFrom phylobase edgeLength
#' @importFrom phylobase nodeDepth
#' 
#' @examples
#' ## Maximum height of tips in a random tree:
#' tree <- rtree(10)
#' maxHeight(tree) 
#' maxHeight(tree, normalise=TRUE)
#' 
#' @export
maxHeight<-function(tree, normalise=FALSE){
  heights <- getDepths(tree)$tipDepths
  if (normalise==FALSE) {return(max(heights))}
  else {return(max(heights)/(length(tree$tip.label) - 1))}
}

