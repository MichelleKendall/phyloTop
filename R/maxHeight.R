#' Maximum tree height
#' 
#' Find the maximum height of tips in the tree.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#' @author Caroline Colijn \email{c.colijn@imperial.ac.uk}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return An integer giving the maximum height of tips in the tree.
#' 
#' @import ape
#' @importFrom phylobase edgeLength
#' @importFrom phylobase nodeDepth
#' 
#' @examples
#' ## Maximum height of tips in a random tree:
#' maxHeight(rtree(10))
#' 
#' 
#' @export
maxHeight<-function(tree){
  heights <- getDepths(tree)$tipDepths
  return(max(heights))
}

