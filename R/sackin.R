#' Sackin index
#' 
#' Finds the Sackin index for a tree. 
#' Note that the package apTreeshape has a function to compute the Sackin index with additional options to normalize it based on the model;
#' we include this simple function here for convenience within this package, and for use on objects of class \code{phylo} and \code{phylo4}.
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @return The Sackin index of the tree.
#' 
#' @import ape
#'   
#' @examples
#' ## Sackin index of a random tree with 10 tips:
#' sackin.phylo(rtree(10))
#' 
#' 
#' @export
sackin.phylo <- function(tree) {
  depths <- getDepths(tree)
  tipDepths <- depths$tipDepths - 1
  return(sum(tipDepths))
}