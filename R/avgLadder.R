#' Average ladder size
#'
#' Finds the mean size of ladders in the tree
#'
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param normalise option to normalise the result, default is \code{FALSE}
#' @return The mean ladder size
#' 
#' @import ape
#' 
#' @seealso \code{\link{ladderSizes}}
#'
#' @examples
#' ## Find the average ladder size in a random tree with 20 tips:
#' tree <- rtree(20)
#' avgLadder(tree)
#' avgLadder(tree, normalise=TRUE)
#' 
#'
#' @export
avgLadder <- function(tree, normalise=FALSE) {
  l <- ladderSizes(tree)$ladderSizes
  if (length(l)==0) warning("No ladders in this tree")
  if (normalise==FALSE) {return(mean(l))}
  else {return(mean(l)/(length(tree$tip.label)-2))}
}