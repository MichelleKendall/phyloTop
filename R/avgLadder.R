#' Average ladder size
#'
#' Finds the mean size of ladders in the tree
#'
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @return The mean ladder size
#' 
#' @import ape
#' 
#' @seealso \code{\link{ladderSizes}}
#'
#' @examples
#' ## Find the average ladder size in a random tree with 20 tips:
#' avgLadder(rtree(20))
#' 
#'
#' @export
avgLadder <- function(tree) {
  l <- ladderSizes(tree)$ladderSizes
  if (length(l)==0) stop("No ladders in this tree")
  return(mean(l))
}