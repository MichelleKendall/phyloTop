#' Average ladder size
#'
#' Finds the mean size of ladders in the tree
#'
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @return The mean ladder size
#' 
#' @import ape
#'
#' @examples
#' ## Find the average ladder size in a random tree with 20 tips:
#' \dontrun{ # known bug!
#' avgLadder(rtree(20))
#' }
#'
#' @export
avgLadder <- function(tree) {
  l <- ladderSizes(tree)
  if (length(l)==0) stop("No ladders in this tree")
  return(mean(l))
}