#' Stairs
#' 
#' Calculates the stair numbers
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @return The stair numbers for a tree
#' 
#' @import ape
#'   
#' @examples
#' ## Find the stair numbers in a random tree with 20 tips:
#' stairs(rtree(20))
#'  
#' 
#' @export
stairs <- function(tree)  {
  N <- length(tree$tip.label)
  NDs <- treeImb(tree)[(N + 1):(2 * N - 1),]
  stair1 <- (1/(N - 1)) * sum(abs(NDs[2, ] - NDs[1, ]))
  stair2 <- (1/(N - 1)) * sum(pmin(NDs[2, ], NDs[1, ])/pmax(NDs[2, ], NDs[1, ]))
  return(c(stair1, stair2))
}