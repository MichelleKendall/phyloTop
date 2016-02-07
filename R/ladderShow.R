#' Show ladders
#' 
#' Plot a tree, highlighting any ladders within it
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @return A plot of the tree, with ladders highlighted in red.
#'   
#' @import ape
#' @importFrom phylobase nodeId
#' @importFrom phylobase nodeType
#' @examples
#' \dontrun{
#' ladderShow(rtree(100))
#' }
#' 
#' @export
ladderShow <- function(tree) {
  tree <- as(tree, "phylo4")
  ladderNumbers <- ladderNums(tree)
  col <- rep('black',nEdges(tree))
  # If the ladder number is at least one make the edge red
  E <- edges(tree)
  for (i in 2:nEdges(tree)) {
    if (ladderNumbers[E[i,1]] > 0) {col[i] <- 'red'}
  }
  plot(tree,edge.color=col)
}


