#' Show ladders
#' 
#' Plot a tree, highlighting any ladders within it
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#' @author Michael Boyd \email{mboyd855@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @param ladderCol colour with which to highlight ladder branches (default is red)
#' @param ... further arguments to be passed to plot.phylo
#' @return A plot of the tree, with ladders highlighted by colour.
#'   
#' @import ape
#' @examples
#' \dontrun{
#' ladderShow(rtree(50), ladderCol="blue", edge.width=2)
#' }
#' 
#' @export
ladderShow <- function(tree, ladderCol="red", ...) {
  edgeList <- tree$edge
  nEdges <- length(tree$edge[,1])
  ladderBranches <- ladderSizes(tree)$ladderBr
  col <- rep('black',nEdges)
  # If the ladder number is at least one make the edge red
  for (i in ladderBranches) {
    col[i] <- ladderCol
  }
  plot(tree,edge.color=col, ...)
}


