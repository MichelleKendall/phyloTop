#' Show ladders
#' 
#' Plot a tree, highlighting any ladders within it
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#' @author Michael Boyd \email{mboyd855@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @param mainCol colour for branches which are not ladders (default is black)
#' @param ladderCol colour for ladder branches (default is red)
#' @param ... further arguments to be passed to plot.phylo
#' 
#' @return A plot of the tree, with ladders highlighted by colour.
#' 
#' @seealso \code{\link{ladderSizes}}
#'   
#' @import ape
#' @examples
#' ## Highlight in blue the ladders in a random tree with 50 tips:
#' \dontrun{
#' tree <- rtree(50)
#' ladderShow(tree, ladderCol="blue", edge.width=2)
#' # compare to:
#' ladderSizes(tree)
#' }
#' 
#' @export
ladderShow <- function(tree, mainCol="black", ladderCol="red", ...) {
  tree <- phyloCheck(tree)
  edgeList <- tree$edge
  nEdges <- length(tree$edge[,1])
  ladderBranches <- ladderSizes(tree)$ladderBr
  col <- rep(mainCol,nEdges)
  # If the ladder number is at least one, change the edge colour
  for (i in ladderBranches) {
    col[i] <- ladderCol
  }
  plot(tree,edge.color=col, ...)
}


