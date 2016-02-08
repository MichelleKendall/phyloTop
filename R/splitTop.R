#' Split topology
#'
#' For nodes at a given distance from the root, this function finds their numbers of tip descendants
#'
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#' @author Michael Boyd \email{mboyd855@gmail.com}
#'
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param dist integer distance of nodes of interest from the root
#' @return The mean ladder size
#' 
#' @import ape
#' @importFrom phylobase descendants
#'
#' @examples
#' ## Find the split topology of a random tree with 20 tips:
#' tree <- rtree(20)
#' plot(tree)
#' splitTop(tree,2)
#' 
#'
#' @export
splitTop <- function(tree,dist) {
  tree <- phyloCheck(tree)
  depths <- getDepths(tree)
  allDepths <- c(depths$tipDepths,depths$nodeDepths)
  if (dist > max(allDepths)) {stop(paste0("For this tree, 'dists' must be <=",max(allDepths)))}
  nodes <- which(allDepths==dist)
  splits <- sapply(nodes,function(x){length(descendants(as(tree,"phylo4"),x,type='tips'))}) 
  return(sort(splits))
}
