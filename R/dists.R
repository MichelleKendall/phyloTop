#' Gives the depth of each node
#' 
#' DEPENDS ON A DEPRICATED FUNCTION. UPDATE.
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return A vector giving the depth of each node
#' 
#' @importFrom phylobase nodeId
#' @importFrom phylobase nEdges
#' @importFrom phylobase edgeLength
#' @importFrom phylobase nodeDepth
#'   
#' @examples
#' dists(rtree(10))
#' 
#' 
#' @export
dists <- function(tree) {
  edgeLength(tree) <- rep(1,nEdges(tree))
  arr <- as.array(1:length(nodeId(tree)))
  suppressWarnings(nodeDep <- apply(arr,1,function(x){nodeDepth(tree,x)-1}))
  #-1 as the root has 'depth'=1
  return(nodeDep)
}
