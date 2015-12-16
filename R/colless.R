#' Colless number
#' 
#' Finds the Colless number for a tree
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return The Colless imbalance number of the tree.
#' 
#' @importFrom phylobase nTips
#'   
#' @examples
#' colless(rtree4(10))
#' 
#' 
#' @export
colless <- function(tree,normalize=TRUE) {
  if (nTips(tree)==2) {return(0)}
  tImb <- treeImb(tree)
  diffs <- abs(apply(tImb,2,diff))
  if (normalize) {
    n <- nTips(tree)
    m <- 2/((n-1)*(n-2))
    return(sum(diffs)*m)
  }
  return(sum(diffs))
}