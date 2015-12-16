#' Tree imbalance
#' 
#' Finds the imbalance of descendants: for each node it gives the number of descendants from of each descendant branch.
#' Notes: needs to issue warning about input types.
#' Does it work for non-binary trees?
#' Does it need to output a load of zeroes for tips or can we drop these entries?
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return A matrix where columns correspond to nodes and rows correspond to descendant branches
#'   
#' @examples
#' treeImb(rtree4(10))
#' 
#' 
#' @export
treeImb <- function(tree) {
  arr <- as.array(nodeId(tree))
  return(apply(arr,1,function(x){nodeImb(tree,x)}))
}
