#' Colless number
#' 
#' I think: finds the Colless number, given a tree imbalance as input.
#' Issues: does not match function "colless", so maybe I'm wrong.
#' Should it have an option to normalize, like "colless"?
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param treeImbalance a tree imbalance matrix as output by \code{treeImb}
#' @return An integer giving the Colless number the tree.
#'   
#' @examples
#' fColless(treeImb(mytree))
#' 
#' 
#' @export
fColless <- function(treeImbalance) {
  n <- (length(treeImb)+2)/4
  if (n==2) {return(0)}
  diffs <- abs(apply(treeImbalance,2,diff))
  m <- 2/((n-1)*(n-2))
  return(sum(diffs)*m)
}