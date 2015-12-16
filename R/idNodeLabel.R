#' Change labels of tree to integers
#' 
#' A tree created by \code{rtree4(k)} will have tip labels "t1", "t2", ... , "tk" and the internal node labels will all be "<NA>".
#' This function will relabel the tips to 1, 2, ..., k, and the internal nodes to k+1, k+2, ... , 2k - 1.
#' NOTE that the tip numbering is lost, is this deliberate?
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return A tree of class \code{phylo4} with relabelled nodes.
#' 
#' @importFrom phylobase nodeId
#' @importFrom phylobase nodeLabels
#' @importFrom phylobase tipLabels
#'   
#' @examples
#' idNodeLabel(rtree(10))
#' 
#' 
#' @export
idNodeLabel <- function(tree) {
  ndLbl <- paste(nodeId(tree,'internal'))
  tpLbl <- paste(nodeId(tree,'tip'))
  nodeLabels(tree) <- ndLbl
  tipLabels(tree) <- tpLbl
  return(tree)
}