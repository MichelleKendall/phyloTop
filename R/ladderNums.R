#' Ladder numbers
#' 
#' Finds the ladders and their positions in the tree, for plotting
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @return A vector corresponding to tree edges and their ladder sizes
#'   
#' @importFrom phylobase nodeId
#' @importFrom phylobase nodeType
#'
#' @keywords internal 
#' 
#' @export
ladderNums <-function(tree) {
  tree <- as(tree, "phylo4")
  arr <- as.array(nodeId(tree))
  locLadd <- function(x) {
    if (nodeType(tree)[x]=='tip') {return(0)}
    else {return(rootLaddDist(subset(tree,node.subtree=x)))}
  }
  ladderNumbers <- apply(arr,1,locLadd)
  return(ladderNumbers)
}