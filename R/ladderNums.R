#' Ladder numbers
#' 
#' Finds the ladder numbers
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return A vector of ladder numbers (clarify)
#'   
#' @importFrom phylobase nodeId
#' @importFrom phylobase nodeType
#' @examples
#' ladderNums(rtree4(10))
#' 
#' 
#' @export
ladderNums <-function(tree) {
  arr <- as.array(nodeId(tree))
  locLadd <- function(x) {
    if (nodeType(tree)[x]=='tip') {return(0)}
    else {return(rootLaddDist(subset(tree,node.subtree=x)))}
  }
  ladderNumbers <- apply(arr,1,locLadd)
  return(ladderNumbers)
}

