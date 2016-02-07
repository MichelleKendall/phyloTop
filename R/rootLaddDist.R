#' Root ladder distance
#' 
#' Called by ladderNums
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return Root ladder distance
#' 
#' @importFrom phylobase nodeId
#' @importFrom phylobase nodeType
#' @importFrom phylobase children
#' 
#' @keywords internal
#' 
#' @export
rootLaddDist <- function(tree) {
  root <- nodeId(tree)[nodeType(tree)=='root']
  childs <- children(tree,root)
  case <- sum(nodeType(tree)[childs]=='tip') #Number of tip children of root
  if (case==1) {
    nonTipChild <- childs[nodeType(tree)[childs]=='internal']
    return(1+rootLaddDist(subset(tree,node.subtree=nonTipChild)))
  }
  else {return(0)} #Not part of a ladder or 2 tip children
}