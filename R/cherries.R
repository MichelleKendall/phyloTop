#' Cherry number
#' 
#' Finds the number of cherries in a tree. A cherry is considered to be a pair of sister tips.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'     
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @param normalise option to normalise the result, default is \code{FALSE}
#' @return An integer representing the number of cherries in the tree.
#'
#' @import ape
#'        
#' @examples
#' tree <- rtree(10)
#' plot(tree)
#' cherries(tree)
#' cherries(tree, normalise=TRUE)
#' 
#' 
#' @export
cherries<-function(tree, normalise=FALSE) {
  tree <- phyloCheck(tree)
  if (normalise==FALSE) {return(nConfig(tree)$numClades[[2]])}
  else {return(2*nConfig(tree)$numClades[[2]]/length(tree$tip.label))}
}
