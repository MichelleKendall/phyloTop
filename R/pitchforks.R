#' Number of pitchforks
#' 
#' Finds the number of pitchforks in a tree. A pitchfork is considered to be a clade with three tips.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'     
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @param normalise option to normalise the result, default is \code{FALSE}
#' @return An integer representing the number of pitchforks in the tree.
#'   
#' @import ape
#' 
#' @examples
#' tree <- rtree(10)
#' plot(tree)
#' pitchforks(tree)
#' pitchforks(tree, normalise=TRUE)
#' 
#' 
#' @export
pitchforks<-function(tree, normalise=FALSE) {
  tree <- phyloCheck(tree)
  if (normalise==FALSE) {return(nConfig(tree)$numClades[[3]])}
  else {return(3*nConfig(tree)$numClades[[3]]/length(tree$tip.label))}
}
