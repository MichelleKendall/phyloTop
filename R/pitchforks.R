#' Number of pitchforks
#' 
#' Finds the number of pitchforks in a tree. A pitchfork is considered to be a clade with three tips.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'     
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @return An integer representing the number of pitchforks in the tree.
#'   
#' @import ape
#' 
#' @examples
#' tree <- rtree(10)
#' plot(tree)
#' pitchforks(tree)
#' 
#' 
#' @export
pitchforks<-function(tree) {
  tree <- phyloCheck(tree)
  nConfig(tree)$numClades[[3]]
}
