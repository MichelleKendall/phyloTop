#' Cherry number
#' 
#' Finds the number of cherries in a tree. A cherry is considered to be a pair of sister tips.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'     
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @return An integer representing the number of cherries in the tree.
#'
#' @import ape
#'        
#' @examples
#' tree <- rtree(10)
#' plot(tree)
#' cherries(tree)
#' 
#' 
#' @export
cherries<-function(tree) {
  tree <- phyloCheck(tree)
  nConfig(tree)$numClades[[2]]
  }