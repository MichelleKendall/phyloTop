#' Cherry number
#' 
#' Finds the number of cherries in a tree
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return An integer representing the number of cherries in the tree.
#'   
#' @examples
#' cherries(rtree4(10))
#' 
#' 
#' @export
cherries<-function(tree) {nConfig(tree,2)}