#' Number of internal nodes with a given number of tip descendants
#' 
#' Finds the number of internal nodes whose number of tip descendants is equal to \code{configSize}.
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @param configSize an integer giving the size of configuration of interest.
#' @return An integer representing the number of internal nodes whose number of tip descendants is equal to \code{configSize}.
#' 
#' 
#' @examples
#' ## Find the number of cherries:
#' nConfig(rtree4(10),2)
#' ## Find the number of pitchforks:
#' nConfig(rtree4(10),3)
#' 
#' 
#' @export
nConfig <- function(tree,configSize) {
  return(sum(nTipDescendants(tree)==configSize))
}

