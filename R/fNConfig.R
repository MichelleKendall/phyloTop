#' Number of internal nodes with a given number of tip descendants
#' 
#' Finds the number of internal nodes whose number of tip descendants is equal to \code{configSize}.
#' Similar to \code{nConfig}, but instead of taking a tree as input, it takes the vector of tip descendants as output by \code{nTipDescendants}.
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param nTipDes vector of tip descendants as output by \code{nTipDescendants}.
#' @param configSize an integer giving the size of configuration of interest.
#' @return An integer representing the number of internal nodes whose number of tip descendants is equal to \code{configSize}.
#' 
#' 
#' @examples
#' nTipDes <- nTipDescendants(rtree4(10))
#' ## Find the number of cherries:
#' fNConfig(nTipDes,2)
#' ## Find the number of pitchforks:
#' fNConfig(nTipDes,3)
#' 
#' 
#' @export
fNConfig <- function(nTipDes,configSize) {
  return(sum(nTipDes==configSize))
}
