#' COMPLETE THIS
#' 
#' 
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return Complete this
#' 
#' @importFrom phylobase nTips
#' 
#' @examples
#' ILnumber(rtree4(10))
#' 
#' 
#' @export
ILnumber <- function(tree) {
  N <- nTips(tree)
  NDs <- treeImb(tree)[,(N+1):(2*N-1)]
  return(sum(apply(NDs,2, function(x) sum(x==1)==1))) # IL number
}