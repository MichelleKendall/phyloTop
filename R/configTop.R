#' Number of each type of configuration
#' 
#' Finds the number of each type of configuration
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return A vector of the same length as the number of tips. Each entry corresponds to the number of configurations of that size.
#' For example, the second entry gives the number of configurations of size two; the nth entry gives the number of configurations of size n.
#'   
#' @examples
#' configTop(rtree4(10))
#' 
#' 
#' @export
configTop <- function(tree) {
  nTipDes <- nTipDescendants(tree)
  arr <- as.array(1:max(nTipDes))
  top <- apply(arr,1,function(x){sum(nTipDes==x)}) #Stands for topology
  return(top)
}