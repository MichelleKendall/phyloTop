#' List of basic tree functions
#' 
#' Creates a base list for the tree to be used later.
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @param baseFuncs a list of base functions
#' @return A list of base functions evaluated on this tree.
#'   
#' @examples
#' ## Find the average ladder size and number of cherries in a random tree
#' baseCreate(rtree4(10),c(avgLadder,cherries))
#' 
#' 
#' @export
baseCreate <- function(tree,baseFuncs) {
  arr <- as.array(1:length(baseFuncs))
  output <- lapply(arr,function(x){baseFuncs[[x]](tree)})    
  return(output)
}