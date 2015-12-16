#' Number of tip descendants
#' 
#' Finds the number of tip descendants for each internal node of the tree
#' Notes: needs to issue warning about input types.
#' Adapt for phylo as well as phylo4?
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo4}
#' @return A vector whose entries correspond to internal nodes of the tree, and give the number of tip descendants of each node.
#'   
#' @importFrom phylobase nTips
#' @importFrom phylobase descendants
#'   
#' @examples
#' nTipDescendants(mytree)
#' 
#' 
#' @export
nTipDescendants <- function(tree) {
  nodelist<- seq(nTips(tree)+1,2*nTips(tree)-1)  
  desc<-apply(as.array(nodelist),1,function(x,tree) descendants(tree,x,type="tips"),tree) # CC added
  tipDes <- apply(as.array(1:length(desc)),1,function(x){length(desc[[x]])})
  return(tipDes)
}