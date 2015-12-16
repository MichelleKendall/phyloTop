#' Average ladder size
#'
#' Finds the mean size of ladders in the tree
#'
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'
#' @param tree a tree of class \code{phylo4}
#' @return The average ladder size
#'
#' @examples
#' ## Find the average ladder size in a random tree
#' #avgLadder(rtree4(10))
#'
#'
#' @export
avgLadder <- function(tree) {
  ladderNumbers <- ladderNums(tree)
  sum <- 0
  nLadds <- 0
  m <- max(ladderNumbers)
  while (m > 0) {
    count <- sum(ladderNumbers==m)
    nLadds <- nLadds + count
    sum <- sum + m*count
    ladderNumbers <- laddItr(ladderNumbers)
    m <- max(ladderNumbers)
  }
  return(sum/nLadds)
}