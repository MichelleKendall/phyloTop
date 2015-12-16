#' Average ladder size
#'
#' Finds the mean size of ladders in a tree; unlike \code{avgLadder} this function
#' does not take a tree as input, but takes the ladder numbers themselves, as output by \code{ladderNums}
#'
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'
#' @param ladderNumbers a vector of ladder numbers as output by \code{ladderNums}
#' @return The average ladder size
#'
#' @examples
#' ## Find the average ladder size in a random tree
#' fAvgLadder(ladderNums(rtree4(10)))
#'
#'
#' @export
fAvgLadder <- function(ladderNumbers) {
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