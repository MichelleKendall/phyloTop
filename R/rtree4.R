#' Random phylo4 tree
#'
#' Creates a random phylo4 tree, in parallel to \code{ape}'s \code{rtree} function.
#'
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'
#' @param n an integer giving the number of tips in the tree
#' @param ... further arguments to be passed to rtree
#' @return An object of class \code{"phylo4"}
#'
#' @importFrom ape rtree
#'
#' @examples
#' ## Create a random phylo4 tree with 10 tips
#' rtree4(10)
#'
#'
#' @export
rtree4 <- function(n, ...) {
  return(as(rtree(n, ...), 'phylo4'))
}