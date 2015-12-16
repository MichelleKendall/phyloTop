#' Analyse base properties of trees
#' 
#' Called by \code{baseListAnalysis} to apply topFuncs to baseFunc outputs
#' 
#' @author Michael Boyd \email{mboyd855@gmail.com}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param base an output from baseCreate
#' @param topFuncs a list of top functions
#' @return A matrix of values
#'   
#' 
#' 
#' @export
baseAnalysis <- function(base,topFuncs) {
  l <- length(topFuncs)
  if (l==1) {topFuncs <- list(topFuncs)}
  mat <- matrix(0,1,l)
  for (i in 1:l) {
    mat[1,i] <- topFuncs[[i]](base[[i]])
  }
  return(mat)
}