% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nodeImb.R
\name{nodeImb}
\alias{nodeImb}
\title{Node imbalance}
\usage{
nodeImb(tree, node)
}
\arguments{
\item{tree}{a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.}

\item{node}{a node index between 1 and 2n-1, where n is the number of tips in the tree.}
}
\value{
Two integers corresponding to the number of tip descendants of each of the node's two children. If the node is itself a tip, then the vector (0,0) will be returned.
}
\description{
For a given node, this function gives the number of tips descending from each of its two children, as a measure of imbalance.
}
\examples{
## Find the imbalance of node 16 in a random tree with 10 tips:
tree <- rtree(10)
plot(tree)
nodelabels()
nodeImb(tree,16)


}
\seealso{
\code{\link{treeImb}}
}
\author{
Michelle Kendall \email{michelle.louise.kendall@gmail.com}
}
