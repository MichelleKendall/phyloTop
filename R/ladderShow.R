ladderShow <- function(tree) {
  ladderNumbers <- ladderNums(tree)
  col <- rep('black',nEdges(tree))
  # If the ladder number is at least one make the edge red
  E <- edges(tree)
  for (i in 2:nEdges(tree)) {
    if (ladderNumbers[E[i,1]] > 0) {col[i] <- 'red'}
  }
  plot(tree,edge.color=col)
}