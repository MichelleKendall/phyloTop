subtreeShow <- function(tree,nodeList) {
  colNums <- numeric()
  col <- rep('black',nEdges(tree))
  # Finds the descendants of the nodes in nodeList
  for (node in nodeList) {
    colNums <- c(colNums,descendants(tree,node,type='all'))
  }
  E <- edges(tree)
  for (i in 2:nEdges(tree)) { # Colors the correct edges
    if (any(colNums==E[i,2])) {col[i] <- 'red'}
  }
  plot(tree,edge.color=col) # Prints the tree
}