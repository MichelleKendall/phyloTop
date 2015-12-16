idNodeLabel <- function(tree) {
  ndLbl <- paste(nodeId(tree,'internal'))
  tpLbl <- paste(nodeId(tree,'tip'))
  nodeLabels(tree) <- ndLbl
  tipLabels(tree) <- tpLbl
  return(tree)
}