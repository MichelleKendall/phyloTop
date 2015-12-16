# Is this function called by any others?
classifyConfigFour <- function(tree) {
  nTipDes <- nTipDescendants(tree)
  fourConfig <- nodeId(tree)[nTipDes==4]
  laddType <- 0
  branchType <- 0
  for (node in fourConfig) {
    case <- nTipChildren(tree,node)
    if (case==1) {laddType <- laddType +1}
    else {branchType <- branchType + 1}
  }
  return(data.frame(laddType=laddType,branchType=branchType))
}