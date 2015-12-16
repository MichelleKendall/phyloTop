# Is this function called by any others?
classifyConfigFive <- function(tree) {
  nTipDes <- nTipDescendants(tree)
  fiveConfig <- nodeId(tree)[nTipDes==5]
  laddType <- 0 #Has the largest ladder
  branchType <- 0 #Most branchy
  fourType <- 0 #Has a (4,1) split in tip descendants at the top (but is not a ladder)
  for (node in fiveConfig) {
    case <- nTipChildren(tree,node)
    if (case==1) {
      laddSize <- ladderDist(tree,node)
      if (laddSize==3) {laddType <- laddType +1}
      else {fourType <- fourType + 1}
    }
    else {branchType <- branchType + 1}
  }
  return(data.frame(laddType=laddType,fourType=fourType,branchType=branchType))
}