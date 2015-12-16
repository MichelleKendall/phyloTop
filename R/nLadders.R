nLadders <- function(tree) {
  ladderNumbers <- ladderNums(tree)
  nLadds <- 0
  m <- max(ladderNumbers)
  while (m > 0) {
    count <- sum(ladderNumbers==m)
    nLadds <- nLadds + count
    ladderNumbers <- laddItr(ladderNumbers)
    m <- max(ladderNumbers)
  }
  return(nLadds)
}
