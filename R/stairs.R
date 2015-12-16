stairs <- function (tree) 
{
  N <- nTips(tree)
  NDs <- treeImb(tree)[, (N + 1):(2 * N - 1)]
  stair1 <- (1/(N - 1)) * sum(abs(NDs[2, ] - NDs[1, ]))
  stair2 <- (1/(N - 1)) * sum(pmin(NDs[2, ], NDs[1, ])/pmax(NDs[2, ], NDs[1, ]))
  return(c(stair1, stair2))
}