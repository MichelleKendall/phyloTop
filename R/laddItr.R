laddItr <- function(ladderNumbers) {
  m <- max(ladderNumbers)
  count <- sum(ladderNumbers==m) # How many of the max there are
  while (m > 0) { # Take off the required number of m's
    newCount <- sum(ladderNumbers==m)
    ladderNumbers <- ladderNumbers[ladderNumbers!=m] # Take off every m
    ladderNumbers <- c(ladderNumbers,rep(m,newCount-count)) # Add on the required number
    m <- m - 1 # Reduce m
  }
  return(ladderNumbers)  
}