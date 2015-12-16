nodeImb <- function(tree,node) {
  if (nodeType(tree)[node]=='tip') {return(c(0,0))}
  else {
    childs <- children(tree,node)
    left <- length(descendants(tree,childs[1],type='tips'))
    right <- length(descendants(tree,childs[2],type='tips'))
    return(c(left,right))
  }
}