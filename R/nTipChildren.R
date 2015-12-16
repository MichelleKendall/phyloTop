nTipChildren <- function(tree,node){
  childs <- children(tree,node)
  return(sum(nodeType(tree)[childs]=='tip'))  
}