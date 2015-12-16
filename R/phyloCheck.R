phyloCheck <- function(object,error=TRUE) {
  if (class(object) != 'phylo4') {
    if (error == TRUE) {
      print('Error in phyloCheck(tree): Object not of class "phylo4"')
      print('Try phy <- as(tree,phylo4) to create an object of the correct type')
    }
    return(FALSE)
  }
  if (nTips(object)-nNodes(object) !=1) {
    if (error == TRUE) {
      print('Error in phyloCheck(tree): The tree is not binary')
    }
    return(FALSE)
  }  
  return(TRUE)
}