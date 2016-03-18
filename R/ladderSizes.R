#' Ladder sizes
#' 
#' NOTE: known bug!
#' Finds the sizes of ladders in the tree. 
#' A ladder is here defined to be a series of consecutive nodes in the tree,
#' each of which has exactly one tip descendant.
#' 
#' @author Caroline Colijn \email{c.colijn@imperial.ac.uk}
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}
#' @return A list of ladder sizes
#'
#' @import ape  
#' @importFrom igraph graph_from_edgelist
#' @importFrom igraph cluster.distribution
#' @importFrom igraph components
#' 
#' @examples
#' ## Find ladder sizes in a random tree with 20 tips:
#' tree <- rtree(20)
#' ladderSizes(tree)
#' 
#' @export
ladderSizes <- function(tree) {
  tree <- phyloCheck(tree)
  ntip=length(tree$tip.label)
  nn=tree$Nnode
  Ancs=(ntip+1):(ntip+nn) # assumes tips are 1:ntip, then internal nodes
  
  # for each internal node (numbered 1:(ntips-1) in the rows of "pointers"), find its immediate children 
  Pointers=t(vapply(Ancs, function(x) tree$edge[tree$edge[,1]==x,2], FUN.VALUE=c(1,2))) 
  
  # create "DP": like pointers, but any tip is replaced with a zero, any internal node with a 1
  DP=Pointers
  DP[DP<=ntip]=0 
  DP[DP>ntip]=1
  
  BrIsLadder=(rowSums(DP)==1) # which branches (1:nn) are ladders (1 or 0)
  LadderBr=(1:nrow(DP))[BrIsLadder] # vector of only br that are part of a ladder
  LadderNodes=ntip+LadderBr  # vector of NODES (orig node id) that are ladders
  allIsLadder=c(rep(0,ntip),BrIsLadder) # 0 for all tips, then 1 or 0 for internal nodes
  
  EL=rbind( cbind(LadderNodes, Pointers[LadderBr,1]),cbind(LadderNodes,Pointers[LadderBr,2])) # Ancestor, Descendant where Ancestor is a Ladder Node (but fast as no finding in the edgelist).
  ToKeep=allIsLadder[EL[,2]] # 1 only where second col of EL is a ladder node
  EdgeList=EL[ToKeep==1,]     # only keep edges where ancestor and des are ladd
  if (length(EdgeList)==0) {return(list(ladderSizes=0,ladderBr=NULL))}
  else {
  EdgeList <- t(as.data.frame(EdgeList)) # prevents problems when there is only one row
  chEL=matrix(data="hi",nrow=nrow(EdgeList),ncol=2) # coerce to format for graph
  chEL[,1]=paste("v",as.character(EdgeList[,1]),sep="") 
  chEL[,2]=paste("v",as.character(EdgeList[,2]),sep="")
  gr=graph_from_edgelist(chEL)
  ladderSizes <- components(gr)$csize # sizes of different ladder components
  return(list(ladderSizes=ladderSizes,ladderBr=LadderBr) )
  }
}