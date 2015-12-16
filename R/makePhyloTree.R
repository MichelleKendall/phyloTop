makePhyloTree <- function(Edges, Lengths, Root) {
  G <- graph(edges=t(Edges));
  orderT <- graph.dfs(G,Root)$order; 
  newLengths=0*Lengths; 
  
  oE<-order(Edges[,2]); Edges <- Edges[oE, ];  Lengths <-Lengths[oE]; 
  newEdges <- matrix(NA, nrow(Edges), ncol(Edges)) 
  ooT<-order(orderT[-1]);
  newEdges[ooT,] <- Edges
  newLengths[ooT]= Lengths
  # newLengths[newLengths==0]=0.01
  
  Nnode <- (length(orderT)-1)/2;
  Ntips <- Nnode+1; 
  pt <- rtree(Ntips); 
  pt$Nnode <- Nnode; #
  pt$edge <-  newEdges;
  pt$edge.length=newLengths;
  pt$tip.label=paste("t",1:Ntips,sep="")
  return(as(pt,"phylo4"))
}