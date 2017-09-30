#  James Rekow

isDisconnectedSubgraph = function(vVec, g){
  
  #  ARGS: vVec - numerical vector of vertices
  #        g - graph
  #
  #  RETURNS: TRUE if the subgraph of g formed by the vertices in vVec has no edges, FALSE otherwise
  
  #  NOTE: this function is meant primarily for testing purposes, not for use
  #        in other functions, as it is probably too slow to apply many times.
  
  subgraph = induced.subgraph(g, vVec)
  
  adjMat = as.matrix(get.adjacency(subgraph))
  
  disconnected = all(adjMat == 0)
  
  return(disconnected)
  
} #  end isCompletelyDisconnected function
