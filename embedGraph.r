#  James Rekow

embedGraph = function(M, intGraph){
  
  #  ARGS: M - number of samples in cohort. Total number of vertices in conGraph
  #        intGraph - connectivity graph describing the connections between interacting
  #                   samples in the cohort
  #
  #  RETURNS: conGraph - connectivity graph describing the connections between all samples
  #                      in the cohort. This adds M - numI vertices with no incident edges
  #                      to intGraph and then randomizes the vertex labels (vertex index)
  
  #  create base graph with M vertices and no edges
  baseGraph = make_empty_graph(n = M, directed = FALSE)
  
  #  extract the number of interacting samples
  numI = length(V(intGraph))
  
  #  select the indices of interacting samples
  intVertices = sample(M, numI)
  
  #  extract a vector of edges (every two numbers forms an edge) from intGraph
  intGraphEdges = c(t(ends(intGraph, E(intGraph))))
  
  #  relabel the indices of intGraphEdges so they are now the indices of the interacting
  #  samples
  conGraphEdges = sapply(intGraphEdges, function(x) intVertices[x])
  
  #  add these new edges to the base graph
  conGraph = add_edges(baseGraph, edges = conGraphEdges)
  
  return(conGraph)
  
} #  end embedGraph function
