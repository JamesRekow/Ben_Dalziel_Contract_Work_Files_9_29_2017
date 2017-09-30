#  James Rekow

rSubgraph = function(g, v){
  
  #  ARGS: g - input graph
  #        v - number of vertices from g to include in the random subgraph
  #
  #  RETRURNS: rSub - a connected subgraph of g with v vertices and all of the edges between them
  
  i = 0
  numVertices = vcount(g)
  rSub = induced.subgraph(g, sample(numVertices, v))
  
  while(!is.connected(rSub)){
    
    rSub = induced.subgraph(g, sample(numVertices, v))
    i = i + 1
    
  } #  end while
  print(i)
  return(rSub)
    
} #  end rSubgraph function
