#  James Rekow

disconnectedSubgraphs = function(g, v, numSubgraphs, numSample = 100, maxIters = 100){
  
  #  ARGS: g - input graph
  #        v - number of vertices in the output graphs
  #
  #  RETURNS: vList - a list of numSample disconnected subgraphs of g with v vertices, or NULL
  #                   if no such subgraphs exist. That is, each element of v is a vector of
  #                   vertices corresponding to a subgraph whose adjacency matrix is the
  #                   v x v zero matrix
  
  #  create adjacency matrix for connectivity graph
  adjMat = as.matrix(get.adjacency(g, type = "both"))
  
  #  get number of vertices in g
  numVertices = vcount(g)
  
  pairMat = combn(numVertices, 2)
  
  produceSubgraphs = function(x = NULL){
    
    #  create a list of all pairs of vertices
    vList = lapply(as.list(1:choose(numVertices, 2)), function(i) pairMat[ , i])
    
    isDisconnected = function(vVec){
      
      #  ARGS: vVec - vector of vertices contained in subgraph, where it is known that vVec without
      #               it's last element forms a completely disconnected subgraph
      #
      #  RETURNS: TRUE/FALSE depending on whether or not the vertices in vVec form a completely
      #           disconnected subgraph
      
      #  number of vertices in vVec
      numV = length(vVec)
      
      #  create vector of vertices which are already known to form a completely disconnected subgraph
      disconnectedV = vVec[-numV]
      
      #  check whether there are any edges connecting the newest vertex to the previous disconnected
      #  vertices
      disconnected = !any(adjMat[ , vVec[numV]][disconnectedV] == 1)
      
      return(disconnected)
      
    } #  end isDisconnected function
    
    #  remove all vertex vectors which correspond to subgraphs containing at least one edge from vList
    vList = vList[unlist(lapply(vList, isDisconnected))]
    
    createNewVList = function(vList, numVertices){
      
      #  ARGS: vList - input vList
      #
      #  RETURNS: newVList - a list of all of the elements in vList with each element of 1:M not in
      #           a given vList appended to vList separately
      
      expandVVec = function(vVec, numVertices){
        
        #  create vector of potential vertices
        baseV = 1:numVertices
        
        #  select elements from baseV which are not already present in vVec
        #newV = baseV[-which(baseV %in% vVec)] - deprecated
        newV = baseV[-vVec]
        
        #  create a list of new vertex vectors, where each element is a vector that is vVec with one
        #  element of newV appended to it, and each element in newV corresponds to exactly one vector
        #  in this newly created list
        vAppendedList = lapply(as.list(newV), function(x) append(vVec, x))
        
        return(vAppendedList)
        
      } #  end expandVVec function
      
      #  create new list of vertex vectors
      newVList = unlist(lapply(vList, function(vVec) expandVVec(vVec = vVec,
                                                      numVertices = numVertices)), recursive = FALSE)
      
      return(newVList)
      
    } #  end createNewVList function
    
    #  return NULL if every pair of vertices has an edge between them (i.e. g is completely
    #  connected)
    if(length(vList) == 0){
      return(NULL)
    } #  end if
    
    #  track the number of vertices in each vector of vertices in vList
    numCurrentV = 2
    
    #  grow the size of the vertex vectors in vList iteratively
    while(numCurrentV < v){
      
      #  if there are no vectors in vList, return NULL
      if(length(vList) == 0){
        return(NULL)
      } #  end if
      
      #  append new values to each vector in vList
      vList = createNewVList(vList = vList, numVertices = numVertices)
      
      #  remove duplicate vectors (vectors corresponding to the same set of vertices)
      #  unnecessary and very slow
      #vList = uniqueSet(vList = vList, select = "ran")
      
      #  remove vectors from vList that correspond to subgraphs with at least one edge
      vList = vList[unlist(lapply(vList, isDisconnected))]
      
      #  if a value for numSample is specified, randomly select that many vectors from vList at each step
      #  this 
      if(!is.null(numSample)){
        vList = sample(vList, numSample, replace = TRUE)
      } #  end if
      
      if(length(vList) == 0){
        vList = NULL
        break
      } #  end if
      
      numCurrentV = numCurrentV + 1
      
    } #  end while loop
    
    return(vList)
    
  } #  end produceSubgraphs function
  
  #  list to store vertex vectors
  vList = list()
  
  #  count the number of iterations so the function can terminate if it iterates too many times
  iter = 0
  
  #  produce the vertex vectors for vList
  while(length(vList) < numSubgraphs && iter <= maxIters){
    
    newSubs = produceSubgraphs()
    
    vList = append(vList, newSubs)
    
    iter = iter + 1
    
  } #  end while loop
  
  #  randomly select the desired number of vertex vectors from the created vList (which may
  #  be too long before this step)
  vList = sample(vList, numSubgraphs)
  
  return(vList)
  
} #  end disconnectedSubgraphs function
