#  James Rekow

disconnectedSubgraphs = function(g, v, numSubgraphs, numSample = NULL){
  
  #  ARGS: g - input graph
  #        v - number of vertices in the output graphs
  #
  #  RETURNS: disconnectedList - a list of all disconnected subgraphs of g with v vertices, or NULL
  #                              if no such subgraphs exist. That is, each subgraph of g with v
  #                              vertices is included if and only if it's adjacency matrix is the
  #                              v x v zero matrix
  
  #  create adjacency matrix for connectivity graph
  adjMat = as.matrix(get.adjacency(conGraph, type = "both"))
  
  #  STEP 1: create new list
  #  STEP 2: remove elements of list which don't have 0 adjacency matrices
  
  #  get number of vertices in g
  numVertices = vcount(g)
  
  pairMat = combn(numVertices, 2)
  
  #  create a list of all pairs of vertices
  vList = lapply(as.list(1:choose(numVertices, 2)), function(i) pairMat[ , i])
  
  isDisconnected = function(vVec){
    
    #  ARGS: vVec - vector of vertices contained in subgraph, where it is known that vVec without
    #               it's last element forms a completely disconnected subgraph
    #
    #  RETURNS: TRUE/FALSE depending on whether or not the vertices in vVec form a completely
    #           disconnected subgraph
    
    numV = length(vVec)
    disconnectedV = vVec[-numV]
    disconnected = !sum(adjMat[ , vVec[numV]][disconnectedV])
    
    return(disconnected)
    
  } #  end isDisconnected function
  
  vList = vList[unlist(lapply(vList, isDisconnected))]
  
  createNewVList = function(vList, numVertices){
    
    #  ARGS: vList - input vList
    #
    #  RETURNS: newVList - a list of all of the elements in vList with each element of 1:M not in
    #           a given vList appended to vList separately
    
    expandVVec = function(vVec, numVertices){
      
      baseV = 1:numVertices
      newV = baseV[-which(baseV %in% vVec)]
      vAppendedList = lapply(as.list(newV), function(x) append(vVec, x))
      
      return(vAppendedList)
      
    } #  end expandVVec function
    
    newVList = unlist(lapply(vList, function(vVec) expandVVec(vVec = vVec, numVertices = numVertices)),
                      recursive = FALSE)
    
    return(newVList)
    
  } #  end createNewVList function
  
  
  if(length(vList) == 0){
    return(NULL)
  } #  end if
  
  numCurrentV = 2
  
  while(numCurrentV < v){
    
    if(length(vList) == 0){
      return(NULL)
    } #  end if
    
    vList = createNewVList(vList = vList, numVertices = numVertices)
    
    #  faster to do this after removing duplicate elements, but sorting destroys the assumed property
    #  in isDisconnected that the first n - 1 elements of vVec are already disconnected
    vList = vList[unlist(lapply(vList, isDisconnected))]  #  SLOWS it down a lot. Fix in another manner
    
    if(!is.null(numSample)){
      vList = sample(vList, numSample, replace = TRUE)
    } #  end if
    
    vList = vList[which(vList %in% unique(lapply(vList, sort)))]
    
    if(length(vList) == 0){
      vList = NULL
      break
    } #  end if
    
    numCurrentV = numCurrentV + 1
    
  } #  end for loop
  
  return(vList)
  
} #  end disconnectedSubgraphs function
