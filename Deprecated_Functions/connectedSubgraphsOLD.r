#  James Rekow

connectedSubgraphs = function(g, v, numSubgraphs){
  
  #  ARGS: g - graph
  #        v - number of vertices in the connected subgraphs to be returned
  #
  #  RETURNS: connectedList - a list of numeric vectors of vertices forming connected subgraphs
  #           of g, all of which have v vertices, or NULL if no such subgraphs are found
    
  ##  remove after editing
  conGraph = g
  synthM = v
  M = vcount(g)
  ##
  
  #  create adjacency matrix for connectivity graph
  adjMat = as.matrix(get.adjacency(conGraph, type = "both"))
    
  #  make a list to store samples that are not part of any connected subgraphs with
  #  M or more vertices
  unfitSamples = c()
  
  constructSynthChrt = function(synthM, M, adjMat, unfitSamples){
    
    #  samples in the synthetic cohort
    synthSamples = 0 * (1:synthM)
    
    validStartSamples = 1:M
    
    if(length(unfitSamples > 0)){
      validStartSamples = validStartSamples[-unfitSamples]
    }
    
    #  select the first sample
    newSample = sample(validStartSamples, 1)
    
    #  add the first sample to the synthetic cohort
    synthSamples[1] = newSample
    
    #  remove extraneous edges connecting to newSample from the adjacency matrix
    adjMat[newSample, ] = 0
    
    #  samples which are connected to any of the samples in the existing synthetic cohort
    potentialSamples = which(adjMat[ , synthSamples[1]] == 1)
    
    for(ii in 2:synthM){
      
      #  if there are no potential samples, but the synthetic cohort is not large enough, mark all
      #  of the synthSamples produced on this iteration as unfit, as they form a connected subgraph
      #  that is disconnected from all of the other samples, but insufficiently large to create a
      #  synthetic cohort with synthM samples
      if(length(potentialSamples) == 0){
        
        #  return the negative indices of samples if they are not part of any connected synthetic
        #  cohort with at least synthM vertices
        synthSamples = -synthSamples
        break
        
      } #  end if
      
      ##  select a new sample to add to the synthetic cohort
      if(length(potentialSamples) == 1){
        newSample = potentialSamples[1]
      } #  end if
      
      if(length(potentialSamples) > 1){
        newSample = sample(potentialSamples, 1) 
      } #  end if
      
      #  remove extraneous edges connecting to newSample from the adjacency matrix
      adjMat[newSample, ] = 0
      
      #  remove the newly added sample from the vector of potential samples
      potentialSamples = potentialSamples[potentialSamples != newSample]
      
      #  identify new potential samples from non-reduntant edges incident to newSample
      newPotentialSamples = which(adjMat[ , newSample] == 1)
      
      #  add new potential samples to potential samples
      potentialSamples = append(potentialSamples, newPotentialSamples)
      
      #  add the new sample to synthSamples
      synthSamples[ii] = newSample
      
    } #  end while loop
    
    return(synthSamples)
    
  } #  end constructSynthChrt function
  
  while(length(unfitSamples) < (M - synthM)){
    
    synthSamples = constructSynthChrt(synthM = synthM, M = M, adjMat = adjMat,
                                      unfitSamples = unfitSamples)
    
    if(synthSamples[1] < 0){
      unfitSamples = append(unfitSamples, -synthSamples)
      print(unfitSamples)
      print(synthSamples)
      
    } #  end if
    
    else{
      break
    } #  end else
    
  } #  end while loop
  
  if(synthM > (M - length(unfitSamples))){
    warning("No valid synthetic cohorts could be created.")
    return(NULL)
  } #  end if
  
  else{
    #  synthChrt = abdList[synthSamples]
    #  for testing
    return(synthSamples)
    #  end for testing
    return(synthChrt)
  } #  end else
  
} #  end connectedSubgraphs function
