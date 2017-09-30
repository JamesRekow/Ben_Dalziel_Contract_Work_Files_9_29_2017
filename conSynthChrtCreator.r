#  James Rekow

conSynthChrtCreator = function(synthM, conGraph, abdList, int = TRUE, numSample = 100, maxIters = 100,
                               numSynthChrt = 10){
  
  #  ARGS:
  #
  #  RETURNS: synthChrt - a list of numSynthChrt sublists. Each sublist is the abundance list for a
  #                       synthetic cohort
  
  source("connectedSubgraphs.r")
  source("disconnectedSubgraphs.r")
  source("disconnectedSubgraphs.r")
  
  if(int){
    synthChrtIx = connectedSubgraphs(g = conGraph, v = synthM, numSubgraphs = numSynthChrt)
  } #  end if interacting
  
  if(!int){
    synthChrtIx = disconnectedSubgraphs(g = conGraph, v = synthM, numSubgraphs = numSynthChrt,
                                        numSample = numSample, maxIters = maxIters)
  } #  end if not interacting
  
  abdSelector = function(ixVec) abdList[ixVec]
  
  synthChrt = lapply(synthChrtIx, abdSelector)
  
  return(synthChrt)
  
} #  end conSynthChrtCreator function
