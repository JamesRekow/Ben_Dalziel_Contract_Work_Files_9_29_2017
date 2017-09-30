#  James Rekow

elbowSSEDFNumSubgroupsCreator  = function(MVec = NULL, numSubgroupsVec = NULL, lambdaVec = NULL,
                                   numReplicates = 10){
  
  #  ARGS:
  #
  #  RETURNS:
  
  #  NOTES: first vector entry for each input is "good", second is "ok", third is "bad"
  
  source("elbowSSETest.r")
  
  #  set default value for MVec
  if(is.null(MVec)){
    MVec = c(40, 40, 40)
  } #  end if
  
  #  set default value for numSubgroupsVec
  if(is.null(numSubgroupsVec)){
    numSubgroupsVec = c(6, 6, 6)
  } #  end if
  
  #  set default value for lambdaVec
  if(is.null(lambdaVec)){
    lambdaVec = c(0, 0, 0)
  } #  end if
  
  createSSEReplicate = function(replicateID){
    
    #  ARGS:
    #
    #  RETURNS: SSEReplicateDF - data frame corresponding to a single replicate
    
    SSE1 = elbowSSETest(M = MVec[1], numSubgroups = numSubgroupsVec[1], lambda = lambdaVec[1])
    SSE2 = elbowSSETest(M = MVec[2], numSubgroups = numSubgroupsVec[2], lambda = lambdaVec[2])
    SSE3 = elbowSSETest(M = MVec[3], numSubgroups = numSubgroupsVec[3], lambda = lambdaVec[3])
    
    SSE = c(SSE1, SSE2, SSE3)
    
    len = length(SSE1)
    numSubgroups1 = rep(numSubgroupsVec[1], len)
    numSubgroups2 = rep(numSubgroupsVec[2], len)
    numSubgroups3 = rep(numSubgroupsVec[3], len)
    
    numSubgroups = as.factor(c(numSubgroups1, numSubgroups2, numSubgroups3))
    
    numCenters = rep(1:len, 3)
    
    replicateIDVec = rep(replicateID, {3 * len})
    
    runVec1 = rep(1, len)
    runVec2 = rep(2, len)
    runVec3 = rep(3, len)
    runVec = c(runVec1, runVec2, runVec3)
    
    runID = as.factor(as.integer(paste(as.character(replicateIDVec), as.character(runVec), sep = "")))
    
    SSEDF = data.frame(runID = runID, numCenters = numCenters, SSE = SSE, numSubgroups = numSubgroups)
    
    return(SSEDF)
    
  } #  end createSSEReplicate function
  
  SSEDFList = lapply(as.list(1:numReplicates), createSSEReplicate)
  
  SSEDF = Reduce(rbind, SSEDFList)
  
  return(SSEDF)
  
} #  end  elbowSSEDFNumSubgroupsCreator function
