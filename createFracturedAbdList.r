#  James Rekow

createFracturedAbdList = function(M = 40, numSubgroups = 1, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                  thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                  intTime = 1, interSmplMult = 0.01, lambda = 0, returnParams = FALSE,
                                  conGraph = NULL){
  
  #  ARGS: numSubgroups - the number of subgroups into which the cohort will be divided. If this value
  #                       is greater than one then each subgroup is created as a separate cohort before
  #                       being combined and interacted as a single cohort
  #
  #  RETURNs: abdList - list of integrated and interacted (if lambda > 0) abundances
  
  source("cohortCreator.r")
  source("intraCohortInteraction.r")
  source("partition.r")
  
  #  create vector with the number of samples in each subgroup
  subgroupSizeVec = partition(n = M, numPartitions = numSubgroups)
  
  createSubgroup = function(subgroupSize){
    
    subgroup = cohortCreator(M = subgroupSize, N = N, iStrength = iStrength, univ = univ,
                             sigmaMax = sigmaMax)
    
    return(subgroup)
    
  } #  end createSubgroup function
  
  subgroupList = lapply(as.list(subgroupSizeVec), createSubgroup)
  chrt = unlist(subgroupList, recursive = FALSE)
  
  
  
} #  end createFracturedAbdList function
