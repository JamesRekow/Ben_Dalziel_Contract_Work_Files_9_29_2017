#  James Rekow

subgroupAbdListCreator = function(M = 40, numSubgroups = 1, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
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
  source("eulerIntegrate.r")
  
  #  used to determine if the system is near equilibrium
  threshold = {thresholdMult * tStep} ^ 2
  
  #  define integrator function to apply eulerIntegrate with desired parameter vals
  integrator = function(smpl){
    return(eulerIntegrate(smpl, threshold = threshold, maxSteps = maxSteps, tStep = tStep))
  } #  end integrator function
  
  #  create vector with the number of samples in each subgroup
  subgroupSizeVec = partition(n = M, numPartitions = numSubgroups)
  
  createSubgroup = function(subgroupSize){
    
    #  ARGS: subgroupSize - number of samples in subgroup
    #
    #  RETURNS: subgroup - a subgroup with subgroupSize samples. This is just an un-integrated cohort with
    #                      the given number of samples
    
    #  construct subgroup
    subgroup = cohortCreator(M = subgroupSize, N = N, iStrength = iStrength, univ = univ,
                             sigmaMax = sigmaMax)
    
    return(subgroup)
    
  } #  end createSubgroup function
  
  #  create subgroups
  subgroupList = lapply(as.list(subgroupSizeVec), createSubgroup)
  
  #  combine subgroups into a single cohort
  chrt = unlist(subgroupList, recursive = FALSE)
  
  #  integrate samples in the cohort
  abdList = lapply(chrt, integrator)
  
  #  simulate intra-cohort interaction if lambda > 0
  if(lambda > 0){
    
    #  update the abundances of each sample in the cohort
    for(i in 1:M){
      chrt[[i]][[1]] = abdList[[i]]
    } #  end for
    
    #  simulate intra-cohort interactions
    abdList = intraCohortInteraction(chrt, M = M, N = N, lambda = lambda, threshold = threshold,
                                     maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                     interSmplMult= interSmplMult, conGraph = conGraph)
    
  } #  end if
  
  return(abdList)
  
} #  end subgroupAbdListCreator function
