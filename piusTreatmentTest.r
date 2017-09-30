#  James Rekow

piusTreatmentTest  = function(M = 40, numSubgroups = 1, N = 20, iStrength = 1, univ = 1,
                              sigmaMax = 0.1, thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4,
                              tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01, lambda = 0,
                              returnParams = FALSE, conGraph = NULL, rollRange = 0.025, numClust = NULL,
                              returnNumSubgroupsComputed = FALSE){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("subgroupsAbdListCreator.r")
  source("computeSubgroupVec.r")
  source("pius.r")
  source("countSubgroupVecDiff.r")
  
  #  create abundance list using treatment input parameters
  abdList = subgroupsAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                    univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                    maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                    interSmplMult = interSmplMult, lambda = lambda,
                                    returnParams = returnParams, conGraph = conGraph)
  
  #  compute default subgroup vector
  subgroupsActual = computeSubgroupVec(M = M, numSubgroups = numSubgroups)
  
  #  create reordering to randomize subgroup structure (to randomly reorder samples in cohort and perform
  #  the same reordering on the default subgroup vector)
  reordering = sample(M)
  
  #  perform randomization
  abdList = abdList[reordering]
  subgroupsActual = subgroupsActual[reordering]
  
  #  compute algorithm's guess at subgroup vector
  subgroupsComputed = pius(abdList, rollRange = rollRange, numClust = numClust)
  
  #  compute the number of differences between actual and computed subgroup membership
  numDiff = countSubgroupVecDiff(subgroupVec1 = subgroupsActual, subgroupVec2 = subgroupsComputed)
  
  #  compute percent error of algorithms computed subgroup membership vector
  percentError = 100 * numDiff / M
  
  #  if returnNumSubgroupsComputed is set to TRUE, return a list with the percent error and the number of
  #  subgroups (clusters) identified by the pius algorithm
  if(returnNumSubgroupsComputed){
    
    numSubgroupsComputed = length(unique(subgroupsComputed))
    
    returnVec = c(percentError = percentError, numSubgroupsComputed = numSubgroupsComputed)
    
    return(returnVec)
    
  } #  end if
  
  return(percentError)
  
} #  end  piusTreatmentTest function
