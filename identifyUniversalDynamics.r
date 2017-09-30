#  James Rekow

identifyUniversalDynamics = function(M = 40, N = 20, iStrength = 1, univ = 1,
                                     sigmaMax = 0.1, thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4,
                                     tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01, lambda = 0,
                                     returnParams = FALSE, conGraph = NULL, rollRange = 0.025){
  
  #  ARGS: M, N, univ, lambda
  #
  #  RETURNS:
  
  #  compute abundance list
  abdList = subgroupsAbdListCreator(M = M, N = N, iStrength = iStrength,
                                    univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                    maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                    interSmplMult = interSmplMult, lambda = lambda,
                                    returnParams = returnParams, conGraph = conGraph)
  
  #  use algorithm to place samples into subgroups
  subgroupsComputed = identifySubgroupsVer1(abdList = abdList, rollRange = rollRange, numClust = NULL)
  
  numSubgroups = length(unique(subgroupsComputed))
  
  return(numSubgroups)
  
} #  end identifyUniversalDynamics function
