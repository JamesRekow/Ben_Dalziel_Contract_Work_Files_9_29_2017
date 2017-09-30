#  James Rekow

identifySubgroupsVer2Test = function(M = 40, numSubgroups = 2, N = 20, iStrength = 1, univ = 1,
                                     sigmaMax = 0.1, thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4,
                                     tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01, lambda = 0,
                                     returnParams = FALSE, conGraph = NULL, rollRange = 0.025){
  
  #  ARGS: M - number of samples in cohort
  #        N - number of species in each sample
  #        univ - universality
  #        lambda - rate parameter for exponentially distributed waiting time between interactions
  #        numSubgroups - number of subgroups in cohort
  #        numClust - number of clusters to use in kmeans. If not specified, the number is computed by
  #                   the algorithm
  #
  #  RETURNS: percentError - percent of samples that identifySubgroupsVer1 placed in the wrong subgroup
  
  source("subgroupsAbdListCreator.r")
  source("identifySubgroupsVer2.r")
  source("computeSubgroupVec.r")
  source("countSubgroupVecDiff.r")
  
  #  compute abundance list
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
  
  #  use algorithm to place samples into subgroups
  subgroupsComputed = identifySubgroupsVer2(abdList = abdList, rollRange = rollRange)
  
  #  compute the number of differences between actual and computed subgroup membership
  numDiff = countSubgroupVecDiff(subgroupVec1 = subgroupsActual, subgroupVec2 = subgroupsComputed)
  
  #  compute percent error of algorithms computed subgroup membership vector
  percentError = 100 * numDiff / M
  
  return(percentError)
  
} #  end identifySubgroupsVer2Test function
