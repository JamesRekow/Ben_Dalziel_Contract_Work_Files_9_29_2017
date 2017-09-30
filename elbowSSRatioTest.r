#  James Rekow

elbowSSRatioTest = function(M = 40, numSubgroups = 2, N = 20, iStrength = 1, univ = 1,
                            sigmaMax = 0.1, thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4,
                            tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01, lambda = 0,
                            returnParams = FALSE, conGraph = NULL, rollRange = 0.025,
                            numClust = NULL, centersVec = 1:10){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("subgroupsAbdListCreator.r")
  source("computeInfoWeightMat.r")
  source("computeSSRatio.r")
  
  #  compute abundance list
  abdList = subgroupsAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                    univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                    maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                    interSmplMult = interSmplMult, lambda = lambda,
                                    returnParams = returnParams, conGraph = conGraph)
  
  #  compute matrix of info weights
  infoMat = computeInfoWeightMat(abdList = abdList, rollRange = rollRange)
  
  #  compute SS ratio of clusters for kmeans results using each number of centers in centersVec
  SSRatioVec = sapply(1:length(centersVec), function(n) computeSSRatio(inputMat = infoMat,
                                                                       centers = centersVec[n]))
  
  return(SSRatioVec)
  
} #  end elbowSSRatioTest function
