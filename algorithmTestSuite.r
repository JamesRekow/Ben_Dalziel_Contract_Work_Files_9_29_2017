#  James Rekow

algorithmTestSuite = function(algorithmTestFile, iStrength = 1, sigmaMax = 0.1, thresholdMult = 10 ^ (-1),
                              maxSteps = 10 ^ 4, tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                              conGraph = NULL, rollRange = 0.025, numReplicates = 3, numClustKnown = TRUE){
  
  #  ARGS:
  #
  #  RETURNS: percentErrorDF - data frame containing columns for all the input data and the average
  #                            percent of samples placed in the wrong subgroup in each treatment
  
  source(algorithmTestFile)
  
  algorithmTestCharName = strsplit(algorithmTestFile, "\\.")[[1]][1]
  
  #  source("identifySubgroupsVer1Test_Exp_Weight.r")
  #  numClust = KNOWN
  #
  #  inputs:
  #    1: M = 40, N = 20, lambda = 0, univ = 1, numSubgroups = 2   : standard case
  #    2: M = 100, N = 20, lambda = 0, univ = 1, numSubgroups = 2  : large M
  #    3: M = 40, N = 20, lambda = 0, univ = 1, numSubgroups = 6   : lot's of subgroups
  #    4: M = 100, N = 20, lambda = 100, univ = 1, numSubgroups = 6: large M, many subgrps, interactions
  #    5: M = 40, N = 20, lambda = 0, univ = 0, numSubgroups = 2   : no universality
  
  #  default setting is that number of clusters in each treatment is unknown
  numClust = NULL
  
  input = vector("list", 5)
  input[[1]] = c(40, 20, 2, 0, 1)
  input[[2]] = c(100, 20, 2, 20, 1)
  input[[3]] = c(40, 20, 6, 0, 1)
  input[[4]] = c(100, 20, 6, 100, 1)
  input[[5]] = c(40, 20, 2, 0, 0)
  
  #  create a matrix of all unique combinations of elements from Mvec, NVec, lambdaVec, and univVec
  inputMat = Reduce(rbind, input)
  
  performAlgorithmTest = function(inputVec){
    
    #  ARGS: inputVec - vector of input values from inputMat which are fed to the algorithm to be tested
    #
    #  RETURNS: meanPercentError - the mean percent error of the algorithm with the given inputs
    
    #  extract input values
    MVec = inputVec[1]
    NVec = inputVec[2]
    numSubgroupsVec = inputVec[3]
    lambdaVec = inputVec[4]
    univVec = inputVec[5]
    
    treatmentArgList = list(MVec = MVec, NVec = NVec, numSubgroupsVec = numSubgroupsVec,
                            lambdaVec = lambdaVec, univVec = univVec, iStrength = iStrength,
                            sigmaMax = sigmaMax, thresholdMult = thresholdMult, maxSteps = maxSteps,
                            tStep = tStep, intTime = intTime, interSmplMult = interSmplMult,
                            conGraph = conGraph, rollRange = rollRange, numReplicates = numReplicates,
                            numClustKnown = numClustKnown)
    
    meanPercentError = do.call(algorithmTestCharName, args = treatmentArgList)[[6]]
    
    return(meanPercentError)

  } #  end performAlgorithmTest function
  
  meanPercentErrorVec = apply(inputMat, 1, performAlgorithmTest)
  
  meanPercentErrorDF = data.frame(M = inputMat[ , 1], N = inputMat[ , 2], numSubgroups = inputMat[ , 3],
                                  lambda = inputMat[ , 4], univVec = inputMat[ , 5],
                                  meanPercentError = meanPercentErrorVec)
  
  return(meanPercentErrorDF)
  
} #  end algorithmTestSuite function
