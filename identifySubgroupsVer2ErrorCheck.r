#  James Rekow

identifySubgroupsVer2ErrorCheck = function(MVec = NULL, NVec = NULL, lambdaVec = NULL,
                                           numSubgroupsVec = NULL, iStrength = 1, univVec = NULL,
                                           sigmaMax = 0.1, thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4,
                                           tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                                           conGraph = NULL, rollRange = 0.025, numReplicates = 10, ...){
  
  #  ARGS:
  #
  #  RETURNS: percentErrorDF - data frame containing columns for all the input data and the average
  #                            percent of samples placed in the wrong subgroup in each treatment
  
  source("identifySubgroupsVer2Test.r")
  
  #  set default value of MVec
  if(is.null(MVec)){
    MVec = seq(40, 200, 80)
  } #  end if
  
  #  set default value of NVec
  if(is.null(NVec)){
    NVec = seq(20, 100, 40)
  } #  end if
  
  #  set default value of numSubgroupsVec
  if(is.null(numSubgroupsVec)){
    numSubgroupsVec = seq(1, 4, 1)
  } #  end if
  
  #  set default value of lambdaVec
  if(is.null(lambdaVec)){
    lambdaVec = seq(0, 200, 50)
  } #  end if
  
  #  set default value of univVec
  if(is.null(univVec)){
    univVec = seq(0, 1, 0.5)
  } #  end if
  
  percentErrorProducer = function(inputVec){
    
    #  ARGS: inputVec - a vector of the form c(M, N, numSUbgroups, lambda, univ)
    #
    #  RETURNS: 
    
    #  extract parameter values from inputVec
    M = inputVec[1]
    N = inputVec[2]
    numSubgroups = inputVec[3]
    lambda = inputVec[4]
    univ = inputVec[5]
    
    #  compute percent error of algorithm's subgroup classification
    percentError = identifySubgroupsVer2Test(M = M, numSubgroups = numSubgroups, N = N,
                                             iStrength = iStrength, univ = univ, sigmaMax = sigmaMax,
                                             thresholdMult = thresholdMult, maxSteps = maxSteps,
                                             tStep = tStep, intTime = intTime, interSmplMult = interSmplMult,
                                             lambda = lambda, returnParams = FALSE, conGraph = NULL,
                                             rollRange = rollRange)
    
    return(percentError)
    
  } #  end percentErrorProducer function
  
  meanPercentErrorProducer = function(inputVec){
    
    #  ARGS: inputVec - vector of the form c(M, N, lambda, univ)
    #
    #  RETURNS: meanPercentError - mean percent error of algorithm guesses at subgroup composition for
    #                              the given input vector
    
    #  compute the percent error of the algorithm in numReplicates replicates for the given inputs
    errorVec = replicate(numReplicates, percentErrorProducer(inputVec = inputVec))
    
    #  compute the mean percent error of the algorithm for the given inputs
    meanPercentError = mean(errorVec)
    
    return(meanPercentError)
    
  } #  end meanPercentErrorProducer function
  
  #  create a matrix of all unique combinations of elements from Mvec, NVec, lambdaVec, and univVec
  inputMat = expand.grid(MVec, NVec, numSubgroupsVec, lambdaVec, univVec)
  
  #  for each combination of inputs, compute the percent error of the algorithm using those inputs
  meanPercentError = apply(inputMat, 1, meanPercentErrorProducer)
  
  #  store data in a data frame
  meanPercentErrorDF = data.frame(M = inputMat[ , 1], N = inputMat[ , 2], numSubgroups = inputMat[ , 3],
                                  lambda = inputMat[ , 4], univ = inputMat[ , 5], 
                                  meanPercentError = meanPercentError)
  
  return(meanPercentErrorDF)
  
} #  end identifySubgroupsVer2ErrorCheck function
