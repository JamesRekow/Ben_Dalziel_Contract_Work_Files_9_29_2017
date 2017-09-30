#  James Rekow

identifySubgroupsTest2ErrorCheck = function(MVec = NULL, NVec = NULL, lambdaVec = NULL, numSubgroups = 2,
                                            iStrength = 1, univVec = NULL, sigmaMax = 0.1,
                                            thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4,
                                            tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                                            conGraph = NULL, rollRange = 0.025, numReplicates = 10){
  
  #  ARGS:
  #
  #  RETURNS: percentErrorDF - data frame containing columns for all the input data and the average
  #                            percent of samples placed in the wrong subgroup in each treatment
  
  source("identifySubgroupsTest2.r")
  
  #  set default value of MVec
  if(is.null(MVec)){
    MVec = seq(40, 200, 80)
  } #  end if
  
  #  set default value of lambdaVec
  if(is.null(NVec)){
    NVec = seq(20, 100, 40)
  } #  end if
  
  #  set default value of lambdaVec
  if(is.null(lambdaVec)){
    lambdaVec = seq(0, 200, 50)
  } #  end if
  
  #  set default value of lambdaVec
  if(is.null(univVec)){
    univVec = seq(0, 1, 0.5)
  } #  end if
  
  percentErrorProducer = function(inputVec){
    
    #  ARGS: inputVec - a vector of the form c(M, N, lambda, univ)
    #
    #  RETURNS: 
    
    #  extract parameter values from inputVec
    M = inputVec[1]
    N = inputVec[2]
    lambda = inputVec[3]
    univ = inputVec[4]
    
    #  compute algorithm's guess for samples in subgroup 1
    subgroup1Guess = identifySubgroupsTest2(M = M, numSubgroups = numSubgroups, N = N,
                                            iStrength = iStrength, univ = univ, sigmaMax = sigmaMax,
                                            thresholdMult = thresholdMult, maxSteps = maxSteps,
                                            tStep = tStep, intTime = intTime,
                                            interSmplMult = interSmplMult, lambda = lambda,
                                            returnParams = FALSE, conGraph = conGraph,
                                            rollRange = rollRange)
    
    #  compute guess for samples in subgroup 2
    subgroup2Guess = (1:M)[-subgroup1Guess]
    
    #  default indices of the 2 subgroups created from subgroupsAbdListCreator
    subgroup1 = 1:ceiling(M / 2)
    subgroup2 = {ceiling(M / 2)+ 1}:M
    
    #  compute the number of samples in subgroup 1 that were correctly placed
    correctSubgroup1Samples = sum(subgroup1 %in% subgroup1Guess)
    
    #  compute the number of samples in subgroup 2 that were correctly placed
    correctSubgroup2Samples = sum(subgroup2 %in% subgroup2Guess)
    
    #  total number of samples that were placed in the correct subgroup
    numCorrectSamples = correctSubgroup1Samples + correctSubgroup2Samples
    
    #  total number of samples placed in the wrong subgroup
    numIncorrectSamples = M - numCorrectSamples
    
    percentError = 100 * numIncorrectSamples / M
    
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
  inputMat = expand.grid(MVec, NVec, lambdaVec, univVec)
  
  #  for each combination of inputs, compute the percent error of the algorithm using those inputs
  percentError = apply(inputMat, 1, meanPercentErrorProducer)
  
  #  store data in a data frame
  percentErrorDF = data.frame(M = inputMat[ , 1], N = inputMat[ , 2], lambda = inputMat[ , 3],
                              univ = inputMat[ , 4], percentError = percentError)
  
  return(percentErrorDF)
  
} #  end identifySubgroupsTest2ErrorCheck function
