#  James Rekow

identifyNumSubgroupsTest = function(MVec = NULL, NVec = NULL, lambdaVec = NULL, numSubgroupsVec = NULL,
                                    iStrength = 1, univVec = NULL, sigmaMax = 0.1,
                                    thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                    intTime = 1, interSmplMult = 0.01, conGraph = NULL, rollRange = 0.025,
                                    numReplicates = 10){
  
  #  ARGS:
  #
  #  RETURNS: percentErrorDF - data frame containing columns for all the input data and the percent
  #                            of replicates for which identifyNumSubgroups returned the wrong number
  #                            of subgroups
  
  source("subgroupsAbdListCreator.r")
  source("computeInfoWeightMat.r")
  source("identifyNumSubgroups.r")
  
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
  
  correctNumberIdentified = function(inputVec){
    
    #  ARGS: inputVec - a vector of the form c(M, N, numSUbgroups, lambda, univ)
    #
    #  RETURNS: correct - boolean corresponding to whether or not identifyNumSubgroups correctly
    #                     identified the number of subgroups in an abundance list generated using the
    #                     input treatment
    
    #  extract parameter values from inputVec
    M = inputVec[1]
    N = inputVec[2]
    numSubgroups = inputVec[3]
    lambda = inputVec[4]
    univ = inputVec[5]
    
    #  create abundance list using the input parameters for the given treatment
    abdList = subgroupsAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                      univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                      maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                      interSmplMult = interSmplMult, lambda = lambda, returnParams = FALSE,
                                      conGraph = conGraph)
    
    #  compute the info weight matrix for the given abundance list
    infoWeightMat = computeInfoWeightMat(abdList = abdList, rollRange = rollRange)
    
    #  compute percent error of algorithm's subgroup classification
    numSubgroupsGuess = identifyNumSubgroups(infoMat = infoWeightMat)
    
    #  check if the algorithm identified the correct number of subgroups
    correct = numSubgroupsGuess == numSubgroups
    
    return(correct)
    
  } #  end correctNumberIdentified function
  
  percentErrorProducer = function(inputVec){
    
    #  ARGS: inputVec - vector of the form c(M, N, lambda, univ)
    #
    #  RETURNS: percentError - the percent of replicates at each treatment for which identifyNumSubgroups
    #                          returned the incorrect number of subgroups
    
    #  compute whether or not the algorithm identified the correct number of subgroups for the given input
    #  and repeat for numReplicates replicates
    correctGuessVec = replicate(numReplicates, correctNumberIdentified(inputVec = inputVec))
    
    #  compute the percent of replicates for which the algorithm failed
    percentError = 100 * sum(!correctGuessVec) / numReplicates
    
    return(percentError)
    
  } #  end percentErrorProducer function
  
  #  create a matrix of all unique combinations of elements from Mvec, NVec, numSubgroupsVec, lambdaVec,
  #  and univVec
  inputMat = expand.grid(MVec, NVec, numSubgroupsVec, lambdaVec, univVec)
  
  #  for each combination of inputs, compute the percent error of the algorithm using those inputs
  percentError = apply(inputMat, 1, percentErrorProducer)
  
  #  store data in a data frame
  percentErrorDF = data.frame(M = inputMat[ , 1], N = inputMat[ , 2], numSubgroups = inputMat[ , 3],
                              lambda = inputMat[ , 4], univ = inputMat[ , 5], 
                              percentError = percentError)
  
  return(percentErrorDF)
  
} #  end identifyNumSubgroupsTest function
