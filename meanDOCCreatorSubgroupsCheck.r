#  James Rekow

meanDOCCreatorSubgroupsCheck = function(MVec = NULL, lambdaVec = NULL, subgroupVec = NULL, N = 20,
                                        iStrength = 1, univ = 1, sigmaMax = 0.1,
                                        thresholdMult = 10 ^ (-4), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                        intTime = 1, interSmplMult = 0.01){
  
  #  ARGS: MVec - vector of M values to use when creating cohorts
  #        lambdaVec - vector of lambda values to use when creating cohorts
  #        subgroupVec - vector of values controlling how many subgroups the created cohort will have.
  #                      Samples in different subgroups are created as if they were from different cohorts,
  #                      but interact together as a single cohort.
  #
  #  RETURNS: meanDOC - data frame containing the mean overlap and mean dissimilarity of DOC points from
  #                     cohorts created from each treatment of MVec crossed with lambdaVec crossed with
  #                     subgroupVec. Also contains a row with the value of lambda / M and a row
  #                     called fitCurve with the value of 1 - exp(-lambda / M) for each cohort.
  
  source("DOCProcedure.r")
  source("subgroupsAbdListCreator.r")
  library(igraph)
  
  #  set default value of MVec
  if(is.null(MVec)){
    MVec = seq(40, 200, 80)
  } #  end if
  
  #  set default value of lambdaVec
  if(is.null(lambdaVec)){
    lambdaVec = seq(0, 200, 50)
  } #  end if
  
  #  set default value of subgroupVec
  if(is.null(subgroupVec)){
    #  total number of groups in the cohort, so 1 subgroup indicates that all samples in the cohort
    #  have the same interaction matrix
    subgroupVec = c(1, 2, 3, 5, 10)
  } #  end if
  
  meanVecProducer = function(inputVec){
    
    #  ARGS: inputVec - a vector of the form c(M, lambda, numSubgroups)
    #
    #  RETURNS: meanVec - a vector of the form c(meanOver, meanDiss) containing the mean overlap and
    #                     mean dissimilarity of DOC points from a cohort created with the values of M
    #                     lambda, and numSubgroups specified by inputVec. The number of samples in each
    #                     subgroup is equal to floor(M / numSubgroups) or floor(M / numSubgroups) + 1,
    #                     depending on divisibility.
    
    #  extract parameter values
    M = inputVec[1]
    lambda = inputVec[2]
    numSubgroups = inputVec[3]
    
    #  create abdList with parameter values
    abdList = fracturedAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                      univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                      maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                      interSmplMult = interSmplMult, lambda = lambda, returnParams = FALSE,
                                      conGraph = NULL)
    
    #  compute DOC
    doc = DOCProcedure(abdList)
    
    #  compute mean overlap and dissimilarity
    meanOver = mean(doc[[1]])
    meanDiss = mean(doc[[2]])
    
    #  store means in a vector
    meanVec = c(meanOver, meanDiss)
    
    return(meanVec)
    
  } #  end meanVecProducer function
  
  #  create a matrix of all unique combinations of elements from Mvec, lambdaVec, and subgroupVec
  inputMat = expand.grid(MVec, lambdaVec, subgroupVec)
  
  #  for each combination of inputs, compute the mean overlap and dissimilarity of a DOC produced using
  #  those input values and store these means in a matrix
  meanMat = apply(inputMat, 1, meanVecProducer)
  
  #  extract mean overlap and mean dissimilarity from meanMat
  meanOver = meanMat[1, ]
  meanDiss = meanMat[2, ]
  
  #  store data in a data.frame
  meanDOC = data.frame(M = inputMat[ , 1], lambda = inputMat[ , 2], numSubgroups = inputMat[ , 3],
                       meanOver = meanOver, meanDiss = meanDiss)
  
  #  add a column storing the value of lambda / M for each cohort
  meanDOC$lambdaOverM = meanDOC$lambda / meanDOC$M
  
  #  add a column storing the value of the 1 - exp(-lambda / M) for each cohort
  meanDOC$overFitCurve = 1 - exp(-meanDOC$lambdaOverM)
  
  return(meanDOC)
  
} #  end meanDOCCreatorSubgroupsCheck function
