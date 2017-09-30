#  James Rekow

treatmentArrayListCreator = function(M = 80, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                           thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                           intTime = 1, interSmplMult = 0.01, lambda = 10 ^ (-1),
                                           synthM = floor(M / 4), numSynthChrt = 10, numSample = 100,
                                           maxIters = 100){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("conGraphCreator.r")
  source("DOCNSDFListCreator.r")
  
  ##  define tests to apply to each treatment
  
  se = function(x){
    
    #  ARGS: x - a numeric vector
    #
    #  RETURNS: stdError - the standard error of x
    
    stdError = sd(x) / sqrt(length(x))
    
    return(stdError)
    
  } #  end se function
  
  correctMeans = function(DFList) mean(DFList[[1]]$x) < mean(DFList[[2]]$x) 
  
  seTestDiff = function(DFList){
    
    over1 = DFList[[1]]$x
    over2 = DFList[[2]]$x
    sigDiff = abs(mean(over1) - mean(over2)) > max(se(over1), se(over2))
    
    return(sigDiff)
    
  } #  end seTestDiff function
  
  tTestDiff = function(DFList){

    over1 = DFList[[1]]$x
    over2 = DFList[[2]]$x
    sigDiff = t.test(over1, over2)$p.value < 0.05
    
    return(sigDiff)
    
  } #  end tTestDiff function
  
  ksTestDiff = function(DFList){

    over1 = DFList[[1]]$x
    over2 = DFList[[2]]$x
    sigDiff = ks.test(over1, over2)$p.value < 0.05
    
    return(sigDiff)
    
  } #  end ksTestDiff function
  
  #  store the three test functions in a vector
  testVec = c(seTestDiff, tTestDiff, ksTestDiff)
  
  # lambdaVec = c(0.05, 0.1, 0.15)
  # graphTypeVec = c("smallWorld", "scaleFree", "gnp", "gnm")
  # meanDegreeVec = c(2, 4, 6)
  
  lambdaVec = c(0.05, 0.1, 0.15)
  graphTypeVec = c("smallWorld", "scaleFree", "gnp", "gnm")
  meanDegreeVec = c(4, 6)
  
  x1 = 1:length(lambdaVec)
  x2 = 1:length(graphTypeVec)
  x3 = 1:length(meanDegreeVec)
  
  g = function(x, y) sapply(1:length(x), function(n) list(append(x[[n]], y[[n]])))
  
  coordArray = outer(x1, outer(x2, x3, g), g)
  
  countSuccessfulTests = function(coords){
    
    #  ARGS:
    #
    #  RETURNS:
    
    #  unlist the coordinates in the input
    coords = unlist(coords)
    
    #  extract the parameter values corresponding to the coordinate vector
    lambda = lambdaVec[coords[1]]
    graphType = graphTypeVec[coords[2]]
    meanDegree = meanDegreeVec[coords[3]]
    
    # NOTE: coords in array of the from c(lambda, graphType, meanDegree)
    
    #  create connectivity graph using the graph type and mean degree parameters
    conGraph = conGraphCreator(graphType = graphType, meanDegree = meanDegree, v = M)
    
    testVecCreator = function(x = NULL){
      
      DOCNSDFList = DOCNSDFListCreator(conGraph = conGraph, M = M, N = N, iStrength = iStrength,
                                       univ = univ, sigmaMax = sigmaMax,
                                       thresholdMult = thresholdMult, maxSteps = maxSteps,
                                       tStep = tStep, intTime = intTime,
                                       interSmplMult = interSmplMult, lambda = lambda,
                                       synthM = synthM, numSynthChrt = numSynthChrt,
                                       numSample = numSample, maxIters = maxIters)
      
      cMean = correctMeans(DOCNSDFList)
      
      #  store the test results in a vector
      #  accurateTestVec is of the form c(se, t, ks)
      accurateTestVec = sapply(testVec, function(f) f(DOCNSDFList))
      
      successfulTestVec = cMean & accurateTestVec
      
      return(successfulTestVec)
      
    } #  end testVecCreator function
    
    testVecList = lapply(as.list(1:10), testVecCreator)
    
    successfulTestCounter = function(n) sum(sapply(testVecList, "[[", n))
    
    #  output is a list of the form list(se, t, ks)
    successfulTests = lapply(as.list(1:3), successfulTestCounter)
    
    return(successfulTests)
    
  } #  end countSuccessfulTests function
  
  successfulTestCounts = apply(coordArray, 1:3, countSuccessfulTests)
  
  extractResults = function(n){
    
    extractor = function(listedTestVec) unlist(listedTestVec[n])
    
    testResults = apply(successfulTestCounts, 1:3, extractor)
    
    return(testResults)
    
  } #  end extractResults function
  
  testResultsList = lapply(as.list(1:3), extractResults)
  names(testResultsList) = c("seTest", "tTest", "ksTest")
  
  return(testResultsList)
  
} #  end treatmentArrayListCreator function
