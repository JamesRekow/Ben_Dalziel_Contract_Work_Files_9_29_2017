#  James Rekow

meanOverDOCCreator = function(MVec = NULL, lambdaVec = NULL, N = 20, iStrength = 1, univ = 1,
                              sigmaMax = 0.1, thresholdMult = 10 ^ (-4), maxSteps = 10 ^ 4,
                              tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01){
  
  source("abdListCreator.r")
  source("DOCProcedure.r")
  
  
  if(is.null(MVec)){
    
    MVec = seq(40, 200, 40)
    
  } #  end if
  
  if(is.null(lambdaVec)){
    
  lambdaVec = seq(0, 200, 50)
    
  } #  end if
  
  meanOverProducer = function(inputVec){
    
    M = inputVec[1]
    lambda = inputVec[2]
    
    abdList = abdListCreator(M = M, N = N, iStrength = iStrength, univ = univ, sigmaMax = sigmaMax,
                             thresholdMult = thresholdMult, maxSteps = maxSteps, tStep = tStep,
                             intTime = intTime, interSmplMult = interSmplMult, lambda = lambda,
                             returnParams = FALSE, conGraph = NULL)
    
    doc = DOCProcedure(abdList)
    meanOver = mean(doc[[1]])
    
    return(meanOver)
    
  } #  end meanOVerProducer function
  
  inputMat = expand.grid(MVec, lambdaVec)
  meanOver = apply(inputMat, 1, meanOverProducer)
  
  meanOverDOC = data.frame(M = inputMat[ , 1], lambda = inputMat[ , 2], meanOver)
  
  return(meanOverDOC)
  
} #  end meanOverDOCCreator function
