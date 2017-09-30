
piusBenchmark  = function(MVec = NULL, NVec = NULL, numSubgroupsVec = NULL, lambdaVec = NULL,
                          univVec = NULL, iStrength = 1, sigmaMax = 0.1, thresholdMult = 10 ^ (-1),
                          maxSteps = 10 ^ 4, tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                          returnParams = FALSE, conGraph = NULL, rollRange = 0.025, numClust = NULL,
                          numReplicates = 2){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("piusBenchmarkDFCreator.r")
  source("piusDFTest.r")
  
  treatmentDF = piusBenchmarkDFCreator(MVec = MVec, NVec = NVec, numSubgroupsVec = numSubgroupsVec,
                                       lambdaVec = lambdaVec, univVec = univVec)
  
  testResultsDF = piusDFTest(treatmentDF = treatmentDF, iStrength = iStrength, sigmaMax = sigmaMax,
                             thresholdMult = thresholdMult, maxSteps = maxSteps, tStep = tStep,
                             intTime = intTime, interSmplMult = interSmplMult, returnParams = returnParams,
                             conGraph = conGraph, rollRange = rollRange, numClust = numClust,
                             numReplicates = numReplicates)
  
  return(testResultsDF)
  
} #  end  piusBenchmark  function
