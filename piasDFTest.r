#  James Rekow

piasDFTest  = function(treatmentDF, iStrength = 1, sigmaMax = 0.1, thresholdMult = 10 ^ (-1),
                       maxSteps = 10 ^ 4, tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                       returnParams = FALSE, conGraph = NULL, rollRange = 0.025, numClust = NULL,
                       numReplicates = 3){
  
  #  ARGS:
  #
  #  RETURNS:
  
  #  inputs from treatmentDF: M, N, univ, numSubgroups, lambda
  
  ##  stretch data frame to include the correct number of replicates for each treatment
  
  stretchTreatmentDFRow = function(n){
    
    treatmentRow = treatmentDF[n, ]
    
    treatmentReplicateMat = matrix(treatmentRow, nrow = numReplicates, ncol = length(treatmentRow),
                                   byrow = TRUE)
    
    replicateID = 1:numReplicates
    
    treatmentReplicateMat = cbind(treatmentReplicateMat, replicateID)
    
    return(treatmentReplicateMat)
    
  } #  end stretchTreatmentDFRow
  
  testInputsMatList = lapply(1:nrow(treatmentDF), stretchTreatmentDFRow)
  testInputsMat = Reduce(rbind, testResultsDFList)
  testInputsDF = data.frame(testInputsMat)
  
  colnames(testInputsDF) = c("M", "N", "numSubgroups", "lambda", "univ", "replicateID")
  
  return(testInputsDF)
  
  
  
  performTreatmentTest = function(inputRow){
    
    #  extract parameter values from inputRow
    M = inputRow[1]
    N = inputRow[2]
    numSubgroups = inputRow[3]
    lambda = inputRow[4]
    univ = inputRow[5]
    
    testResultsVec = piasTreatmentTest(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                       univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                       maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                       interSmplMult = interSmplMult, lambda = lambda,
                                       returnParams = returnParams, conGraph = conGraph,
                                       rollRange = rollRange, numClust = numClust,
                                       returnNumSubgroupsComputed = TRUE)
    
    
    
    return(testResultsVec)
    
  } #  end performTreatmentTest
  
  testResultsMat = 
  
  
  performTreatmentTestReplicates = function(inputRow){
    
    testResultsMat = replicate(numReplicates, performTreatmentTest(inputRow))
    
    errorVec = testResultsMat[1, ]
    numSubgroupsComputed = testResultsMat[2, ]
    
    return(meanPercentErrorVec)
    
  } #  end performTreatmentTestReplicates function
  
  
  meanPercentErrorVec = apply(inputMat, 1, performAlgorithmTest)
  
  meanPercentErrorDF = data.frame(M = inputMat[ , 1], N = inputMat[ , 2], numSubgroups = inputMat[ , 3],
                                  lambda = inputMat[ , 4], univVec = inputMat[ , 5],
                                  meanPercentError = meanPercentErrorVec)
  
} #  end  piasDFTest  function
