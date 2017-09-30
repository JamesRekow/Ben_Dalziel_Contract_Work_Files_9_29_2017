#  James Rekow

piusDFTest = function(treatmentDF, iStrength = 1, sigmaMax = 0.1, thresholdMult = 10 ^ (-1),
                      maxSteps = 10 ^ 4, tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                      returnParams = FALSE, conGraph = NULL, rollRange = 0.025, numClust = NULL,
                      numReplicates = 3){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("piusTreatmentTest.r")
  
  #  inputs from treatmentDF: M, N, numSubgroups, lambda, univ
  
  ##  stretch data frame to include the correct number of replicates for each treatment
  
  treatmentMat = apply(treatmentDF, 1:2, as.numeric)

  stretchTreatmentMatRow = function(n){
    
    treatmentRow = treatmentMat[n, ]
    
    treatmentReplicateMatList = rep(list(treatmentRow), numReplicates)
    
    treatmentReplicateMat = Reduce(rbind, treatmentReplicateMatList, init = NULL)
    
    replicateID = 1:numReplicates
    treatmentReplicateMat = cbind(treatmentReplicateMat, replicateID)
    
    return(treatmentReplicateMat)
    
  } #  end stretchTreatmentMatRow
  
  testInputsMatList = lapply(1:nrow(treatmentMat), stretchTreatmentMatRow)
  testInputsMat = Reduce(rbind, testInputsMatList)

  performTreatmentTest = function(inputRow){
    
    #  extract parameter values from inputRow
    M = inputRow[1]
    N = inputRow[2]
    numSubgroups = inputRow[3]
    lambda = inputRow[4]
    univ = inputRow[5]
    
    testResultsVec = piusTreatmentTest(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                       univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                       maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                       interSmplMult = interSmplMult, lambda = lambda,
                                       returnParams = returnParams, conGraph = conGraph,
                                       rollRange = rollRange, numClust = numClust,
                                       returnNumSubgroupsComputed = TRUE)
    
    return(testResultsVec)
    
  } #  end performTreatmentTest
  
  testResultsMat = apply(testInputsMat, 1, performTreatmentTest)
  
  testResultsMat = t(testResultsMat)
  
  testResultsMat = cbind(testInputsMat, testResultsMat)

  testResultsDF = as.data.frame(testResultsMat)
  names(testResultsDF) = c("M", "N", "numSubgroups", "lambda", "univ", "replicateID", "percentError",
                           "numSubgroupsComputed")
  
  return(testResultsDF)
  
} #  end  piusDFTest  function
