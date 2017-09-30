
createScaledResidualSSEDF  = function(){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("testComputeUnivWithinSSTEST.r")
  
  MVec = seq(from = 40, to = 200, by = 40)
  MVec = c(MVec, MVec)
  numM = length(MVec) / 2
  
  univVec = c(rep(1, numM), rep(0, numM))
  
  inputDF = data.frame(M = MVec, univ = univVec)
  
  testReplicate = function(inputRow){
    
    M = inputRow[1]
    univ = inputRow[2]
    
    testResult = testComputeUnivWithinSSTEST(M = M, univ = univ)
    
    return(testResult)
    
  } #  end testReplicate function
  
  testDFReplicate = function(n){
    
    replicateIDVec = rep(n, 2 * numM)
    
    testResults = apply(inputDF, 1, testReplicate)
    
    outputDF = cbind(inputDF, replicateIDVec, testResults)
    
    return(outputDF)
    
  } #  end testDFReplicate function
  
  testResultsDFList = lapply(as.list(1:numReplicates), testDFReplicate)
  testResultsDF = Reduce(rbind, testResultsDFList)
  
  return(testResultsDF)
  
} #  end  createScaledResidualSSEDF  function

#  code used to create plot using output df
#  dredd$univ = as.factor(dredd$univ)
# p = ggplot(dredd, aes(x = M, y = scaledResidualSSE, col = univ)) +
#       geom_point() +
#       geom_smooth(method = "loess") +
#       ggtitle("Identifying Universality Using Scaled Residual SSE",
#               subtitle = "20 Replicates at Each Treatment")
