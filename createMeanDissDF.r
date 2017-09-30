#  James Rekow

createMeanDissDF  = function(MVec = NULL, N = 20, numReplicates = 2){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("abdListCreator.r")
  source("DOCProcedure.r")
  
  if(is.null(MVec)){
    MVec = seq(from = 40, to = 200, by = 40)
  } #  end if
  
  MVec = c(MVec, MVec)
  numM = length(MVec) / 2
  
  univVec = c(rep(1, numM), rep(0, numM))
  
  inputDF = data.frame(M = MVec, univ = univVec)
  
  testReplicate = function(inputRow){
    
    M = inputRow[1]
    univ = inputRow[2]
    
    abdList = abdListCreator(M = M, N = N, univ = univ)
    
    ##  testing
    
    createSampledMeanDiss = function(zz = NULL){
      
      keepIx = sample(N, 20)
      
      speciesSelector = function(abdVec) abdVec[keepIx]
      
      speciesSubsetAbdList = lapply(abdList, speciesSelector)
      
      speciesSubsetDoc = DOCProcedure(abdList = speciesSubsetAbdList)
      
      speciesSubsetMeanDiss = mean(speciesSubsetDoc$y)
      
      return(speciesSubsetMeanDiss)
      
    } #  end createSampledMeanDiss function
    
    sampledMeanDissVec = sapply(1:10, createSampledMeanDiss)
    sampledMeanDiss = mean(sampledMeanDissVec)
    
    return(sampledMeanDiss)
    
    ##
    
    doc = DOCProcedure(abdList = abdList)
    meanDiss = mean(doc$y)
    
    return(meanDiss)
    
  } #  end testReplicate function
  
  testDFReplicate = function(n){
    
    replicateIDVec = rep(n, 2 * numM)
    
    meanDiss = apply(inputDF, 1, testReplicate)
    
    outputDF = cbind(inputDF, replicateIDVec, meanDiss)
    
    return(outputDF)
    
  } #  end testDFReplicate function
  
  testResultsDFList = lapply(as.list(1:numReplicates), testDFReplicate)
  # testResultsDF = Reduce(rbind, testResultsDFList)
  testResultsDF = do.call(rbind, testResultsDFList)
  testResultsDF$univ = as.factor(testResultsDF$univ)
  
  return(testResultsDF)
  
} #  end  createMeanDissDF  function
