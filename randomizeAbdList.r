#  James Rekow

#  this doesn't seem to work

randomizeAbdList  = function(abdList){
  
  #  ARGS:
  #
  #  RETURNS: randomizedAbdList - abundance list randomized according to the procedure used to create Null
  #           Model 1 in Section 1.3 of Bashan et al.
  
  #  extract number of samples in cohort and number of species in each sample 
  M = length(abdList)
  N = length(abdList[[1]])
  
  abdMat = Reduce(rbind, abdList, init = NULL)
  
  randomizeSpeciesAbd = function(inputCol){
    
    nonzeroIx = inputCol > 0
    nonzeroAbdVals = inputCol[nonzeroIx]
    
    newNonzeroAbdVals = sample(nonzeroAbdVals, replace = TRUE)
    
    randomizedCol = inputCol
    randomizedCol[nonzeroIx] = newNonzeroAbdVals
    
    return(randomizedCol)
    
  } #  end randomizeSpeciesAbd function
  
  randomizedAbdMat = apply(abdMat, 2, randomizeSpeciesAbd)
  
  randomizedAbdMat = randomizedAbdMat / max(randomizedAbdMat)
  
  randomizedAbdList = lapply(as.list(1:M), function(n) randomizedAbdMat[n, ])
  
  # 
  # 
  # randomizedList = lapply(as.list(1:M), function(n) sample(abdMat[n, ], replace))
  # 
  # nonzeroIx = abdMat > 0
  # 
  # selectRandomizedSample = function(n){
  #   
  #   
  #   
  # } #  end selectRandomizedSample function
  
  return(randomizedAbdList)
  
} #  end  randomizedAbdList  function
