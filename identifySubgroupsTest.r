#  James Rekow

identifySubgroupsTest = function(M = 40, numSubgroups = 2, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                 thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                 intTime = 1, interSmplMult = 0.01, lambda = 0, returnParams = FALSE,
                                 conGraph = NULL, rollRange = 0.025){
  
  #  ARGS: numSubgroups - number of subgroups in the cohort
  #        rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS:
  
  source("subgroupsAbdListCreator.r")
  source("DOCProcedure.r")
  
  #  default indices of the 2 subgroups created from subgroupsAbdListCreator
  subgroup1Ix = 1:20
  subgroup2Ix = 21:40
  
  #  create abdList with desired number of subgroups
  abdList = subgroupsAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                    univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                    maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                    interSmplMult = interSmplMult, lambda = lambda,
                                    returnParams = returnParams, conGraph = conGraph)
  
  #  compute DOC
  doc = DOCProcedure(abdList)
  
  #  make DOC a data frame (by default a doc is a named list)
  doc = data.frame(over = doc$x, diss = doc$y)
  
  cutoffSelector = function(x){
    
    #  ARGS: x - overlap input value
    #
    #  RETURNS: cutoffMult - multiplier to determine the cutoff dissimilarity value for DOC points with
    #                        overlap of x
    
    cutoffMult = 0.5

    return(cutoffMult)
    
  } #  end cutoffSelector function
  
  rollDissCutoff = function(docPoint){
    
    #  ARGS: docPoint - vector of the form c(over, diss)
    #
    #  RETURNS: localDissMid - midpoint of local dissimilarities (unweighted, not the same as the mean)
    
    #  extract overlap of DOC point
    over = docPoint[1]
    
    #  identify data frame row indices of DOC points with similar overlap
    similarOver = abs(doc[[1]] - over) < rollRange
    
    #  create vector of dissimilarities of all DOC points with similar overlap to input docPoint
    localDiss = doc[[2]][similarOver]
    
    #  compute the midpoint of the local dissimilarities
    localDissCutoff = cutoffSelector(over) * {max(localDiss) + min(localDiss)}
    
    return(localDissCutoff)
    
  } #  end rollDissCutoff function
  
  #  compute local dissimilarity midpoint for each DOC point in data frame
  dissRollCutoff = apply(doc, 1, rollDissCutoff)
  
  #  add dissRollMid as a column to doc data frame
  doc$rollDissCutoff = dissRollCutoff
  
  #  add a column identifying whether or not a DOC point has dissimilarity less than the local midpoint
  doc$lowDiss = doc$diss < doc$rollDissCutoff
  
  #  matrix of unique combinations of sample indices
  ixMat = combn(M, 2)
  
  #  add columns identifying sample indices to doc
  doc$ix1 = ixMat[1, ]
  doc$ix2 = ixMat[2, ]
  
  #  add column identifying DOC points with high overlap
  doc$highOver = doc$over > 0.5
  
  return(doc)
  
  ####
  
  # docHighOver = subset(doc, highOver == TRUE)
  # 
  # docHighOverLowDiss = subset(docHighOver, lowDiss == TRUE)
  # ixVec1 = c(docHighOverLowDiss$ix1, docHighOverLowDiss$ix2)
  # lowerInfo = sort(unique(ixVec1))
  # 
  # docHighOverHighDiss = subset(docHighOver, lowDiss == FALSE)
  # ixVec2 = c(docHighOverHighDiss$ix1, docHighOverHighDiss$ix2)
  # upperInfo = sort(unique(ixVec2))
  # # try lower half of low diss points for over > 0.5?
  
  ####
  
  # f = function(n){
  #   
  #   docSimDiss = subset(doc, simDiss == TRUE)
  #   
  #   keep = (docSimDiss$ix1 == n) | (docSimDiss$ix2 == n)
  #   
  #   docSimDissIxOnly = docSimDiss[keep, ]
  #   
  #   ixVec = unique(c(docSimDissIxOnly$ix1, docSimDissIxOnly$ix2))
  #   
  #   return(ixVec)
  #   
  # } #  end f function
  # 
  # f1 = f(1)
  # 
  # g = function(n){
  #   
  #   numSharedIx = sum(f(n) %in% f1)
  #   
  #   return(numSharedIx)
  #   
  # } #  end g function
  # 
  # closeVec = sapply(1:M, g)
  # 
  # firstTwo = sort(closeVec, index.return = TRUE)$ix[M - 1, M]
  # 
  # subgroup1Guess = c(1, firstTwo)
  # 
  # meanMatch = mean()
  # 
  # remainingIx = (1:M)[-subgroup1Guess]
  # 
  # h = function(ix1, ix2){
  #   
  #   numSharedIx = sum(f(ix1) %in% f(ix2))
  #   
  #   return(numSharedIx)
  #   
  # } #  end g function
  # 
  # addNewIx = function(x = NULL){
  #   
  #   ii = 1
  #   
  #   while(ii < length(remainingIx)){
  #     
  #     currentMean = 
  #     
  #   } #  end while
  #   
  # } #  end addNewIx function
  # 
  # 
  # sortedCloseVecList = sort(closeVec, index.return = TRUE)
  # 
  # cutoffIx = which(diff(sortedCloseVecList$x) == max(diff(sortedCloseVecList$x)))
  # 
  # subgroup1Guess = sort(sortedCloseVecList$ix[cutoffIx:length(sortedCloseVecList$ix)])
  # 
  # return(subgroup1Guess)
  
} #  end identifySubgroupsTest function
