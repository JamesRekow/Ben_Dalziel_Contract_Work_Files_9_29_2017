# James Rekow

#  sdDiff is almost never significant (to the level of having a difference in means greater than or
#  equal to one standard deviation), so don't bother with that anymore

#  change sd to a formulation using se, see if that helps at all

se = function(x){
  
  #  ARGS: x - a numeric vector
  #
  #  RETURNS: stdError - the standard error of x
  
  stdError = sd(x) / sqrt(length(x))
  
  return(stdError)
  
} #  end se function

correctMeans = function(DFListList){
  DFListList = unlist(DFListList, recursive = FALSE)
  
  correctListMeans = function(DFList) mean(DFList[[1]]$x) < mean(DFList[[2]]$x)
  correctMeanVec = unlist(lapply(DFListList, correctListMeans))
  correctMeanVec = list(correctMeanVec)
  
  return(correctMeanVec)
  
} #  end correctMeans function

seDiff = function(DFListList){
  DFListList = unlist(DFListList, recursive = FALSE)
  
  significantSEDiff = function(DFList){
    over1 = DFList[[1]]$x
    over2 = DFList[[2]]$x
    sigDiff = abs(mean(over1) - mean(over2)) > max(se(over1), se(over2))
    return(sigDiff)
  } #  end significantSDDiff function
  
  seDiffVec = unlist(lapply(DFListList, significantSEDiff))
  seDiffVec = list(seDiffVec)
  
  return(seDiffVec)
  
} #  end seDiff function

tTestDiff = function(DFListList){
  DFListList = unlist(DFListList, recursive = FALSE)
  
  significanttTestDiff = function(DFList){
    over1 = DFList[[1]]$x
    over2 = DFList[[2]]$x
    sigDiff = t.test(over1, over2)$p.value < 0.05
    return(sigDiff)
  } #  end significantSDDiff function
  
  tTestDiffVec = unlist(lapply(DFListList, significanttTestDiff))
  tTestDiffVec = list(tTestDiffVec)
  
  return(tTestDiffVec)
  
} #  end tTestDiff function

ksTestDiff = function(DFListList){
  DFListList = unlist(DFListList, recursive = FALSE)
  
  significantksTestDiff = function(DFList){
    over1 = DFList[[1]]$x
    over2 = DFList[[2]]$x
    sigDiff = ks.test(over1, over2)$p.value < 0.05
    return(sigDiff)
  } #  end significantSDDiff function
  
  ksTestDiffVec = unlist(lapply(DFListList, significantksTestDiff))
  ksTestDiffVec = list(ksTestDiffVec)
  
  return(ksTestDiffVec)
  
} #  end DFListList function

intersectArrays = function(arr1, arr2, coordArray){
  
  intersectArrayVecs = function(coords){
    
    coords = unlist(coords)
    
    vec1 = unlist(arr1[coords[1], coords[2], coords[3]])
    vec2 = unlist(arr2[coords[1], coords[2], coords[3]])
    
    matchVec = vec1 & vec2
    matches = sum(matchVec)
    
    #  try returning this after initial checks
    #  significant = matches > 9
    
    return(matches)
    
  } #  end intersectArrayVecs
  
  intersectionArray = apply(coordArray, 1:3, intersectArrayVecs)
  
  return(intersectionArray)
  
} #  end intersectArrays function

#  Deprecated, but kept to highlight philosophy
# successfulTest = function(DFList){
#   DFList = unlist(DFList, recursive = FALSE)
#   over1 = DFList[[1]]$x
#   over2 = DFList[[2]]$x
#   correct = mean(over2) > mean(over1)
#   significant = t.test(over1, over2)$p.value < 0.05
#   return(correct && significant)
# }
