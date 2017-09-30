#  James Rekow

identifyNumSubgroups2  = function(infoMat, centersVec){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("computeSSRatio.r")
  
  #  compute SS ratio of clusters for kmeans results using each number of centers in centersVec
  SSRatioVec = sapply(1:length(centersVec), function(n) computeSSRatio(inputMat = infoMat,
                                                                       centers = centersVec[n]))
  
  kmr = kmeans(x = SSRatioVec, centers = 2)
  
  numSubgroupsGuess = which.max(abs(diff(kmr$cluster)))
  
  return(numSubgroupsGuess)
  
} #  end  identifyNumSubgroups2  function
