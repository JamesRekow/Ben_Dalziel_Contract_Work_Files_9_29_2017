#  James Rekow

identifyNumSubgroupsTest = function(abdList, infoMat){
  
  #  ARGS: infoMat - matrix of info weights corresponding to pairs of samples from an abundance list
  #
  #  RETURNS: numClust - predicted number of clusters in infoMat
  
  normalizeVec = function(x) x / sqrt(sum(x ^ 2))
  
  abdMat = Reduce(rbind, abdList)
  
  abdMat = t(apply(abdMat, 1, normalizeVec))
  infoMat = t(apply(infoMat, 1, normalizeVec))
  
  mixMat = cbind(abdMat, infoMat)
  
  mixMat = t(apply(mixMat, 1, normalizeVec))
  
  cmr = clusGap(x = mixMat, FUNcluster = kmeans, K.max = 9, d.power = 2, B = 100)
  
  numClust = maxSE(f = cmr$Tab[ , "gap"], SE.f = cmr$Tab[ , "SE.sim"], SE.factor = 1,
                   method = "Tibs2001SEmax")
  
  return(numClust)
  
} #  end identifyNumSubgroupsTest function
