#  James Rekow

computeUnivWithinSSTEST  = function(abdList, rollRange = 0.025){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("computeInfoWeightMat.r")
  
  #  compute matrix of info weights
  infoMat = computeInfoWeightMat(abdList = abdList, rollRange = rollRange)

  meanInfoVec = apply(infoMat, 2, mean)
  
  squaredDistanceFromMean = function(infoVec) sum((infoVec - meanInfoVec) ^ 2)
  
  seVec = apply(infoMat, 2, squaredDistanceFromMean)
  sse = sum(seVec)
  
  # normalizedSeVec = seVec / max(seVec)
  # 
  # sse = mean(normalizedSeVec)
  
  # sse = - mean(infoMat[infoMat < 0]) / mean(infoMat[infoMat > 0])
  
  # M = nrow(infoMat)
  # signMat = matrix(sample(c(-1, 1), M * M, replace = TRUE), nrow = M)
  # newInfoMat = infoMat * signMat
  # 
  # newMeanInfoVec = apply(newInfoMat, 2, mean)
  # 
  # newSquaredDistanceFromMean = function(infoVec) sum((infoVec - newMeanInfoVec) ^ 2)
  # 
  # newSeVec = apply(newInfoMat, 2, newSquaredDistanceFromMean)
  # newSse = sum(newSeVec)
  # 
  # sse = sse / newSse
  
  # sse = sum(infoMat < 0) / sum(infoMat > 0)
  
  # sse = sse / sum(infoMat[infoMat > 0])
  
  #  divide by M ^ 1.5 to try and "normalize" out the effect M has on sse, only capturing effect of univ.
  #  If we consider the rows of infoMat as M dimensional vectors that are random perturbations of their
  #  mean vector, then the expected sse should be roughly proportional to c * M ^ 1.5, for some constant
  #  c. This is because each vector v has M dimensions, and if we suppose the difference between each
  #  component of v and the corresponding component of the mean vector is roughly equal to a constant,
  #  call it e, then the squared error of ## should be eM, which ends with M ^ 2, not sure how to explain
  #  this after all ## v is sqrt(M * e ^ 2) = e * sqrt(M). There are M such vectors,
  #  so summing these squared errors will give a dependence of M * sqrt(M) = M ^ 1.5. Squared error is
  #  sum((v - mv) ^ 2), where v is a row vector of infoMat and mv is the vector mean of all such row
  #  vectors.
  M = length(abdList)
  sse = sse / {M ^ 1.5}
  
  #  crit value appears to be ~0.185
  
  return(sse)
  
} #  end  computeUnivWithinSSTEST function
