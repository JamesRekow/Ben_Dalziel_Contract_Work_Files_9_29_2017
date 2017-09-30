#  James Rekow

computeUnivWithinSS  = function(abdList, rollRange = 0.025){
  
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
  
  #  deprecated, below computes the sse, but in a ridiculous way
  # kmr = kmeans(x = infoMat, centers = 1, iter.max = 1000, nstart = 100)
  # 
  # withinSS = kmr$withinss
  
  return(sse)
  
} #  end  computeUnivWithinSS function
