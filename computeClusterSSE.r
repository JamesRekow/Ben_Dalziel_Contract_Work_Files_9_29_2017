#  James Rekow

computeClusterSSE = function(inputMat, centers){
  
  #  ARGS:
  #
  #  RETURNS:
  
  #  perform kmeans on the input data matrix using the given number of centers
  kmr = kmeans(inputMat, centers = centers, iter.max = 1000, nstart = 100)
  
  SSE = sum(kmr$tot.withinss)
  
  return(SSE)
  
} #  end computeClusterSSE function
