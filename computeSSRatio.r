#  James Rekow

computeSSRatio = function(inputMat, centers){
  
  #  ARGS: inputMat - matrix or data frame of vectors to apply kmeans to
  #        centers - number of centers to use in kmeans algorithm
  #
  #  RETURNS: SSRatio - ratio of total between clusters sum of squares to total within clusters sum of
  #                     squares
  
  #  perform kmeans on the input data matrix using the given number of centers
  kmr = kmeans(inputMat, centers = centers, iter.max = 1000, nstart = 100)
  
  #  compute the ratio of the total between clusters sum of squares to the total within clusters sum of
  #  squares
  SSRatio = kmr$betweenss / kmr$tot.withinss
  
  return(SSRatio)
  
} #  end computeSSRatio function
