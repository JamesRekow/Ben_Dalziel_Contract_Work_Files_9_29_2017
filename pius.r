#  James Rekow

#  currently equivalent to identifySubgroupsVer1 (using gap statistic of Tibshirani et al. to determine
#  number of clusters, and kmeans to partition into clusters. Other methods didn't improve performance)
#  Partition Into Universal Subgroups (PIUS algorithm)

pius = function(abdList, rollRange = 0.025, numClust = NULL){
  
  #  ARGS: rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS: subgroupsGuess - vector of indices grouping input samples by subgroup. e.g. subgroupIxVec = 
  #                            c(1, 1, 1, 2, 2) would indicate that samples 1:3 are in a subgroup together,
  #                            and samples 4 and 5 are in a separate subgroup together
  
  source("computeInfoWeightMat.r")
  source("identifyNumSubgroups.r")
  
  ##  STEP 1: matrix of info weights
  ##  METHOD: linear residual based on rolling midpoint (mean of min, max) of dissimilarity values,
  ##          not normalized
  
  #  compute matrix of info weights
  infoMat = computeInfoWeightMat(abdList = abdList, rollRange = rollRange)
  
  ##  STEP 2: compute the number of universal subgroups present in cohort
  ##  METHOD: gap statistic of Tibshirani et al.
  
  #  use identify numsubgroups to identify correct number of subgroups, if an expected number of clusters
  #  isn't provided
  if(is.null(numClust)){
    numClust = identifyNumSubgroups(infoMat = infoMat)
  } #  end if
  
  ##  STEP 3:  partition samples into subgroups with universal dynamics
  ##  METHOD:  use the number of clusters computed in Step 2 an the matrix of info weights computed in
  ##           Step 1 as the data
  
  #  use kmeans to identify subgroups of samples, putting them into numClust clusters
  subgroupsGuess = kmeans(x = infoMat, centers = numClust, iter.max = 1000, nstart = 100)$cluster
  
  return(subgroupsGuess)
  
} #  end pius function
