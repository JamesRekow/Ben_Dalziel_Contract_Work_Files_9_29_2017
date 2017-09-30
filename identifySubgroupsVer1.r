#  James Rekow

identifySubgroupsVer1 = function(abdList, rollRange = 0.025, numClust = NULL){
  
  #  ARGS: rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS: subgroupsGuess - vector of indices grouping input samples by subgroup. e.g. subgroupIxVec = 
  #                            c(1, 1, 1, 2, 2) would indicate that samples 1:3 are in a subgroup together,
  #                            and samples 4 and 5 are in a separate subgroup together
  
  source("computeInfoWeightMat.r")
  source("identifyNumSubgroups.r")
  
  #  compute matrix of info weights
  infoMat = computeInfoWeightMat(abdList = abdList, rollRange = rollRange)
  
  #  use identify numsubgroups to identify correct number of subgroups, if an expected number of clusters
  #  isn't provided
  if(is.null(numClust)){
    numClust = identifyNumSubgroups(infoMat = infoMat)
  } #  end if
  
  #  use kmeans to identify subgroups of samples, putting them into numClust clusters
  subgroupsGuess = kmeans(x = infoMat, centers = numClust, iter.max = 1000, nstart = 100)$cluster
  
  return(subgroupsGuess)
  
} #  end identifySubgroupsVer1 function
