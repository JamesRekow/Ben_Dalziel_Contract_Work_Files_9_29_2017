#  James Rekow

identifySubgroupsVer2 = function(abdList, rollRange = 0.025){
  
  #  ARGS: rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS: subgroupsGuess - vector of indices grouping input samples by subgroup. e.g. subgroupIxVec = 
  #                            c(1, 1, 1, 2, 2) would indicate that samples 1:3 are in a subgroup together,
  #                            and samples 4 and 5 are in a separate subgroup together
  
  library(NbClust)
  source("computeInfoWeightMat.r")

  #  compute matrix of info weights
  infoMat = computeInfoWeightMat(abdList = abdList, rollRange = rollRange)
  
  nbcr = NbClust(data = infoMat + 10, method = "kmeans")
  
  subgroupsGuess = nbcr$Best.partition
  
  return(subgroupsGuess)
  
} #  end identifySubgroupsVer2 function
