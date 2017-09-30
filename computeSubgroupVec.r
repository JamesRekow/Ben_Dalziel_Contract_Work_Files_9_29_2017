#  James Rekow

computeSubgroupVec = function(M , numSubgroups){
  
  #  ARGS: M - number of samples in cohort
  #        numSubgroups - number of subgroups in cohort
  #
  #  RETURNS: subgroupVec - numeric vector of length M whose ith value is the index of the default subgroup
  #                         to which the ith sample in the cohort will belong, assuming the cohort was
  #                         created using subgroupsAbdListCreator
  
  source("partition.r")
  
  #  compute the number of samples in each subgroup
  subgroupSizes = partition(n = M, numPartitions = numSubgroups)
  
  #  create subgroupList
  subgroupList = lapply(as.list(1:numSubgroups), function(ii) rep(ii, subgroupSizes[ii]))
  
  #  unlist into a vector
  subgroupVec = unlist(subgroupList)
  
  return(subgroupVec)
  
} #  end computeSubgroupVec function
