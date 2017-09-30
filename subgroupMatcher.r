#  James Rekow

subgroupMatcher = function(partition1, partition2){
  
  #  ARGS: partition1 - a partition of a numerical vector of indices. A list of vectors whose elements
  #                     are all distinct and whose union is a set A.
  #        partition2 - another partition of A.
  #
  #  RETURNS: partition2Ordered  - a partition list with the same elements (numerical vectors of indices)
  #                                as partition2, but in an order such that the ith vector in the list
  #                                corresponds to the ith vector in partition1. Corresponds in the sense that
  #                                the total number of indices placed in different subgroups than those
  #                                in partition1 is minimized
  
  #  TODO: add a method argument with two options, Jaccard index, and misallocated elements
  
  library(gtools)
  source("subgroupDifferences.r")
  
  #  compute number of subgroups in each partition
  numSubgroups1 = length(partition1)
  numSubgroups2 = length(partition2)
  
  #  create a matrix whose rows are the permutations of 1:numSubgroups 2
  partition2Permutations = permutations(n = numSubgroups2, r = numSubgroups2)
  
  computeSubgroupDiff = function(perm){
    
    #  ARGS: perm - numeric vector that is a permutation of 1:numSubgroups2
    #
    #  RETURNS: subgroupDiff - number of differences between partition1 and this permutation of partition2
    
    #  compute permutation of partition2
    partition2Perm = partition2[perm]
    
    #  compute differences between permutation of partition2 and partition1
    subgroupDiff = subgroupDifferences(partition1 = partition1, partition2 = partition2Perm)
    
    return(subgroupDiff)
    
  } #  end computeSubgroupDiff function
  
  #  compute the number of differences between partition1 and each permutation of partition2
  subgroupDiffs = apply(partition2Permutations, 1, computeSubgroupDiff)
  
  #  find the row number of the permutation that gives the minimum number of subgroup differences
  minDiffIx = which.min(subgroupDiffs)
  
  #  identify optimal permutation of partition2
  optimalPerm = partition2Permutations[minDiffIx, ]
  
  #  order subgroups in partition2 according to optimal permutation
  partition2Ordered = partition2[optimalPerm]
  
  return(partition2Ordered)
  
} #  end subgroupMatcher function
