#  James Rekow

subgroupVecMatcher = function(subgroupVec1, subgroupVec2){
  
  #  ARGS: subgroupVec1 - a numeric vector whose ith element is the subgroup to which sample i belongs,
  #                       where sample i is an element of a set A
  #        subgroupVec2 - another vector of the same form as subgroupVec1, corresponding to the same set A
  #
  #  RETURNS: subgroupVec2Ordered  - subgroupVec2 with the values permuted so as to minimize the number of
  #                                  differences between subgroupVec1 and subgroupVec2
  
  #  TODO: add a method argument with two options, Jaccard index, and misallocated elements
  
  library(gtools)

  #  number of subgroups in subgroupVec2
  numSubgroups2 = max(subgroupVec2)
  
  #  list which indices correspond to each subgroup
  subgroupIxList = lapply(as.list(1:numSubgroups2), function(ii) as.numeric(subgroupVec2 == ii))
  
  #  create a matrix whose rows are the permutations of 1:numSubgroups 2
  permutationMat = permutations(n = numSubgroups2, r = numSubgroups2)
  
  createSubgroupVec = function(perm){
    
    #  ARGS: perm - numeric vector that is a permutation of 1:numSubgroups2
    #
    #  RETURNS: permSubgroupVec2 - subgroup created by permuting the values of subgroupVec2 according to
    #                              perm
    
    #  compute the elements of the permutation in each subgroup
    permSubgroupVec2List = lapply(as.list(1:numSubgroups2), function(ii) perm[ii] * subgroupIxList[[ii]])
    
    #  combine values to create the permuted subgroup vector
    permSubgroupVec2 = Reduce("+", permSubgroupVec2List)
    
    return(permSubgroupVec2)
    
  } #  end createSubgroupVec function
  
  computeSubgroupDiff = function(perm){
    
    #  ARGS: perm - numeric vector that is a permutation of 1:numSubgroups2
    #
    #  RETURNS: subgroupDiff - number of differences between subgroupVec1 and the given permutation
    #                          of the values of subgroupVec2. All of the values of i in subgroupVec2
    #                          are replaced by perm[i]
    
    #  create subgroup corresponding to input permutation of values of subgroupVec2
    permSubgroupVec2 = createSubgroupVec(perm)
    
    #  compute number of differences between the given permutation of values of subgroupVec2 and
    #  subgroupVec1
    subgroupDiff = sum(subgroupVec1 != permSubgroupVec2)
    
    return(subgroupDiff)
    
  } #  end computeSubgroupDiff function
  
  #  compute the number of differences between subgroupVec1 and each permutation of values of subgroupVec2
  subgroupDiffs = apply(permutationMat, 1, computeSubgroupDiff)
  
  #  find the row number of the permutation that gives the minimum number of subgroup differences
  minDiffIx = which.min(subgroupDiffs)
  
  #  identify optimal permutation of values of subgroupVec2
  optimalPerm = permutationMat[minDiffIx, ]
  
  #  create optimal permutation of values of subgroupVec2
  subgroupVec2Ordered = createSubgroupVec(optimalPerm)
  
  return(subgroupVec2Ordered)
  
} #  end subgroupVecMatcher function
