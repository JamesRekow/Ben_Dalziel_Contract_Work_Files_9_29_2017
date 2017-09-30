#  James Rekow

countSubgroupVecDiff = function(subgroupVec1, subgroupVec2){
  
  #  ARGS: subgroupVec1 - numeric vector of subgroup indices corresponding to a base set A
  #        subgroupVec2 - numeric vector of subgroup indices corresponding to the same base set A
  #
  #  RETURNS: numDiff - minimum number of differences between the two subgroups for any permutation
  #                     of values of subgroupVec2
  
  source("subgroupVecMatcher.r")
  
  #  find the optimal permutation of the values of subgroupVec2
  subgroupVec2Ordered = subgroupVecMatcher(subgroupVec1 = subgroupVec1, subgroupVec2 = subgroupVec2)
  
  #  compute the minimum number of differences between the subgroup assignment in the two input vectors
  numDiff = sum(subgroupVec1 != subgroupVec2Ordered)
  
  return(numDiff)
  
} #  end countSubgroupVecDiff function
