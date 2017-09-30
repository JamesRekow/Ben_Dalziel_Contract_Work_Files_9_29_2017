#  James Rekow

subgroupVecDifferences = function(subgroupVec1, subgroupVec2){
  
  #  ARGS: subgroupVec1 - a partition of a numeric vector of the form 1:n
  #        subgroupVec2 - another partition of the same numeric vector
  #
  #  RETURNS: numDiffs - number of elements in subgroupVec2 that are placed in a different subgroup
  #                      than in partition1
  
  #  identify the number of elements in the set being partitioned
  numElements = length(subgroupVec1)
  
  #  identify the set which the partitions divide
  baseSet = 1:numElements
  
  computeSubgroupIxVec = function(inputPartition){
    
    #  ARGS: inputPartition - partition
    #
    #  RETURNS: subgroupIxVec - numeric vector whose ith element is the index of the subgroup to which
    #                           the ith element of the base set belongs to in the input partition
    
    #  identify number of subgroups in input partition
    numSubgroups = length(inputPartition)
    
    # intializeVector
    subgroupIxVec = baseSet
    
    for(ii in 1:numSubgroups){
      
      #  identify elements of baseSet in subgroup ii
      subgroupElements = inputPartition[[ii]]
      
      #  
      subgroupIxVec[subgroupElements] = ii
      
    } #  end for
    
    return(subgroupIxVec)
    
  } #  end computesubgroupIxVec
  
  #  compute subgroup ix vector for each partition
  subgroupIxVec1 = computeSubgroupIxVec(partition1)
  subgroupIxVec2 = computeSubgroupIxVec(partition2)
  
  #  count the number of elements that are in different subgroups in the two partitions
  numDiffs = sum(subgroupIxVec1 != subgroupIxVec2)
  
  return(numDiffs)
  
} #  end subgroupVecDifferences function
