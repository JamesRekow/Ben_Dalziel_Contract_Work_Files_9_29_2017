#  James Rekow

partitionVec = function(v, numPartitions){
  
  #  ARGS: v - numerical vector
  #        numPartitions - number of vectors into which v will be partitioned
  #
  #  RETURNS: partitionList - a list of numPartition numerical vectors that form a partition of v.
  #                           The length of each vector is length(v) / numPartitions or
  #                           1 + length(v) / numPartitions (depending on whether or not length(v) is
  #                           divisible by numPartitions) and the elements in each vector are taken
  #                           randomly from v (in such a way that no two vectors share any elements).
  
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  
  n = length(v)
  numPartitions = min(numPartitions, n)
  remainder = n %% numPartitions
  basePartitionSize = floor(n / numPartitions)
  partitionSizeVec = as.list(rep(basePartitionSize, numPartitions))
  
  if(remainder > 0){
    partitionSizeVec[1:remainder] = lapply(partitionSizeVec[1:remainder], function(z) z + 1)
  } #  end if
  
  splitIx = cumsum(partitionSizeVec)
  splitIx = splitIx[-length(splitIx)] + 1
  
  ranV = sample(v)
  
  partitionList = splitAt(x = ranV, pos = splitIx)
  
  return(partitionList)
  
} #  end partitionVec function
