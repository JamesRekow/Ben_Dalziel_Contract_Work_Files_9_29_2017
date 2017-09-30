#  James Rekow

partition = function(n, numPartitions){
  
  #  ARGS: n - number to be partitioned
  #        numPartitions - number of pieces into which n is to be partitioned
  #
  #  RETURNS: partitionSizeVec - numeric vector of the unique set of numPartitions numbers with the
  #                              property that the sum of the numbers is n and any two numbers in
  #                              partitionSizeVec differ by at most 1
  
  #  if the number of partitions is greater than n, set it to n
  numPartitions = min(numPartitions, n)
  
  #  compute the size of the partitions, ignoring the remainder
  basePartitionSize = floor(n / numPartitions)
  
  #  compute the number of partition elements that will be one larger than the base partition size
  #  in order to account for the remainder
  remainder = n %% numPartitions
  
  #  create base partition, ignoring remainder
  partitionSizeVec = rep(basePartitionSize, numPartitions)
  
  #  increase the size of some elements of the partition by 1 to account for the remainder
  if(remainder > 0){
    partitionSizeVec[1:remainder] = sapply(partitionSizeVec[1:remainder], function(z) z + 1)
  } #  end if
  
  return(partitionSizeVec)
  
} #  end partition function
