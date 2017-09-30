#  James Rekow

#  mdr = Mean Diss Ratio
#  mmd = mean mean dissimilarity (i.e. of subsets of a cohort with a given size)
#  ^^ didn't show any slope. No useful ratios apparent

#  SRSSE = scaled residual sse


computeMMD = function(abdList){
  
  source("DOCProcedure.r")
  source("computeUnivWithinSSTEST.r")
  
  subgroupSizeVec = floor(length(abdList) / {c(10, 8, 6, 4, 2)})
  
  computeSubgroupMMD = function(subgroupSize){
    
    computeSubgroupSampleMMD = function(zz = NULL){
      
      subgroupAbdList = sample(abdList, subgroupSize)
      subgroupSRSSE = computeUnivWithinSSTEST(abdList = subgroupAbdList)
      
      return(subgroupSRSSE)
      
    } #  end computeSubgroupSampleMMD
    
    sampleMMDVec = replicate(10, computeSubgroupSampleMMD())
    
    subgroupMMD = mean(sampleMMDVec)
    
    return(subgroupMMD)
    
  } #  end computeSubgroupMMD function
  
  mmd = sapply(subgroupSizeVec, computeSubgroupMMD)
  
  return(mmd)
  
} #  end computeMMD function
