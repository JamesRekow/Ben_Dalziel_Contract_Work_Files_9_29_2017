#James Rekow

testComputeUnivWithinSSTEST  = function(M = 40, N = 20, univ = 1){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("abdListCreator.r")
  source("computeUnivWithinSSTEST.r")
  
  abdList = abdListCreator(M = M, N = N, univ = univ)
  
  testResult = computeUnivWithinSSTEST(abdList = abdList)
  
  return(testResult)
  
} #  end  testComputeUnivWithinSSTEST  function
