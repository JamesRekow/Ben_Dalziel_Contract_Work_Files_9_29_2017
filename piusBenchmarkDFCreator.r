
piusBenchmarkDFCreator  = function(MVec = NULL, NVec = NULL, numSubgroupsVec = NULL, lambdaVec = NULL,
                                   univVec = NULL){
  
  #  ARGS:
  #
  #  RETURNS:
  
  if(is.null(MVec)){
    MVec = c(40, 100)
  } #  end if
  
  if(is.null(NVec)){
    NVec = c(20, 40)
  } #  end if
  
  if(is.null(numSubgroupsVec)){
    numSubgroupsVec = c(1, 2, 3, 4, 5, 6)
  } #  end if
  
  if(is.null(lambdaVec)){
    lambdaVec = c(0, 100)
  } #  end if
  
  if(is.null(univVec)){
    univVec = c(0, 1)
  } #  end if
  
  treatmentDF = expand.grid(MVec, NVec, numSubgroupsVec, lambdaVec, univVec)
  names(treatmentDF) = c("M", "N", "numSubgroups", "lambda", "univ")
  
  return(treatmentDF)
  
} #  end  piusBenchmarkDFCreator  function
