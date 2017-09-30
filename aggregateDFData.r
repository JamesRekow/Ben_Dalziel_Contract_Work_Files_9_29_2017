#  James Rekow

aggregateDFData  = function(outputDF, inputDF, outputRowIx, dataColIx, fun){
  
  #  ARGS: outputRowIx - indices of elements of outputDF rows to match to the rows of inputDF
  #        dataColIx - index of the outputDF on which to apply fun. e.g. meanPercentError
  #
  #  RETURNS:
  
  source("aggregateDFRowData.r")
  
  applyFun = function(inputRow) aggregateDFRowData(outputDF = outputDF, inputRow = inputRow,
                                                   outputRowIx = outputRowIx, dataColIx = dataColIx,
                                                   fun = fun)
  
  statisticVec = apply(inputDF, 1, applyFun)
  
  return(statisticVec)
  
} #  end  aggregateDFData  function

#  Example of use
# sdNumSubgroupsComputed = aggregateDFData(outputDF = piusBenchmark, inputDF = piusBenchmarkDF,
#                                          outputRowIx = 1:5, dataColIx = 8, fun = sd)
