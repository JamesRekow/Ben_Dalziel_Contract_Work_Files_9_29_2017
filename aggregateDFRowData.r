#  James Rekow

aggregateDFRowData  = function(outputDF, inputRow, outputRowIx, dataColIx, fun){
  
  #  ARGS:
  #
  #  RETURNS:
  
  checkRow = function(outputRow) all(outputRow[outputRowIx] == inputRow)
  
  keepRow = apply(outputDF, 1, checkRow)
  
  data = outputDF[ , dataColIx][keepRow]
  
  statistic = fun(data)
  
  return(statistic)
  
} #  end  aggregateDFRowData  function
