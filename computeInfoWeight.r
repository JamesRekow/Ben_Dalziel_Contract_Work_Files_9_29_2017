#  James Rekow

computeInfoWeight = function(docPoint, rollRange = 0.025){
  
  #  ARGS: docPoint - vector of the form c(over, diss)
  #
  #  RETURNS: localDissMid - midpoint of local dissimilarities (unweighted, not the same as the mean)
  
  #  extract overlap and dissimilarity of DOC point
  over = docPoint[1]
  diss = docPoint[2]
  
  #  return 0 if something goes wrong
  if(!is.numeric(over) || !is.numeric(diss)){
    return(0)
  } #  end if
  
  #  don't count DOC points that have low overlap but 0 dissimilarity, these have 0 diss because they
  #  only share a single species
  if(diss < 0.005){
    if(over < 0.7){
      return(0)
    } #  end if
  } #  end if
  
  #  identify data frame row indices of DOC points with similar overlap
  similarOver = abs(doc[[1]] - over) < rollRange
  
  #  create vector of dissimilarities of all DOC points with similar overlap to input docPoint
  localDiss = doc[[2]][similarOver]
  
  #  compute the midpoint of the local dissimilarities
  localDissMid = 0.5 * {max(localDiss) + min(localDiss)}
  
  #  compute weighted value whose sign corresponds to whether or not the samples corresponding to the DOC
  #  point are in the same or different subgroups (- same, + different) and whose absolute value
  #  corresponds to how much weight is given to the info from the given DOC point
  infoWeight = diss - localDissMid
  
  return(infoWeight)
  
} #  end computeInfoWeight function
