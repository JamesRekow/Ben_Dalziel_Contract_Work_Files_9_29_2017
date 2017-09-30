#  James Rekow

computeInfoWeightMat = function(abdList, rollRange = 0.025){
  
  #  ARGS: rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS: infoMat - M x M numerical matrix, where M is the number of samples in abdList. The diagonal
  #                     elements are -1. The ijth element is equal to the weighted value assigned to the
  #                     DOC point corresponding to samples i and j. Negative weights indicate that the
  #                     samples are in the same subgroup, positive weights indicate that the samples
  #                     are in different subgroups
  
  source("DOCProcedure.r")
  
  #  identify number of samples
  M = length(abdList)
  
  #  compute DOC
  doc = DOCProcedure(abdList)
  
  #  make DOC a data frame (by default a doc is a named list)
  doc = data.frame(over = doc$x, diss = doc$y)
  
  computeInfoWeight = function(docPoint){
    
    #  ARGS: docPoint - vector of the form c(over, diss)
    #
    #  RETURNS: localDissMid - midpoint of local dissimilarities (unweighted, not the same as the mean)
    
    #  extract overlap and dissimilarity of DOC point
    over = docPoint[1]
    diss = docPoint[2]
    
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
  
  #  compute info weight for each DOC point in data frame
  infoWeight = apply(doc, 1, computeInfoWeight)
  
  #  compute maximum absolute value of weights
  maxAbsWeight = max(abs(infoWeight))
  
  #  normalize info weights so that the greatest absolute value of weights is equal to 1
  #infoWeight = infoWeight / maxAbsWeight
  
  #  create matrix of info weights
  infoMat = matrix(rep(-1, M * M), nrow = M, ncol = M)
  infoMat[lower.tri(infoMat, diag = FALSE)] = infoWeight
  infoMat = t(infoMat)
  infoMat[lower.tri(infoMat, diag = FALSE)] = infoWeight
  
  return(infoMat)
  
} #  end computeInfoWeightMat function
