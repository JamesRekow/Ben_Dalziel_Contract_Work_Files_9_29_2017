#  James Rekow

identifySubgroupsTest2 = function(M = 40, numSubgroups = 2, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                  thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                  intTime = 1, interSmplMult = 0.01, lambda = 0, returnParams = FALSE,
                                  conGraph = NULL, rollRange = 0.025){
  
  #  ARGS: numSubgroups - number of subgroups in the cohort
  #        rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS:
  
  source("subgroupsAbdListCreator.r")
  source("DOCProcedure.r")
  
  #  default indices of the 2 subgroups created from subgroupsAbdListCreator
  subgroup1Ix = 1:ceiling(M / 2)
  subgroup2Ix = {ceiling(M / 2)+ 1}:M
  
  #  create abdList with desired number of subgroups
  abdList = subgroupsAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                    univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                    maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                    interSmplMult = interSmplMult, lambda = lambda,
                                    returnParams = returnParams, conGraph = conGraph)
  
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
  
  #  compute info weight for each DOC point in data frame
  infoWeight = apply(doc, 1, computeInfoWeight)
  
  #  add infoWeight as a column to doc data frame
  doc$infoWeight = infoWeight
  
  #infoMat = matrix(rep(0, M * M), nrow = M, ncol = M)
  infoMat = matrix(rep(-1, M * M), nrow = M, ncol = M)
  infoMat[lower.tri(infoMat, diag = FALSE)] = infoWeight
  infoMat = t(infoMat)
  infoMat[lower.tri(infoMat, diag = FALSE)] = infoWeight
  
  kmr = kmeans(x = infoMat, centers = 2, iter.max = 1000, nstart = 100)$cluster
  
  # if(kmr[1] == 2){
  #   a = kmr == 2
  #   kmr[a] = 1
  #   kmr[!a] = 2
  # } #  end if
  
  subgroup1Guess = (1:M)[kmr == kmr[1]]
  
  return(subgroup1Guess)
  
} #  end identifySubgroupsTest2 function
