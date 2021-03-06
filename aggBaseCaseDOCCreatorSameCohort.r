#  James Rekow

aggBaseCaseDOCCreatorSameCohort = function(M = 40, N = 20, iStrength = 1, univ = 1, intIx = NULL,
                                           sigmaMax = 0.1, thresholdMult = 10 ^ (-2), maxSteps = 10 ^ 4,
                                           tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                                           lambda = NULL, numDOC = 10){
  
  #  ARGS: numDOC - number of DOCs from which the aggregate points should be assembled
  #
  #  RETURNS: aggDOCDF - an aggregate DOC comprised of points from numDOC number of cohorts
  #                      with streamlined interaction steps (e.g. there is a completely connected
  #                      subset of interacting samples and a constellation of non-interacting samples
  #                      that don't interact at all). Each point also has a replicate ID associated with
  #                      it that tracks which replicate it was from, and the number of interacting
  #                      samples (0, 1, or 2) corresponding to it (e.g. if two interacting samples
  #                      are compared to create a DOC point this number would be 2)
  
  source("DOCProcedure.r")
  source("cohortCreator.r")
  source("eulerIntegrate.r")
  source("intraCohortInteraction_optimizedBaseCase.r")
  
  #  set default value of lambda
  if(is.null(lambda)){
    
    lambda = -0.5 * M * log(0.5)
    
  } #  end if
  
  #  let half of the samples be interacting samples
  numI = floor(M / 2)
  
  #  if intIx is not specified, randomly select half of the samples to interact
  if(is.null(intIx)){
    
    numI = floor(M / 2)
    intIx = sample(1:M, numI)
    
  } #  end if
  
  #  create a numerical vector with 1's in positions with the indices of interacting elements
  #  and 0's elsewhere
  intSmpls = rep(0, M)
  intSmpls[intIx] = 1
  
  #  create a matrix whose columns are each unique pair of sample indices
  pairMat = combn(M, 2)
  
  #  function to count the number of interacting samples in a vector of sample indices
  intCounter = function(ixVec) sum(intSmpls[ixVec])
  
  #  numerical vector of the number of samples in each sample pair that were interacting samples
  intCount = apply(pairMat, 2, intCounter)
  
  #  used to determine if the system is near equilibrium
  threshold = {thresholdMult * tStep} ^ 2
  
  #  define integrator function to apply eulerIntegrate with desired parameter vals
  integrator = function(smpl){
    return(eulerIntegrate(smpl, threshold = threshold, maxSteps = maxSteps, tStep = tStep))
  } #  end integrator function
  
  #  create cohort
  chrt = cohortCreator(M = M, N = N, iStrength = iStrength, univ = univ, sigmaMax = sigmaMax)
  
  #  integrate samples in the cohort and store the abundance vectors in a list
  abdList = lapply(chrt, integrator)
  
  produceDOCDF = function(n){
    
    #  simulate intra-cohort interaction if lambda > 0
    if(lambda > 0){
      
      #  update the abundances of each sample in the cohort
      for(i in 1:M){
        chrt[[i]][[1]] = abdList[[i]]
      } #  end for
      
      #  simulate intra-cohort interactions
      intAbdList = intraCohortInteraction_optimizedBaseCase(intIx = intIx, chrt = chrt, M = M, N = N,
                                                            lambda = lambda, threshold = threshold,
                                                            maxSteps = maxSteps, tStep = tStep,
                                                            intTime = intTime,
                                                            interSmplMult = interSmplMult)
      
    } #  end if
    
    #  create DOC
    doc = DOCProcedure(intAbdList)
    
    #  store data in a data frame
    DOCDF = data.frame(replicateID = n, intCount = intCount, over = doc$x, diss = doc$y)
    
    return(DOCDF)
    
  } #  end produceDOC function
  
  #  create a list of replicate data frames
  DOCDFList = lapply(as.list(1:numDOC), produceDOCDF)
  
  #  aggregate data from the replicates
  aggDOCDF = Reduce(rbind, DOCDFList)
  
  #  make the intCount variable a factor (mostly for plotting purposes)
  aggDOCDF$intCount = as.factor(aggDOCDF$intCount)
  
  return(aggDOCDF)
  
} #  end aggBaseCaseDOCCreatorSameCohort function
