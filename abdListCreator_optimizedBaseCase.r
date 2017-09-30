#  James Rekow

abdListCreator_optimizedBaseCase = function(intIx = NULL, M = 40, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                            thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                            intTime = 1, interSmplMult = 0.01, lambda = 0){
  
  #  ARGS: intTime - total time for intra cohort interactions to be modeled
  #        lambda - rate parameter for the exponentially distributed wait times between interactions
  #        intIx - indices of interacting samples
  #
  #  RETURNS: abdList - a list of length M containing the integrated and interacted (if pairProb > 0)
  #                     numerical abundance vectors of length N from a single cohort
  #
  #  NOTE: base non-zero lambda is 0.1 for the time being. Expected value of waiting time is 1 / rate
  
  source("cohortCreator.r")
  source("eulerIntegrate.r")
  source("intraCohortInteraction_optimizedBaseCase.r")
  
  #  if intIx is not specified, randomly select half of the samples to interact
  if(is.null(intIx)){
    
    numI = floor(M / 2)
    intIx = sample(1:M, numI)
    
  } #  end if
  
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
  
  #  simulate intra-cohort interaction if lambda > 0
  if(lambda > 0){
    
    #  update the abundances of each sample in the cohort
    for(i in 1:M){
      chrt[[i]][[1]] = abdList[[i]]
    } #  end for
    
    #  simulate intra-cohort interactions
    abdList = intraCohortInteraction_optimizedBaseCase(intIx = intIx, chrt = chrt, M = M, N = N,
                                                       lambda = lambda, threshold = threshold,
                                                       maxSteps = maxSteps, tStep = tStep,
                                                       intTime = intTime, interSmplMult= interSmplMult)
    
  } #  end if
  
  return(abdList)
  
  } #  end abdListCreator_optimizedBaseCase function
