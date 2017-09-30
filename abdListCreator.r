#  James Rekow

abdListCreator = function(M = 40, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                          thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                          intTime = 1, interSmplMult = 0.01, lambda = 0, returnParams = FALSE,
                          conGraph = NULL){
  
  #  ARGS: returnParams - if TRUE grList and imatList will also be returned
  #        conGraph - "connectivity graph". Vertices represent samples and edges connect
  #                   samples that can interact
  #        time - total time for intra cohort interactions to be modeled
  #        lambda - rate parameter for the exponentially distributed wait times between interactions
  #
  #  RETURNS: abdList - a list of length M containing the integrated and interacted (if pairProb > 0)
  #                     numerical abundance vectors of length N from a single cohort
  #
  #  NOTE: base non-zero lambda is 0.1 for the time being. Expected value of waiting time is 1 / rate
  
  library(igraph)
  source("cohortCreator.r")
  source("eulerIntegrate.r")
  source("intraCohortInteraction.r")
  
  #  give a warning if conGraph is supplied but there are no interactions
  if(lambda == 0 && (!is.null(conGraph))){
    warning("Changing the cohort connectivity (conGraph) will have no effect if 
            there are no interactions.")
  } #  end if
  
  #  if conGraph is supplied, check if it has any edges
  nonzeroEdges = FALSE
  if(!is.null(conGraph)){
    nonzeroEdges = ecount(conGraph) > 0
  } #  end if
  
  #  used to determine if the system is near equilibrium
  threshold = {thresholdMult * tStep} ^ 2
  
  #  define integrator function to apply eulerIntegrate with desired parameter vals
  integrator = function(smpl){
    return(eulerIntegrate(smpl, threshold = threshold, maxSteps = maxSteps, tStep = tStep))
  } #  end integrator function
  
  chrt = cohortCreator(M = M, N = N, iStrength = iStrength, univ = univ, sigmaMax = sigmaMax)
  abdList = lapply(chrt, integrator)
  
  #  simulate intra-cohort interaction if lambda > 0 and some of the samples can interact with each other
  #  (e.g. the number of edges in conGraph is greater than zero)
  if(lambda > 0 && {nonzeroEdges || is.null(conGraph)}){
    
    #  update the abundances of each sample in the cohort
    for(i in 1:M){
      chrt[[i]][[1]] = abdList[[i]]
    } #  end for
    
    #  simulate intra-cohort interactions
    abdList = intraCohortInteraction(chrt, M = M, N = N, lambda = lambda, threshold = threshold,
                                     maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                     interSmplMult= interSmplMult, conGraph = conGraph)
    
  } #  end if
  
  #  if returnParams is TRUE return grList and imatList along with abdList
  if(returnParams){
    
    grList = lapply(chrt, "[[", 2)
    imatList = lapply(chrt, "[[", 3)
    
    return(list(abdList, grList, imatList))
    
  } #  end if returnParams
  
  #  default is to only return abdList
  else{
    return(abdList)
  } #  end else
  
} #  end abdListCreator function
