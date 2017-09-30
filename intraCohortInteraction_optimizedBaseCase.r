#  James Rekow

intraCohortInteraction_optimizedBaseCase = function(chrt, M, N, intIx, lambda = 0, threshold = 10 ^ (-6),
                                                    maxSteps = 10 ^ 4, tStep = 10 ^ (-2), intTime = 1,
                                                    interSmplMult= 0.01){
  
  #  ARGS:  chrt - input cohort in the form list(list(abd, gr, imat)), where abundances have
  #                already been integrated
  #         lambda - rate of exponential random variable describing time between interactions
  #         intTime - time over which interactions are to be simulated
  #         interSmplMult - the fraction of a sample's abundances that get transmitted
  #                         upon contact with another sample (transmission is bi-directional
  #                         and does not deplete the abundance of the dog from which it is 
  #                         transmitted)
  #         intIx - the indices of the interacting samples in the cohort. It is assumed that any of these
  #                 samples can interact with any other one (e.g. form a completely connected subgraph). The
  #                 samples that are not interacting do not interact at all.
  #
  #  RETURNS:  abdList - list of interacted and re-integrated abundances from chrt
  #            intIx - numerical vector containing the indices of all samples within the cohort
  #                    that are connected (capable of interacting) with at least one other sample.
  #                    Only returned if retIntIx == TRUE, in which case the output is of the form
  #                    list(abdList, intIx)
  #
  #  NOTE: exactly one interaction occurs during each iteration of the loop. If cumTime is less
  #        than intTime but the next wait time makes cumTime greater than intTime then the interaction
  #        corresponding to that wait time does not occur.
  
  source("eulerIntegrate.r")
  
  #  precompute coefficient for use in interaction step
  tempSmplMult = 1 - interSmplMult
  
  #  track cumulative time and terminate interaction step once cumulative time exceeds intTime
  cumTime = rexp(n = 1, rate = lambda)
  
  #  interact samples until the cumulative interaction time exceeds the amount of time for the interaction
  #  step
  while(cumTime < intTime){
    
    #  select indices of the two samples which will interact during this interaction step
    iPair = sample(intIx, 2)
    
    #  select the abundance vectors of the interacting samples
    abd1 = chrt[[iPair[1]]][[1]]
    abd2 = chrt[[iPair[2]]][[1]]
    
    #  compute weighted sum
    tempSum = interSmplMult * {abd1 + abd2}
    
    #  update sample abundances in the cohort (the net effect is that interSmplMult * abd1 is added to abd2,
    #  and similarly interSmplMult * abd2 is added to abd1)
    chrt[[iPair[1]]][[1]] = tempSmplMult * abd1 + tempSum
    chrt[[iPair[2]]][[1]] = tempSmplMult * abd2 + tempSum
    
    #  re-integrate samples that have interacted
    for(ii in iPair){
      chrt[[ii]][[1]] = eulerIntegrate(chrt[[ii]], threshold = threshold, maxSteps = maxSteps,
                                       tStep = tStep)
    } #  end for
    
    #  compute the cumulative time after waiting for another interaction
    cumTime = cumTime + rexp(n = 1, rate = lambda)
    
  } #  end while loop
  
  #  extract the list of abundance vectors from the cohort
  abdList = lapply(chrt, "[[", 1)
  
  return(abdList)
  
} #  end intraCohortInteraction_optimizedBaseCase function
