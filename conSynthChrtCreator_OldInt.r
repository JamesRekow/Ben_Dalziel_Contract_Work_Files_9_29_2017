#  James Rekow

conSynthChrtCreator_OldInt = function(synthM, abdList, conGraph, int = TRUE){
  
  #  ARGS:  synthM - number of samples in synthetic cohorts
  #         abdList - input abundance list from the base cohort
  #         conGraph - connectivity graph describing interactions between samples in base cohort
  #         int - a boolean indicating whether the synthetic cohort should be created from
  #               interacting samples (int == TRUE) or non-interacting samples (int == FALSE)
  #               from the original cohort
  #
  #  RETURNS: synthChrt - a synthetic cohort containing the abundance vectors of synthM samples
  #                       from the base cohort that form a connected subgraph.
  
  ##  NOTE: for now, this function only applies to connected subgraphs formed using the "cSub"
  #         procedure. In the future, it will be extended to construct an adjacency matrix
  #         and use that to produce random connected subgraphs for subgraphs generated using
  #         any procedure
  
  library(igraph)
  
  #  compute the number of samples in the base cohort
  M = length(abdList)
  
  #  store the indices of samples that can interact
  ix = unique(c(ends(conGraph, E(conGraph))))
  
  if(!int){
    #  if creating a non-interacting cohort, instead store the indices of samples 
    #  that can't interact
    ix = (1:M)[-ix]
  } #  end if
  
  #  ensure that the number of samples in the synthetic cohort does not exceed the number of
  #  interacting (if int == TRUE) or non-interacting (if int == FALSE) samples in the base cohort
  if(synthM > length(ix)){
    stop("In conSynthChrtListCreator the number of samples in the synthetic cohorts is larger
         than the number of interacting or non-interacting samples in the base cohort. synthM is
         too large.")
  } #  end if
  
  synthChrt = abdList[sample(ix, synthM)]
  
  return(synthChrt)
  
  } #  end conSynthChrtCreator_OldInt function
