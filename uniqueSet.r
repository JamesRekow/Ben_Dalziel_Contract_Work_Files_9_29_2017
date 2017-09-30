#  James Rekow

uniqueSet = function(vList, select = c("ran", "first")){
  
  #  ARGS: vList - list of numerical vectors
  #        select - if ran, for each group of vectors whose set of elements is identical, select one at
  #                 random. If first, select the one the appears first in vList. Defaults to "ran".
  #
  #  RETURNS: uniquevList - a list of elements of vList such that exactly one vector from vList
  #                         corresponding to each unique set of numbers
  
  #  NOTE: this function assumes that each vector in vList contains no duplicate elements
  #  NOTE: this function is exceedingly SLOW!
  
  #  default to "ran" if select is not specified
  if(length(select) == 2){
    select = "ran"
  } #  end if
  
  #  check that the given method for select is supported
  if(!(select %in% c("ran", "first"))){
    stop("Invalid method chosen for select.")
  } #  end if
  
  #  sort the elements of vList so they can be compared to the sorted sets in the list of unique sets
  sortedVList = lapply(vList, sort)
  
  #  create a list of unique sets present in vList
  uniqueSets = unique(lapply(vList, sort))
  
  selectRep = function(set){
    
    #  ARGS: n - the index of the set in uniqueSets for which a representative is to be chosen
    #
    #  RETURNS: repr - a representative of the set chosen from vList according to the method specified
    #                  by the value of the select variable
    
    #  create vector of indices of the elements in vList which correspond to set
    potentialRepIxs = which(sortedVList %in% list(set))
    
    ##  select index (in vList) of set representative
    if(select == "ran"){
      repIx = sample(potentialRepIxs, 1)
    } #  end if
    
    if(select == "first"){
      repIx = potentialRepIxs[1]
    } #  end if
    
    rep = vList[[repIx]]
    
    return(rep)
    
  } #  end selectRep function
  
  #  select a representative for each unique set
  uniqueVList = lapply(uniqueSets, selectRep)
  
  return(uniqueVList)
  
} #  end uniqueSet function
