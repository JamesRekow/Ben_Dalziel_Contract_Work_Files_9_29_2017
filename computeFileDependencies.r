#  James Rekow

computeFileDependencies = function(input){
  
  #  ARGS:
  #
  #  RETURNS: dependenciesList - list of the form list(librariesVec, filesVec), where librariesVec is a
  #                              character vector of all libraries upon which input depends, and
  #                              filesVec is a character vector of all files upon which input depends,
  #                              including itself
  
  source("computeDirectFileDependencies.r")
  
  noFileDependencies = is.null(computeDirectFileDependencies(unlist(input)))
  
  input  = as.list(unlist(input))
  
  if(noFileDependencies){
    return(input)
  } else{
    
    #  compute direct file dependencies for each file in the input list
    directFileDependenciesList = lapply(as.list(unlist(input)), computeDirectFileDependencies)
    
    #  recursively call function until each file dependency has been computed
    return(lapply(directFileDependenciesList, computeFileDependencies))
    
  } #  end if/else
  
} #  end computeFileDependencies function
