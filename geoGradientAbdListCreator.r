#  James Rekow

geoGradientAbdListCreator  = function(M = 100){
  
  #  ARGS: M - number of spatial grid points. These are the samples in this model. Schnerb et al. used 
  #             = 500.
  #
  #  RETURNS:
  
  #  number of species
  N = 200
  
  #  spatial grid of 500 geographical sites
  x = 1:M
  
  createGeoSpeciesAbd = function(placeholder = NULL){
    
    slope = rnorm(n = 1, mean = 0, sd = 1)
    intercept = rnorm(n = 1, mean = 0, sd = 50)
    
    computeGeoAbd = function(z) slope * z + intercept
    
    geoAbd = sapply(x, computeGeoAbd)
    
    geoAbd[geoAbd < 0] = 0
    
    return(geoAbd)
    
  } #  end createGeoSpeciesAbd function
  
  geoSpeciesMat = sapply(1:N, createGeoSpeciesAbd)
  
  extractAbdGeoSample = function(n) geoSpeciesMat[n, ]
  
  abdList = lapply(as.list(x), extractAbdGeoSample)
  
  return(abdList)
  
} #  end  geoGradientAbdListCreator  function
