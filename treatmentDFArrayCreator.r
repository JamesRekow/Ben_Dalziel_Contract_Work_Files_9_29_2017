#  James Rekow

treatmentDFListArrayCreator = function(M = 80, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                       thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                       intTime = 1, interSmplMult = 0.01, lambda = 10 ^ (-1),
                                       synthM = floor(M / 4), numSynthChrt = 10, numSample = 100,
                                       maxIters = 100){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("conGraphCreator.r")
  source("DOCNSDFListCreator.r")
  
  lambdaVec = c(0.05, 0.1, 0.15)
  graphTypeVec = c("smallWorld", "scaleFree", "gnp", "gnm")
  meanDegreeVec = c(2, 4, 6)
  
  x1 = 1:length(lambdaVec)
  x2 = 1:length(graphTypeVec)
  x3 = 1:length(meanDegreeVec)
  
  g = function(x, y) sapply(1:length(x), function(n) list(append(x[[n]], y[[n]])))
  
  coordArray = outer(x1, outer(x2, x3, g), g)
  
  produceTreatmentDF = function(coords){
    
    #  ARGS:
    #
    #  RETURNS:
    
    coords = unlist(coords)
    lambda = lambdaVec[coords[1]]
    graphType = graphTypeVec[coords[2]]
    meanDegree = meanDegreeVec[coords[3]]
    
    conGraph = conGraphCreator(graphType = graphType, meanDegree = meanDegree, v = M)
    
    
    
  } #  end produceTreatmentDF function
  
  return(treatmentDFListArray)
  
} #  end treatmentDFListArrayCreator function
