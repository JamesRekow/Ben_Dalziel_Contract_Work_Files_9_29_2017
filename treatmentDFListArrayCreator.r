#  James Rekow

#  when abdList[[pairs[, i][1]]] : subscript out of bounds happens, just return NA in DOCProcedure
#  function to prevent unnecessary termination of parent process. I should adopt this philosophy in
#  any helper functions that become problematic only rarely but happen to interupt slower parent
#  processes.

treatmentDFListListArrayCreator = function(M = 80, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                           thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                           intTime = 1, interSmplMult = 0.01, lambda = 10 ^ (-1),
                                           synthM = floor(M / 4), numSynthChrt = 10, numSample = 100,
                                           maxIters = 100){
  
  #  ARGS:
  #
  #  RETURNS:
  
  source("conGraphCreator.r")
  source("DOCNSDFListCreator.r")
  
  # lambdaVec = c(0.05, 0.1, 0.15)
  # graphTypeVec = c("smallWorld", "scaleFree", "gnp", "gnm")
  # meanDegreeVec = c(2, 4, 6)
  
  lambdaVec = c(0.05, 0.1, 0.15)
  graphTypeVec = c("smallWorld", "scaleFree", "gnp", "gnm")
  meanDegreeVec = c(4, 6)
  
  x1 = 1:length(lambdaVec)
  x2 = 1:length(graphTypeVec)
  x3 = 1:length(meanDegreeVec)
  
  g = function(x, y) sapply(1:length(x), function(n) list(append(x[[n]], y[[n]])))
  
  coordArray = outer(x1, outer(x2, x3, g), g)
  
  produceTreatmentDFListList = function(coords){
    
    #  ARGS:
    #
    #  RETURNS:
    
    #  unlist the coordinates in the input
    coords = unlist(coords)
    
    #  extract the parameter values corresponding to the coordinate vector
    lambda = lambdaVec[coords[1]]
    graphType = graphTypeVec[coords[2]]
    meanDegree = meanDegreeVec[coords[3]]
    
    # NOTE: coords in array of the from c(lambda, graphType, meanDegree)
    
    #  create connectivity graph using the graph type and mean degree parameters
    conGraph = conGraphCreator(graphType = graphType, meanDegree = meanDegree, v = M)
    
    treatmentDFListCreator = function(x = NULL){
      return(
        DOCNSDFListCreator(conGraph = conGraph, M = M, N = N, iStrength = iStrength,
                           univ = univ, sigmaMax = sigmaMax,
                           thresholdMult = thresholdMult, maxSteps = maxSteps,
                           tStep = tStep, intTime = intTime,
                           interSmplMult = interSmplMult, lambda = lambda,
                           synthM = synthM, numSynthChrt = numSynthChrt,
                           numSample = numSample, maxIters = maxIters)
      ) #  end return
    } #  end treatmentDFListCreator function
    
    treatmentDFListList = lapply(as.list(1:10), treatmentDFListCreator)
    
    return(treatmentDFListList)
    
  } #  end produceTreatmentDFListList function
  
  treatmentDFListListArray = apply(coordArray, 1:3, produceTreatmentDFListList)
  
  return(treatmentDFListListArray)
  
} #  end treatmentDFListListArrayCreator function
