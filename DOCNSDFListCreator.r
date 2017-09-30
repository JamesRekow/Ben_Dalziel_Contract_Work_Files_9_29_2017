#  James Rekow

#  output array has each value as a list of 1 instead of 2 DF's, fix this

DOCNSDFListCreator = function(conGraph, M = 80, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                              thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                              intTime = 1, interSmplMult = 0.01, lambda = 10 ^ (-1),
                              synthM = floor(M / 4), numSynthChrt = 10, numSample = 100,
                              maxIters = 100){
  
  #  ARGS: conGraph - supplied connectivity graph
  #        synthM - number of samples in each synthetic cohort
  #        numSynthChrt - number of synthetic cohorts (there will be this many of both the
  #                       interacting and non-interacting cohorts)
  #
  #  RETURNS:
  
  #  import libraries and functions
  library(igraph)
  library(snow)
  source("abdListCreator.r")
  source("DOCProcedure.r")
  source("conSynthChrtCreator.r")
  source("DOCNSSelector.r")
  source("plotDensDiff.r")
  
  #  check that conGraph is undirected
  if(is.directed(conGraph)){
    stop("conGraph must be undirected.")
  } #  end if
    
  #  check that conGraph has the appropriate number of vertices
  if(vcount(conGraph) != M){
    stop("conGraph must have M vertices.")
  } #  end if
  
  #  create abundance list after interacting cohort using the connectivity given in conGraph
  abdList = abdListCreator(M = M, N = N, iStrength = iStrength, univ = univ, sigmaMax = sigmaMax,
                           thresholdMult = thresholdMult, maxSteps = maxSteps, tStep = tStep,
                           intTime = intTime, interSmplMult = interSmplMult, lambda = lambda,
                           returnParams = FALSE, conGraph = conGraph)
  
  #  create a synthetic cohort from abdList and return the points in the NS region of the OD
  #  plane
  #  create a data frame of the aggregate OD NS region points from numSynthChrt number of
  #  synthetic cohorts
  produceSynthDOCNSDF = function(int){
    
    #  create synthetic cohort
    synthChrtList = conSynthChrtCreator(synthM = synthM, conGraph = conGraph, abdList = abdList,
                                        int = int, numSample = numSample, maxIters = maxIters,
                                        numSynthChrt = numSynthChrt)
    
    produceNSPoints = function(synthChrt){
      doc = DOCProcedure(synthChrt)
      docNS = DOCNSSelector(doc)
      
      return(docNS)

    } #  end produceNSPoints function
    
    docList = lapply(synthChrtList, produceNSPoints)
    docList = docList[!unlist(lapply(docList, is.null))]
    
    aggOver = unlist(lapply(docList, "[[", 1))
    dissOver = unlist(lapply(docList, "[[", 2))
    
    DOCNSDF = data.frame(x = aggOver, y = dissOver)
    
    return(DOCNSDF)
    
  } #  end produceSynthDOCNS function
  
  #  produce a list of the aggregate data frames containing the OD NS points from the
  #  non-interacting and interacting synthetic cohorts
  docNSDFList = lapply(list(FALSE, TRUE), produceSynthDOCNSDF)
  
  return(docNSDFList)
  
} #  end DOCNSDFListCreator function
