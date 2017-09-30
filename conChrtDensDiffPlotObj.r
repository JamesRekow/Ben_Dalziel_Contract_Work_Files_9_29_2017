#  James Rekow

conChrtDensDiffPlotObj = function(M = 80, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                 thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                 intTime = 1, interSmplMult = 0.01, lambda = 10 ^ (-1),
                                 conGraph = NULL, graphType = "gnp", meanDegree = 4,
                                 synthM = floor(M / 4), numSynthChrt = 10, dispPlots = FALSE,
                                 numSample = 100, maxIters = 100){
  
  #  ARGS: conGraph - supplied connectivity graph
  #        graphMethod - method to use to create conGraph if one is not supplied
  #        graphArgs - arguments to pass to the graphMethod
  #        synthM - number of samples in each synthetic cohort
  #        numSynthChrt - number of synthetic cohorts (there will be this many of both the
  #                       interacting and non-interacting cohorts)
  #        dispPlots - if TRUE, a plot of the interacting samples in conGraph and a plot
  #                    of the density difference will be displayed. If FALSE, the ggplot
  #                    object corresponding to the density difference plot will be returned,
  #                    but nothing will be displayed.
  #
  #  RETURNS: a ggplot object displaying the difference between the 2D density of the aggregate OD NS
  #           points of the interacting synthetic cohorts and the non-interacting synthetic cohorts.
  
  #  import libraries and functions
  library(igraph)
  library(snow)
  source("conGraphCreator.r")
  source("abdListCreator.r")
  source("DOCProcedure.r")
  source("conSynthChrtCreator.r")
  source("DOCNSSelector.r")
  source("plotDensDiff.r")

  ##  check that all of the information supplied to create conGraph is valid and consistent
  
  #  check which conGraph related inputs have been supplied
  suppliedGraph = !is.null(conGraph)
  
  #  if conGraph was supplied, check that it is valid
  if(suppliedGraph){
    
    #  warn user that extraneous parameters will be ignored if conGraph is already given
    if(suppliedMethod || suppliedArgs){
      warning("conGraph was supplied, so graphMethods and graphArgs will be ignored.")
    } #  end if
    
    #  check that conGraph is undirected
    if(is.directed(conGraph)){
      stop("conGraph must be undirected.")
    } #  end if
    
    #  check that conGraph has the appropriate number of vertices
    if(length(V(conGraph)) != M){
      stop("conGraph must have M vertices.")
    } #  end if
    
  } #  end if suppliedGraph
  
  #  if conGraph is not supplied, create conGraph using graphType and meanDegree
  if(!suppliedGraph){
    
    conGraph = conGraphCreator(graphType = graphType, meanDegree = meanDegree, v = M)
    
  } #  end if !suppliedGraph
  
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
    
    #  function to create a DOC for a synthetic cohort and remove points in the DOC that are outside
    #  the NS region
    produceNSPoints = function(synthChrt) DOCNSSelector(DOCProcedure(synthChrt))
    
    #  create a list of DOCNS's by applying produceNSPoints to the list of synthetic cohorts
    docList = lapply(synthChrtList, produceNSPoints)
    
    #  create vectors of aggregate overlap and dissimilarity values (e.g. combine overlap values from
    #  each synthetic cohorts NS region into a single vector, similarly for dissimilarity values)
    aggOver = unlist(lapply(docList, "[[", 1))
    aggDiss = unlist(lapply(docList, "[[", 2))
    
    #  create a DOCNS data frame using the aggregate overlap and dissimilarity vectors
    DOCNSDF = data.frame(x = aggOver, y = aggDiss)
    
    return(DOCNSDF)
    
  } #  end produceSynthDOCNS function
  
  #  produce a list of the aggregate data frames containing the OD NS points from the
  #  non-interacting and interacting synthetic cohorts
  docDFList = lapply(list(FALSE, TRUE), produceSynthDOCNSDF)
  
  #  create ggplot object corresponding to density difference plot between the aggregate
  #  OD NS points
  p = plotDensDiff(docDFList[[1]], docDFList[[2]])
  
  #  if dispPlots is TRUE, plot conGraph and the density difference plot
  if(dispPlots){
    #  fix the subgraph display to only display interacting vertices. Might have to do
    #  this manually
    dispGraph = delete_vertices(conGraph, (1:M)[-unique(ends(conGraph, E(conGraph)))])
    conGraphLayout = layout_with_fr(dispGraph)
    plot(dispGraph, vertex.size = 10, vertex.color = "orangered", vertex.label = NA,
         edge.color = "orangered", layout = conGraphLayout)
    
    dev.new()
    plot(p)
    
  } #  end if dispPlots
  
  return(p)
  
} #  end conChrtDensDiffPlotObj function
