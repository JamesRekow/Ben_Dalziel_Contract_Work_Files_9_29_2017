#  James Rekow

conGraphCreator = function(graphMethod, graphArgs, M){
  
  #  ARGS: graphMethod - method used to create connectivity graph
  #        graphArgs - a named list of values to be used in the method specified by graphMethod.
  #
  #  RETURNS: conGraph - connectivity graph whose vertices represent samples and whose edges
  #                      represent pairs of samples with the potential to interact
  
  #  current graphMethods and required graphArgs:
  #    "cSub": numI
  #    "smallWorld": numI, dim, nei, p
  #    "ranGNM": numI, p.or.m
  #    "ranGNP": numI, p.or.m
  #    "scaleFree": numI, power
  
  library(igraph)
  source("embedGraph.r")
  
  #  list of currently available methods
  methodList = c("cSub", "smallWorld", "ranGNM", "ranGNP", "scaleFree")
  
  #  check that the supplied graphMethod is supported
  if(!(graphMethod %in% methodList)){
    stop(paste("In conGraphCreator an invalid graphMethod was selected. Currently
               available graphMethods are:", paste(methodList, collapse = ", "), "."))
  } #  end if
  
  ##  implement graphMethod to create connectivity graph for interacting samples
  
  if(graphMethod == "cSub"){
    
    args = c(graphArgs, list(n = graphArgs$numI, directed = FALSE, loops = FALSE))
    args$numI = NULL
    intGraph = do.call(make_full_graph, args)

  } #  end csub
  
  if(graphMethod == "smallWorld"){
    
    args = c(graphArgs, list(size = graphArgs$numI, loops = FALSE, multiple = FALSE))
    args$numI = NULL
    intGraph = do.call(sample_smallworld, args)

  } #  end smallworld
  
  if(graphMethod == "ranGNM"){
  
    args = c(graphArgs, list(n = graphArgs$numI, type = "gnm", directed = FALSE, loops = FALSE))
    args$numI = NULL
    intGraph = do.call(erdos.renyi.game, args)

  } #  end ranGNM
  
  if(graphMethod == "ranGNP"){
    
    args = c(graphArgs, list(n = graphArgs$numI, type = "gnp", directed = FALSE, loops = FALSE))
    args$numI = NULL
    intGraph = do.call(erdos.renyi.game, args)

  } #  end ranGNP
  
  if(graphMethod == "scaleFree"){
    
    args = c(graphArgs, list(n = graphArgs$numI, algorithm = "psumtree", directed = FALSE,
                             out.pref = TRUE))
    args$numI = NULL
    intGraph = do.call(sample_pa, args)

  } #  end scaleFree
  
  #  embed connectivity graph inside graph representing entire base cohort
  conGraph = embedGraph(M = M, intGraph = intGraph)
  
  return(conGraph)
  
  } #  end conGraphCreator function
