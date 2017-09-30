#  James Rekow

densDiffMultiPlot = function(M = 80, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                             thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                             intTime = 1, interSmplMult = 0.01, lambda = 10 ^ (-1),
                             conGraph = NULL, graphType = "gnp", meanDegree = 4,
                             synthM = floor(M / 4), numSynthChrt = 10, numSample = 100,
                             maxIters = 100){
  
  #  ARGS: conGraph - supplied connectivity graph
  #        graphMethod - method to use to create conGraph if one is not supplied
  #        graphArgs - arguments to pass to the graphMethod
  #        synthM - number of samples in each synthetic cohort
  #        numSynthChrt - number of synthetic cohorts (there will be this many of both the
  #                       interacting and non-interacting cohorts)
  #
  #  RETURNS: a figure containing 9 plots. Each plot is the difference between the 2D density
  #           of the aggregate OD NS points of the interacting synthetic cohorts and the
  #           non-interacting synthetic cohorts. Each plot uses a different base cohort.
  #
  #  NOTE: Steps in construction of each density difference plot: first, a base cohort is created
  #        with synthM interacting samples and M - synthM non-interacting samples. Next, conGraph
  #        is created (if not supplied). Then the edge list of conGraph is used to determine which
  #        pairs of samples can interact during the intra-cohort interaction step. conGraph should
  #        have no loops or multiple edges. The samples in the base cohort are then integrated and
  #        interacted as usual (using the edge list from con graph to determine possible interactions).
  #        Next, numSynthChrt interacting synthetic cohorts are created. Each one is created by
  #        randomly selecting synthM interacting samples from the base cohort. For each synthetic
  #        cohort the points in the NS region of the OD plane are selected and stored in a data
  #        frame along with the points from all of the other interacting synthetic cohorts. The same
  #        is done for the non-interacting synthetic cohorts. Then the 2D density of the aggregate
  #        OD NS points (the collection of points from all of the synthetic cohorts) is computed
  #        for the interacting and non-interacting aggregates. The density of the non-interacting
  #        aggregate is subtracted from the density of the interacting aggregate, and this density
  #        difference is plotted. Regions where the density difference is negative (non-interacting
  #        aggregate has a higher density than interacting aggregate) are colored blue, and regions
  #        where the density difference is positive (interacting aggregate has a higher density than
  #        non-interacting aggregate) are colored red.
  
  #  import libraries
  library(gridExtra)
  library(grid)
  library(snow)
  library(parallel)
  
  #  function used to produce the ggplot object for one density difference plot
  producePlot = function(x = NULL){
    source("conChrtDensDiffPlotObj.r")
    return(
      conChrtDensDiffPlotObj(M = M, N = N, iStrength = iStrength, univ = univ, sigmaMax = sigmaMax,
                             thresholdMult = thresholdMult, maxSteps = maxSteps, tStep = tStep,
                             intTime = intTime, interSmplMult = interSmplMult, lambda = lambda,
                             conGraph = conGraph, graphType = graphType, meanDegree = meanDegree,
                             synthM = synthM, numSynthChrt = numSynthChrt, numSample = numSample,
                             maxIters = maxIters)
    ) #  end return
  } #  end producePlot function
  
  #  preallocate list to store plot objects
  pList = vector("list", 9)
  
  #  create 9 plot objects, printing the number created so far
  for(ii in 1:9){
    
    pList[[ii]] = producePlot()
    print(ii)
    
  } #  end for loop
  
  # #  make clusters to compute in parallel
  # cl = makeCluster(8, type = "SOCK")
  # 
  # #  export function to each cluster
  # clusterExport(cl = cl, list = list("producePlot", "conGraph"), envir = environment())
  # 
  # #  produce an aggregate doc for non-interacting and interacting synthetic cohorts
  # pList = parLapply(cl, as.list(1:9), producePlot)
  # 
  # #  remove the clusters and close connections
  # stopCluster(cl)
  
  ##  create text for graph arguments
  graphArgsText = NULL
  
  if(is.null(conGraph)){
    graphArgsText = paste("graphType = ", graphType, "meanDegree = ", meanDegree)
  } # end if
  
  if(!is.null(conGraph)){
    graphArgsText = "conGraph: custom"
  } #  end if
  
  #  create title object
  main = textGrob(paste("Aggregate Synthetic Cohort Density Differences \n M = ", M, "N = ", N,
                        "synthM = ", synthM, "numSynthChrt = ", numSynthChrt, graphArgsText))
  
  #  create a new graphical device
  dev.new()
  
  #  arrange the 9 plots in a 3 x 3 grid, add title, and plot them
  grid.arrange(pList[[1]], pList[[2]], pList[[3]], pList[[4]], pList[[5]], pList[[6]], pList[[7]],
               pList[[8]], pList[[9]], nrow = 3, ncol = 3, top = main)
  
} #  end densDiffMultiPlot function
