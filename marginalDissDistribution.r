#  James Rekow

marginalDissDensity = function(M = 40, numSubgroups = 2, N = 20, iStrength = 1, univ = 1, sigmaMax = 0.1,
                                    thresholdMult = 10 ^ (-1), maxSteps = 10 ^ 4, tStep = 10 ^ (-2),
                                    intTime = 1, interSmplMult = 0.01, lambda = 0, returnParams = FALSE,
                                    conGraph = NULL, overRange = 0.05, highOver = 0.9){
  
  #  ARGS: numSubgroups - number of subgroups in the cohort
  #        rollRange - the maximum difference in overlap between two DOC points for which the two points
  #                    are still considered close enough that they are included in each others local
  #                    dissimilarity profiles (e.g. in computing rolling mean, etc.)
  #
  #  RETURNS:
  
  library(ggplot2)
  library(grid)
  library(gridExtra)
  source("subgroupsAbdListCreator.r")
  source("DOCProcedure.r")
  
  #  default indices of the 2 subgroups created from subgroupsAbdListCreator
  subgroup1Ix = 1:20
  subgroup2Ix = 21:40
  
  #  create abdList with desired number of subgroups
  abdList = subgroupsAbdListCreator(M = M, numSubgroups = numSubgroups, N = N, iStrength = iStrength,
                                    univ = univ, sigmaMax = sigmaMax, thresholdMult = thresholdMult,
                                    maxSteps = maxSteps, tStep = tStep, intTime = intTime,
                                    interSmplMult = interSmplMult, lambda = lambda,
                                    returnParams = returnParams, conGraph = conGraph)
  
  #  compute DOC
  doc = DOCProcedure(abdList)
  
  #  make DOC a data frame (by default a doc is a named list)
  doc = data.frame(over = doc$x, diss = doc$y)
  
  #  create matrix whose ith column is the pair of indices corresponding to the ith DOC point in the doc
  #  data frame
  ixMat = combn(1:M, 2)
  
  #  check whether either of the indices corresponding to a given DOC point come from a sample in
  #  subgroup 1(2)
  ixInSubgroup1 = apply(ixMat, 2, function(x) {x[1] %in% subgroup1Ix} || {x[2]} %in% subgroup1Ix)
  ixInSubgroup2 = apply(ixMat, 2, function(x) {x[1] %in% subgroup2Ix} || {x[2]} %in% subgroup2Ix)
  
  #  check which DOC points come from samples in the same or different subgroups
  diffSubgroup = ixInSubgroup1 & ixInSubgroup2
  sameSubgroup = !diffSubgroup
  
  #  add column to data frame
  doc$sameSubgroup = sameSubgroup
  
  #  create ggplot coloring DOC points based on classification as same or different subgroups (of
  #  corresponding samples)
  p1 = ggplot(doc, aes(x = over, y = diss, col = sameSubgroup)) + geom_point()
  
  ggplotSubgroupDissDens = function(over){
    
    #  ARGS: over - numeric overlap value
    #
    #  RETURNS: densPlot - ggplot object plotting the dissimilarity density of DOC points with overlap
    #                      similar to over, grouped by sameSubgroup
    
    #  identify indices of DOC points with similar overlap as input overlap value
    similarOver = abs(doc$over - over) < overRange
    
    #  create DOC data frame with only the points that have similar overlap to the input over value
    similarOverDOC = subset(doc, similarOver == TRUE)
    
    #  create ggplot object plotting the two marginal dissimilarity densities
    densPlot = ggplot(similarOverDOC, aes(diss, col = sameSubgroup)) + geom_density() + xlab("") + ylab("")
    
    return(densPlot)
    
  } #  end ggplotSubgroupDissDens
  
  #  compute mean overlap
  meanOver = mean(doc$over)
  
  #  create ggplot objects for the marginal dissimilarity distributions
  meanOverDissDensPlot = ggplotSubgroupDissDens(over = meanOver)
  highOverDissDensPlot = ggplotSubgroupDissDens(over = highOver)
  
  titleText = "Marginal Dissimilarity Densities"
  p2 = grid.arrange(arrangeGrob(meanOverDissDensPlot, top = "Mean Overlap"),
                    arrangeGrob(highOverDissDensPlot, top = paste("High Overlap: ", highOver)),
                    ncol = 2, left = "Density", bottom = "Dissimilarity", top = titleText)
  
  #  plot first ggplot object in new graphics device
  dev.new()
  plot(p1)
  
  #  plot second ggplot object in new graphics device
  dev.new()
  plot(p2)
  
  return(doc)
  
} #  end marginalDissDensity function
