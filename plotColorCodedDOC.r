#  James Rekow

plotColorCodedDOC = function(M = 40, N = 20, iStrength = 1, univ = 1,
                             sigmaMax = 0.1, thresholdMult = 10 ^ (-2), maxSteps = 10 ^ 4,
                             tStep = 10 ^ (-2), intTime = 1, interSmplMult = 0.01,
                             lambda = 0.1, numDOC = 10){
  
  #  ARGS: numDOC - number of DOCs from which the aggregate points should be assembled
  #
  #  RETURNS:
  
  library(hexbin)
  library(ggplot2)
  source("abdListCreator_optimizedBaseCase.r")
  source("intraCohortInteraction_optimizedBaseCase.r")
  source("DOCProcedure.r")
  
  #  let half of the samples be interacting samples
  numI = floor(M / 2)
  
  produceDOCDF = function(x = NULL){
    
    #  select the indices of the interacting samples
    intIx = sample(M, numI)
    
    #  create abundance list
    abdList = abdListCreator_optimizedBaseCase(intIx = intIx, M = M, N = N, iStrength = iStrength,
                                               univ = univ, sigmaMax = sigmaMax,
                                               thresholdMult = thresholdMult, maxSteps = maxSteps,
                                               tStep = tStep, intTime = intTime,
                                               interSmplMult = interSmplMult, lambda = lambda)
    
    #  create DOC
    doc = DOCProcedure(abdList)
    
    #  create a numerical vector with 1's in positions with the indices of interacting elements
    #  and 0's elsewhere
    intSmpls = rep(0, M)
    intSmpls[intIx] = 1
    
    #  create a matrix whose columns are each unique pair of sample indices
    pairMat = combn(M, 2)
    
    #  function to count the number of interacting samples in a vector of sample indices
    intCounter = function(ixVec) sum(intSmpls[ixVec])
    
    #  numerical vector of the number of samples in each sample pair that were interacting samples
    intCount = apply(pairMat, 2, intCounter)
    
    #  store data in a data frame
    DOCDF = data.frame(intCount = intCount, over = doc$x, diss = doc$y)
    
    return(DOCDF)
    
  } #  end produceDOC function
  
  #  create a list of replicate data frames
  DOCDFList = lapply(as.list(1:numDOC), produceDOCDF)
  
  #  aggregate data from the replicates
  aggDOCDF = Reduce(rbind, DOCDFList)
  
  #  make the intCount variable a factor
  aggDOCDF$intCount = as.factor(aggDOCDF$intCount)
  
  #  create title text
  titleText = "DOC Points"
  
  #  create parameter text
  paramText = paste("M = ", M, "N = ", N, "lambda = ", lambda, "numDOC = ", numDOC)
  
  #  compute the mean overlap of points that correspond to comparisons between samples that are
  #  both non-interacting (intCount == 0), one interacting and one non-interacting (intCount == 1),
  #  or both interacting (intCount == 2)
  meanVec = sapply(0:2, function(x) round(mean(aggDOCDF$over[aggDOCDF$intCount == x]), 2))
  
  #  create text to display means in plot title
  meanText = paste("meanOverlap: ", "nonInt-nonInt = ", meanVec[1], ", nonInt-int = ", meanVec[2],
                   ", int-int = ", meanVec[3])
  
  aggDOCDF = aggDOCDF[aggDOCDF$intCount != 1, ]
  
  ##  create ggplot object
  p = ggplot(aggDOCDF, aes(x = over, y = diss, col = intCount)) +
    #geom_point(alpha = 0.2) +
    stat_bin_2d() +
    coord_cartesian(xlim = c(0.5, 1), ylim = c(0, 0.6)) +
    ggtitle(titleText) +
    annotate("text", x = 0.75, y = 0.6, label = paramText) +
    annotate("text", x = 0.75, y = 0.55, label = meanText)
    
  #  plot the ggplot object in a new graphics window
  dev.new()
  plot(p)
  
} #  end plotColorCodedDOC function
