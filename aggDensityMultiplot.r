#  James Rekow


aggDensityMultiplot = function(aggDOCDF, densLim = 20){
  
  #  ARGS: aggDOCDF - an aggregate data frame containing DOC points from multiple replicates labeled
  #                   by replicate number and the number of interacting samples corresponding to each
  #                   point
  #        densLim - maximimum density displayed in the output density multiplot. Regions where the density
  #                  exceeds the maximum are displayed in grey
  #
  #  RETURNS: a plot of nince 2D DOC density plots. Each plot is the 2D density of the DOC points from a
  #           single replicate. Nine distinct replicates are randomly chosen from aggDOCDF, which is
  #           assumed to have at least 9 distinct replicates
  
  library(ggplot2)
  library(gridExtra)
  library(grid)
  source("DOCDensity_2d.r")
  
  replicateDensity = function(n){
    
    #  ARGS: n - replicate id
    #
    #  RETURNS: replicateDens - a ggplot object corresponding to the 2D DOC density of replicate n
    
    #  select the DOC points from the given replicate
    replicateDOCDF = subset(aggDOCDF, replicateID == n, select = c("over", "diss"))
    
    #  create the ggplot object plotting the 2D DOC density of the selected DOC points
    replicateDens = DOCDensity_2d(doc = replicateDOCDF, densLim = densLim)
    
    return(replicateDens)
    
  } #  end replicateDensity function
  
  #  create a list of the replicate IDs present in the aggregate data frame
  replicateIDList = as.list(unique(aggDOCDF$replicateID))
  
  #  create the 2D DOC density ggplot object for each replicate
  densGrobList = lapply(replicateIDList, replicateDensity)
  
  #  create title object for the multiplot (must manually change this to include correct M, lambda values)
  titleText = textGrob("DOC Density of 9 Replicates, M = 400, 1 - exp(-lambda/M) = 0.5")
  
  #  arrange and plot all 9 density plots in a new graphics device
  dev.new()
  grid.arrange(grobs = sample(densGrobList, 9), nrow = 3, ncol = 3, top = titleText)
  
} #  end aggDensityMultiplot function
