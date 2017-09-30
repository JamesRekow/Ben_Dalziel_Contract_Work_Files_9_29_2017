#  James Rekow

aggDensityComparativeMultiplot = function(aggDOCDF, densLim = 20){
  
  #  ARGS: aggDOCDF - an aggregate data frame containing DOC points from multiple replicates labeled
  #                   by replicate number and the number of interacting samples corresponding to each
  #                   point
  #        densLim - maximimum density displayed in the output density multiplot. Regions where the density
  #                  exceeds the maximum are displayed in grey
  #
  #  RETURNS: a plot of six 2D DOC density plots arranged into two columns and three rows. Each row
  #           corresponds to a distinct replicate (for 3 total replicates). The left column plots 2D
  #           densities of DOC points corresponding to 0 interacting samples, and the right column
  #           plots 2D densities of DOC points corresponding to 2 interacting samples
  
  library(ggplot2)
  library(gridExtra)
  library(grid)
  source("DOCDensity_2d.r")
  
  #  create vector with all replicate IDs present in the aggregate data frame
  replicateIDs = unique(aggDOCDF$replicateID)
  
  #  randomly select the replicate IDs of the three replicates used in the density multiplot
  displayedReplicateIDs = sample(replicateIDs, 3)

  #  select only DOC points corresponding to 0 or 2 interacting samples, respectively
  aggDOCDF_IC0 = aggDOCDF[aggDOCDF$intCount == 0, ]
  aggDOCDF_IC2 = aggDOCDF[aggDOCDF$intCount == 2, ]

  replicateDensity = function(n){
    
    #  ARGS: n - replicate ID
    #
    #  RETURNS: replicateDensList - list of two ggplot objects of 2D DOC density plots. The first element
    #           in the list, repDensGrob_IC0, is the density plot for DOC points from replicate n
    #           corresponding to 0 interacting samples. The second element, repDensGrob_IC2, is the
    #           density plot for DOC points from replicate n corresponding to 2 interacting samples
    
    #  select DOC points from replicate n
    replicateDOCDF_IC0 = subset(aggDOCDF_IC0, replicateID == n, select = c("over", "diss"))
    replicateDOCDF_IC2 = subset(aggDOCDF_IC2, replicateID == n, select = c("over", "diss"))
    
    #  create the ggplot objects corresponding to the 2D DOC density plots for the points from replicate n
    #  with 0 and 2 interacting samples, respectively
    repDensGrob_IC0 = DOCDensity_2d(doc = replicateDOCDF_IC0, densLim = densLim)
    repDensGrob_IC2 = DOCDensity_2d(doc = replicateDOCDF_IC2, densLim = densLim)
    
    #  create a list to store the two ggplot objects
    replicateDensList = list(repDensGrob_IC0, repDensGrob_IC2)
    
    return(replicateDensList)
    
  } #  end replicateDensity function

  #  create a replicate density list for each of the three displayed replicates
  densGrobList = lapply(as.list(displayedReplicateIDs), replicateDensity)
  
  #  put all 6 ggplot objects in a single list
  densGrobList = unlist(densGrobList, recursive = FALSE)
  
  #  create title object for the multiplot (must manually change this to include correct M, lambda values)
  titleText = textGrob("DOC Densities of 3 Replicates, M = 400, 1 - exp(-lambda/M) = 0.5")
  
  #  arrange and plot all 6 density plots in a new graphics device
  dev.new()
  grid.arrange( arrangeGrob(densGrobList[[1]], densGrobList[[3]], densGrobList[[5]], 
                            top = "0 Interacting Samples"),
                arrangeGrob(densGrobList[[2]], densGrobList[[4]], densGrobList[[6]],
                            top = "2 Interacting Samples"), ncol = 2, top = titleText)
  
} #  end aggDensityComparativeMultiplot function
