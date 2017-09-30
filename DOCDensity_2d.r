#  James Rekow

DOCDensity_2d = function(doc, densLim = 10){
  
  #  ARGS: doc - input DOC
  #        densLim - maximum density displayed in plot. Regions with higher densities are plotted in
  #                  grey
  #
  #  RETURNS: docDens - ggplot object corresponding to the 2d density of the input DOC
  
  library(ggplot2)
  
  #  guarantee that input doc is a data frame (by default a doc is a named list)
  doc = data.frame(doc)
  
  #  create ggplot object
  docDens = ggplot(doc, aes(x = over, y = diss)) +
            stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
            scale_fill_gradient2(limits = c(0, densLim)) +
            coord_cartesian(xlim = c(0.5, 1), ylim = c(0, 0.3), expand = c(0, 0))
  
  return(docDens)
  
} #  end DOCDens function
