#  James Rekow

#  a good example of title with subtitles, changing an axis label, plotting by factor, and doing a linear
#  curve fit to the data, with a separate fitting done for each factor level

#  dredd was a data frame
#  meanOrderZero was a factor with two levels

library(ggplot)

p = ggplot(dredd, aes(x = fitCurve, y = meanOver, col = meanOrderZero)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Mean Overlap of DOCs with Various Values of M, lambda, and meanOrder",
           subtitle = "meanOrder is the Mean Order of the Vertices in the Random Connectivity Graph Related to Each DOC, meanOrder = 2 * numEdges / numVertices, meanOrder = 0, 13, 26, 39") +
      xlab("1 - exp(-lambda / M)")

plot(p)
