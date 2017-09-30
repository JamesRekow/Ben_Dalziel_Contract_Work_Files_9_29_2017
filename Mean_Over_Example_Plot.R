#  James Rekow

#  Example script for creating a meanOverDOC data frame and using the data frame to create some basic plots

library(ggplot)
source("meanOverDOCCreatorOrderCheck.r")

#  create data frame containing mean overlap values
#meanOverDOC = meanOverDOCCreatorOrderCheck()

meanOverDOC = read.csv("meanOverDOCOrderCheck.csv")

##  Plot lambda / M vs. meanOver
#  create a ggplot object plotting the values of lambda / M vs. mean overlap, grouped by mean order
p1 = ggplot(meanOverDOC, aes(x = lambdaOverM, y = meanOver, col = meanOrder)) +
       geom_point() +
       geom_smooth(method = "loess", se = FALSE) +
       labs(title = "Mean Overlap of DOCs with Various Values of M, lambda, and meanOrder",
            subtitle = "meanOrder is the Mean Order of the Vertices in the Random Connectivity Graph Related to Each DOC. 
                        meanOrder = 2 * numEdges / numVertices")

##  Plot fitCurve (1 - exp(-lambda / M)) vs. meanOver
#  create a ggplot object plotting the values of lambda / M vs. mean overlap, grouped by mean order
p2 = ggplot(meanOverDOC, aes(x = fitCurve, y = meanOver, col = meanOrder)) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       labs(title = "Mean Overlap of DOCs with Various Values of M, lambda, and meanOrder",
            subtitle = "meanOrder is the Mean Order of the Vertices in the Random Connectivity Graph Related to Each DOC. 
                        meanOrder = 2 * numEdges / numVertices")

#  plot the created ggplot objects
dev.new()
plot(p1)

dev.new()
plot(p2)
