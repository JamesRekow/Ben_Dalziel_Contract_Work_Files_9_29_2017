#  James Rekow

#  Example script for creating a meanOverDOC data frame and using the data frame to create some basic plots

library(ggplot2)
source("meanOverDOCCreatorOrderCheck.r")
source("meanDOCCreatorSubgroupsCheck.r")

#  create data frames containing mean overlap values
#  meanOverDOC = meanOverDOCCreatorOrderCheck()
#  meanDOC = meanDOCCreatorSubgroupsCheck()

#  read in data from file
meanOverDOC = read.csv("meanOverDOCOrderCheck.csv")
meanDOC = read.csv("meanDOC_SubgroupsCheck.csv")

#  make meanOrder and numSubgroups factors for plotting purposes
meanOverDOC$meanOrder = as.factor(meanOverDOC$meanOrder)
meanDOC$numSubgroups = as.factor(meanDOC$numSubgroups)

##  Using varying mean order from meanOverDOC

#  Plot lambda / M vs. meanOver
p1 = ggplot(meanOverDOC, aes(x = lambdaOverM, y = meanOver, col = meanOrder)) +
       geom_point() +
       geom_smooth(method = "loess", se = FALSE) +
       labs(title = "Mean Overlap VS lambda / M",
            subtitle = "meanOrder is the Mean Order of the Vertices in the Random Connectivity Graph Related to Each DOC. 
                        meanOrder = 2 * numEdges / numVertices")

##  Plot fitCurve (1 - exp(-lambda / M)) vs. meanOver
p2 = ggplot(meanOverDOC, aes(x = fitCurve, y = meanOver, col = meanOrder)) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       labs(title = "Mean Overlap VS fitCurve = 1 - exp(-lambda / M)",
            subtitle = "meanOrder is the Mean Order of the Vertices in the Random Connectivity Graph Related to Each DOC. 
                        meanOrder = 2 * numEdges / numVertices")

## Using varying number of subgroups from meanDOC

#  Plot lambda / M vs. meanOver
p3 = ggplot(meanDOC, aes(x = lambdaOverM, y = meanOver, col = numSubgroups)) +
       geom_point() +
       geom_smooth(method = "loess", se = FALSE) +
       labs(title = "Mean Overlap VS lambda / M")

#  Plot lambda / M vs. meanDiss
p4 = ggplot(meanDOC, aes(x = lambdaOverM, y = meanDiss, col = numSubgroups)) +
       geom_point() +
       geom_smooth(method = "loess", se = FALSE) +
       labs(title = "Mean Dissimilarity VS lambda / M")

#  plot the created ggplot objects
dev.new()
plot(p1)

dev.new()
plot(p2)

dev.new()
plot(p3)

dev.new()
plot(p4)
