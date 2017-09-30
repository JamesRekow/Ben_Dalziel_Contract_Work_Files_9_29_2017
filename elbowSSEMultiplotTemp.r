#  James Rekow

elbowSSEMultiplotTemp = function(elbowSSEDFLambda = NULL, elbowSSEDFM, elbowSSEDFNumSubgroups = NULL){
  
  #  lambda: c(150, 30, 0), M: c(40, 100, 160) (uses lambda = 100), numSubgroups: c(2, 5, 8)
  
  #  ARGS:
  #
  #  RETURNS:
  
  library(ggplot2)
  library(gridExtra)
  
  ##  compute meanDF
  
  # SSELambda150 = sapply(1:10, function(n) mean(subset(elbowSSEDFLambda, numCenters == n & lambda == 150)$SSE))
  # SSELambda30 = sapply(1:10, function(n) mean(subset(elbowSSEDFLambda, numCenters == n & lambda == 30)$SSE))
  # SSELambda0 = sapply(1:10, function(n) mean(subset(elbowSSEDFLambda, numCenters == n & lambda == 0)$SSE))
  # 
  # meanNumCenters = rep(1:10, 3)
  # meanSSE = c(SSELambda150, SSELambda30, SSELambda0)
  # meanLambda = as.factor(c(rep(150, 10), rep(30, 10), rep(0, 10)))
  # 
  # meanDF1 = data.frame(numCenters = meanNumCenters, SSE = meanSSE, lambda = meanLambda)
  # 
  # p1 = ggplot() +
  #   ggtitle("Effect of Interaction Rate on SSE of K-Means",
  #           subtitle = "M = 40, N = 20, numSubgroups = 6, univ = 1") +
  #   geom_point(data = elbowSSEDFLambda, aes(x = numCenters, y = SSE, group = runID, col = lambda),
  #              alpha = 0.4) +
  #   geom_line(data = elbowSSEDFLambda, aes(x = numCenters, y = SSE, group = runID, col = lambda),
  #             alpha = 0.4) +
  #   geom_point(data = meanDF1, aes(x = numCenters, y = SSE, col = lambda), size = 1.5) +
  #   geom_line(data = meanDF1, aes(x = numCenters, y = SSE, col = lambda), size = 1.5) +
  #   scale_x_discrete(limits = as.character(1:10)) +
  #   theme(axis.ticks.x = element_line(size = c(1, 1, 1, 1, 1, 3, 1, 1, 1, 1)))
  
  SSEM40 = sapply(1:10, function(n) mean(subset(elbowSSEDFM, numCenters == n & M == 40)$SSE))
  SSEM80 = sapply(1:10, function(n) mean(subset(elbowSSEDFM, numCenters == n & M == 80)$SSE))
  SSEM160 = sapply(1:10, function(n) mean(subset(elbowSSEDFM, numCenters == n & M == 160)$SSE))
  
  meanNumCenters = rep(1:10, 3)
  meanSSE = c(SSEM40, SSEM80, SSEM160)
  meanM = as.factor(c(rep(40, 10), rep(80, 10), rep(160, 10)))
  
  meanDF2 = data.frame(numCenters = meanNumCenters, SSE = meanSSE, M = meanM)
  
  p2 = ggplot() +
    ggtitle("Effect of Number of Samples on SSE of K-Means",
            subtitle = "N = 20, numSubgroups = 8, lambda = 100, univ = 1") +
    geom_point(data = elbowSSEDFM, aes(x = numCenters, y = SSE, group = runID, col = M),
               alpha = 0.4) +
    geom_line(data = elbowSSEDFM, aes(x = numCenters, y = SSE, group = runID, col = M),
              alpha = 0.4) +
    geom_point(data = meanDF2, aes(x = numCenters, y = SSE, col = M), size = 1.5) +
    geom_line(data = meanDF2, aes(x = numCenters, y = SSE, col = M), size = 1.5) +
    scale_x_discrete(limits = as.character(1:10)) +
    theme(axis.ticks.x = element_line(size = c(1, 1, 1, 1, 1, 1, 1, 3, 1, 1)))
  
  # SSENumSubgroups2 = sapply(1:10, function(n) mean(subset(elbowSSEDFNumSubgroups,
  #                                                         numCenters == n & numSubgroups == 2)$SSE))
  # SSENumSubgroups5 = sapply(1:10, function(n) mean(subset(elbowSSEDFNumSubgroups,
  #                                                         numCenters == n & numSubgroups == 5)$SSE))
  # SSENumSubgroups8 = sapply(1:10, function(n) mean(subset(elbowSSEDFNumSubgroups,
  #                                                         numCenters == n & numSubgroups == 8)$SSE))
  # 
  # meanNumCenters = rep(1:10, 3)
  # meanSSE = c(SSENumSubgroups2, SSENumSubgroups5, SSENumSubgroups8)
  # meanNumSubgroups = as.factor(c(rep(2, 10), rep(5, 10), rep(8, 10)))
  # 
  # meanDF3 = data.frame(numCenters = meanNumCenters, SSE = meanSSE, numSubgroups = meanNumSubgroups)
  # 
  # p3 = ggplot() +
  #   ggtitle("Effect of Number of Subgroups on SSE of K-Means",
  #           subtitle = "M = 40, N = 20, lambda = 0, univ = 1") +
  #   geom_point(data = elbowSSEDFNumSubgroups, aes(x = numCenters, y = SSE, group = runID, col = numSubgroups),
  #              alpha = 0.4) +
  #   geom_line(data = elbowSSEDFNumSubgroups, aes(x = numCenters, y = SSE, group = runID, col = numSubgroups),
  #             alpha = 0.4) +
  #   geom_point(data = meanDF3, aes(x = numCenters, y = SSE, col = numSubgroups), size = 1.5) +
  #   geom_line(data = meanDF3, aes(x = numCenters, y = SSE, col = numSubgroups), size = 1.5) +
  #   scale_x_discrete(limits = as.character(1:10)) +
  #   theme(axis.ticks.x = element_line(size = c(1, 3, 1, 1, 3, 1, 1, 3, 1, 1),
  #                                     color = c("black", "#F8766D", "black", "black", "#7CAE00", "black",
  #                                               "black", "#00BFC4", "black", "blacK")))
  
  # grid.arrange(p1, p2, p3, ncol = 3)
  plot(p2)
  
} #  end elbowSSEMultiplotTemp function
