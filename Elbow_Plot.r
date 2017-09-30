#  James Rekow

library(ggplot2)
source("elbowSSRatioTest.r")

SSRatio1 = elbowSSTest(M = 40, N = 20, numSubgroups = 7, lambda = 100, univ = 1)
SSRatio2 = elbowSSTest(M = 40, N = 20, numSubgroups = 7, lambda = 30, univ = 1)
SSRatio3 = elbowSSTest(M = 40, N = 20, numSubgroups = 7, lambda = 0, univ = 1)

SSRatio = c(SSRatio1, SSRatio2, SSRatio3)

len = length(SSRatio1)
clarity1 = rep("good", len)
clarity2 = rep("ok", len)
clarity3 = rep("bad", len)

clarity = as.factor(c(clarity1, clarity2, clarity3))

numCenters = rep(1:10, 3)

SSRatioDF = data.frame(numCenters = numCenters, SSRatio = SSRatio, clarity = clarity)

p = ggplot(SSRatioDF, aes(x = numCenters, y = SSRatio, col = clarity)) +
      geom_point() +
      geom_line() +
      scale_x_discrete(limits = as.character(1:10)) +
      ggtitle("Identifying Elbows in Plots of SSRatio VS. Number of Centers",
              subtitle = "M = 40, N = 20, univ = 1, lambda = 0 (bad), 20 (ok), 100 (good)")


dev.new()
plot(p)
