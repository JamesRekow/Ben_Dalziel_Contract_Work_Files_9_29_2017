


#####

p = ggplot(SSRatioDF, aes(x = numCenters, y = SSRatio, col = clarity)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(limits = as.character(1:10)) +
  ggtitle("Identifying Elbows in Plots of SSRatio VS. Number of Centers",
          subtitle = "M = 40, N = 20, univ = 1, lambda = 0 (bad), 20 (ok), 100 (good)")


dev.new()
plot(p)

p = ggplot(dredd, aes(x = numCenters, y = SSE, group = runID, col = clarity)) + geom_point(alpha = 0.4) + geom_line(alpha = 0.4) + scale_x_discrete(limits = as.character(1:10)) + geom_smooth(aes(x = numCenters, y = SSE, group = clarity, col = clarity), method = loess, se = FALSE, size = 1.5, span = 0)
