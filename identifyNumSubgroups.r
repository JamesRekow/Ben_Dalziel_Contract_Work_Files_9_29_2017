#  James Rekow

identifyNumSubgroups = function(infoMat){
  
  #  ARGS: infoMat - matrix of info weights corresponding to pairs of samples from an abundance list
  #
  #  RETURNS: numClust - predicted number of clusters in infoMat
  
  library(cluster)
  
  #  B = 100 works pretty well, but is slow. Left as a note in case a good value is forgotten
  #  compute statistics for kmeans results with various numbers of clusters
  cmr = clusGap(x = infoMat, FUNcluster = kmeans, K.max = 9, d.power = 2, B = 10)
  
  #  identify the optimal number of clusters, standard SE.factor is 1.0 (from Tibshirani et al.)
  numClust = maxSE(f = cmr$Tab[ , "gap"], SE.f = cmr$Tab[ , "SE.sim"], SE.factor = 1.0,
                   method = "Tibs2001SEmax")

  return(numClust)
  
#   backupTest = function(infoMat){
# 
#     #  B = 100 works pretty well, but is slow. Left as a note in case a good value is forgotten
#     #  compute statistics for kmeans results with various numbers of clusters
#     cmr = clusGap(x = infoMat, FUNcluster = kmeans, K.max = 9, d.power = 2, B = 10)
# 
#     #  identify the optimal number of clusters
#     numClust = maxSE(f = cmr$Tab[ , "gap"], SE.f = cmr$Tab[ , "SE.sim"], SE.factor = 1,
#                      method = "Tibs2001SEmax")
# 
#     return(numClust)
# 
#   } #  end backupTest function
# 
#   defaultTest = function(infoMat){
# 
#     #  add 10 to infoMat to prevent high condition number. For some reason NbClust doesn't like these
#     #  matrices even if ther det is very far from zero (0.1). Adding random noise should make nonzero det,
#     #  (it does) but that didn't help. Don't know what the problem was
#     nbcr = NbClust(data = infoMat + 10, method = "kmeans")
# 
#     #  select optimal predicted partition
#     partition = nbcr$Best.partition
# 
#     #  identify number of clusters in optimal partition
#     numClust = length(unique(partition))
# 
#     return(numClust)
# 
#   } #  end defaultTest function
# 
#   # numClustTest = function(infoMat){
#   #   tryCatch(defaultTest(infoMat), error = function(infoMat) backupTest(infoMat))
#   # } #  end numClustTest function
# # 
# #   numClust = numClustTest(infoMat)
#   numClust = defaultTest(infoMat)
# 
#   return(numClust)
  
} #  end identifyNumSubgroups function
