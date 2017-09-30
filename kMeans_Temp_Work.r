#  James Rekow

#  I just returned the fractured abdList in the beginning of the program
abdList = marginalDissDensity(M = 80, thresholdMult = 10 ^ (-3))

#  convert abd list to a matrix, each row is an abundance vector, so the columns are the abundances of a
#  given species
mat = Reduce(rbind, abdList)

h(101)

#  identify clusters using kmeans algorithm, standardizing cluster labels so that sample 1 is always in
#  cluster 1
f = function(x = NULL){
  kmr = kmeans(x = mat, centers = 2, iter.max = 1000, nstart = 10)$cluster
  if(kmr[1] == 2){
    a = kmr == 2
    kmr[a] = 1
    kmr[!a] = 2
  }
  return(kmr)
}

g = function(n){
  kmrMat = replicate(n, f())
  kmrAgg = apply(kmrMat, 1, numMode)
  return(which(kmrAgg == 1))
}

h = function(n){
  gr = g(n)
  a = sum((1:40) %in% g(n))
  b = sum((41:80) %in% g(n))
  return(c(a, b))
}

