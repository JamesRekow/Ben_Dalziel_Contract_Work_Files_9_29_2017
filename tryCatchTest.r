#  James Rekow

f = function(x){
  return(sum(x))
} #  end f function

g = function(x){
  return(0)
} #  end g function

h = function(x){
  tryCatch(f(x), error = function(x) g(x))
} #  end h function
