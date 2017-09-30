#  James Rekow

meanOverCurve = function(M, lambda){
  
  #  ARGS:
  #
  #  RETURNS: the expected value of the mean overlap of a DOC with M samples and exponentially distributed
  #           wait times between interactions with rate parameter lambda
  
  return(1 - exp(-100 * lambda / M))
  
} #  end meanOverCurve function
