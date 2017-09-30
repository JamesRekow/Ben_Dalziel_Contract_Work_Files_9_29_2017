#  James Rekow

projectLines = function(SLOC = FALSE){
  
  #  ARGS: SLOC - if SLOC == TRUE return source lines of code. Otherwise total lines of code returned
  #
  #  RETURNS: number of LOC or SLOC in current working directory
  
  source("codeLines.r")
  
  #  function to count the number of lines of code (LOC or SLOC)
  lineCounter = function(file) codeLines(file = file, SLOC = SLOC)
  
  #  list all files in the current working directory
  filesInDirectory = dir()
  
  #  compute number of code lines in each file in the current working directory
  lineCountList = lapply(filesInDirectory, lineCounter)
  
  #  compute the total number of code lines
  totalLineCount = Reduce("+", lineCountList)
  
  return(totalLineCount)
  
} #  end projectLines function
