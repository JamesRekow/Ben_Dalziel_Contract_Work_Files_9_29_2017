#  James Rekow

computeDirectFileDependencies = function(inputFile){
  
  #  ARGS: inputFile - an R file
  #
  #  RETURNS: directFileDependencies - a character vector containing the names of all of the files on which
  #                                    input file directly depends (e.g. which are sourced in inputFile)
  
  source("selectLines.r")
  source("extractSourceFilename.r")
  
  #  select the lines in input file in which a file is sourced
  fileSourceLines = selectLines(inputFile = inputFile, lineStart = "source(")
  
  #  extract the name of each file sourced
  directFileDependenciesList = lapply(fileSourceLines, extractSourceFilename)
  
  #  unlist the sourced filenames into a character vector
  directFileDependencies = unlist(directFileDependenciesList)
  
  return(directFileDependencies)
  
} #  end computeDirectFileDependencies function
