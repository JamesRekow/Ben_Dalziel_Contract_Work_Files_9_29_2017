#  James Rekow

selectLines = function(inputFile, lineStart = NULL){
  
  #  ARGS: inputFile - R file
  #        lineStart - character specifying which lines from inputFile should be returned
  #
  #  RETURNS: selectedLines - If lineStart is NULL, all lines from the file are returned. If a value is
  #                           provided then only lines that begin (not counting whitespace characters) with
  #                           lineStart are returned. Does not work with lines that begin with whitespace
  #                           characters
  
  #  split characters of lineStart
  splitLineStart = unlist(strsplit(lineStart, ""))
  
  #  number of characters in lineStart
  numChars = length(splitLineStart)
  
  #  define function to select lines that begin with lineStart
  lineSelector = function(fileLine){
    
    #  value to be returned
    returnVal = NULL
    
    #  remove spaces from line
    trimmedLine = gsub(" ", "", fileLine, fixed = TRUE)
    
    if(trimmedLine == ""){
      return(NULL)
    } #  return NULL if the line was all whitespace
    
    #  split characters of the trimmed line
    splitLine = unlist(strsplit(trimmedLine, ""))
    
    #  check if the beginning of splitLine matches lineStart
    keepLine = all(splitLine[1:numChars] == splitLineStart)
    
    #  if line begins with lineStart, return line content with whitespace removed
    if(keepLine){
      returnVal = trimmedLine
    } #  end if
    
    return(returnVal)
    
  } #  end lineSelector function
  
  #  read in lines from file
  fileLines = readLines(inputFile)
  
  #  return all lines from file if lineStart is not specified
  if(is.null(lineStart)){
    selectedLines = fileLines
  } #  end if
  
  #  if lineStart is specified, only return lines that begin with the given value
  if(!is.null(lineStart)){
    selectedLines = unlist(lapply(as.list(fileLines), lineSelector))
  } #  end if
  
  return(selectedLines)
  
} #  end selectLines function
