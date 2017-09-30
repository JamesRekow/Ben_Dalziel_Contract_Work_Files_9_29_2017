#  James Rekow

codeLines = function(file, SLOC = FALSE){
  
  #  ARGS: file - a file in the current working directory
  #        SLOC - if TRUE, omits comments (only counts Source Lines Of Code)
  #
  #  RETURNS: numLines - number of lines of code (or source lines of code, if SLOC == TRUE)
  
  #  split file name into component symbols
  splitFileName = strsplit(x = file, split = "")[[1]]
  
  #  find length of file name
  nameLength = length(splitFileName)
  
  #  if file name has length less than three then it isn't a file of the form x.r, and thus has 0 LOC
  if(nameLength < 3){
    return(0)
  } #  end if
  
  #  check if the input has an R file extension
  isRFile = ".R" == paste(splitFileName[nameLength - 1], toupper(splitFileName[nameLength]), sep = "")
  
  #  if the file isn't an R file, it has 0 LOC
  if(!isRFile){
    return(0)
  } #  end if
  
  #  read in lines from file
  fileLines = readLines(file)
  
  #  compute the number of lines in the file
  numLines = length(fileLines)
  
  if(SLOC){
    
    isComment = function(fileLine){
      
      #  ARGS: fileLine - single line from input file
      #
      #  RETURNS: isNotSLOC - TRUE if fileLine is empty, all spaces, or a comment
      
      #  remove spaces from line
      trimmedLine = gsub(" ", "", fileLine, fixed = TRUE)
      
      #  check if fileLine is empty or only spaces
      isEmptySpace = trimmedLine == ""
      
      #  check if fileLine is a comment
      isAComment = substr(trimmedLine, 1, 1) == "#"
      
      #  TRUE if fileLine is empty, spaces only, or a comment
      isNotSLOC = isEmptySpace | isAComment
      
      return(isNotSLOC)
      
    } #  end isComment function
    
    #  determine which lines are comments
    isCommentList = lapply(fileLines, isComment)
    
    #  count the number of non-comment lines in the file
    numLines = numLines - Reduce("+", isCommentList)
    
  } #  end if SLOC
  
  return(numLines)
  
} #  end codeLines function
