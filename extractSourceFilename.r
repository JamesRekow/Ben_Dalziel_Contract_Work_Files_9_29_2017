#  James Rekow

extractSourceFilename = function(sourceLine){
  
  #  ARGS: sourceLine - character vector of the form "*optional whitespace* source(filename.r)"
  #
  #  RETURNS: sourceFilename - name of file being sourced. In the form "filename.r"
  
  #  create a character vector of the individual characters of the  input line
  splitSourceLine = unlist(strsplit(sourceLine, ""))
  
  #  identify indices of the parentheses surrounding the filename
  ix = which(splitSourceLine %in% c("(", ")"))
  
  #  extract filename from split input line
  splitSourceFilename = splitSourceLine[(ix[1] + 2):(ix[2] - 2)]
  
  #  concatenate characters into filename being sourced
  sourceFilename = paste(splitSourceFilename, collapse = "")
  
  return(sourceFilename)
  
} #  extractSourceFilename
