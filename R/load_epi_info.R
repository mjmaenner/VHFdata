# Load Epi Info files
# Unfortunately, Epi Info outputs csv's with an unusual encoding,
# and weirdly states the delimiter on the first line. This creates
# an extra step for reading the CSVs directly into R.

#load case data
load_casemanage <- function(pathToCaseFile = "c:/vhfdata/casemanagement.csv") {
  #x = scan(pathToCaseFile, what="character", skip=1, skipNul=TRUE, encoding="UTF-16")
  x = readLines(pathToCaseFile, skipNul=TRUE,encoding="UTF-8", n=-1L )
  x = x[-1] # epi info puts a statement indicating the delimiter on the first line.
  x = read.csv(textConnection(x), header = TRUE, stringsAsFactors = FALSE)
  return(assign("VHFcase",x, envir=.GlobalEnv))
}

load_contacts <- function(pathToContactsFile = "c:/vhfdata/contacts.csv") {
  x = readLines(pathToContactsFile, skipNul=TRUE,encoding="UTF-16" )
  x = x[-1] # epi info puts a statement indicating the delimiter on the first line.
  x = read.csv(textConnection(x), header = TRUE, stringsAsFactors = FALSE)
  return(assign("VHFcontacts",x, envir=.GlobalEnv))
}