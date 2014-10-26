FixName <-
function(myName){
  myName <- gsub(x=myName, pattern=" ", replacement=".")
  myName <- gsub(x=myName, pattern="&", replacement=".")
  myName <- gsub(x=myName, pattern="/", replacement=".")
  return(myName)
}
