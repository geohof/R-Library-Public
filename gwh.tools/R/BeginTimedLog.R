BeginTimedLog <-
function(...){
# This command is used together with EndTimedLog(). Its return value has to be
# passed to EndTimedLog().
  cat(format(Sys.time(), "%H-%M-%S"), ": ", ... ,"  ...", sep="")
  return(unclass(Sys.time()))
}
