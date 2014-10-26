EndTimedLog <-
function(log.start.seconds, ...){
### At the end of a block of code. Used together with MyLogBegin().
  cat(" done. (", 
      as.integer(unclass(Sys.time()) - log.start.seconds), 
      " secs)\r\n", sep="")
}
