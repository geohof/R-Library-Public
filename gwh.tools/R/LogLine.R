LogLine <-
function(...){
## Function used for logging. Default is standard output. 
## Includes linefeed and carriage return to terminate the line.
  cat(format(Sys.time(), "%H-%M-%S"), ": ", ... , "\r\n", sep="")
}
