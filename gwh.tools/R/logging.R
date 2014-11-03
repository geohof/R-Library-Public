#' A suite of logging functions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @name Logging Functions
#' @aliases EndTimedLog
#' @aliases LogLine
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return 
#' Function used for logging. Default is standard output. 
#' Includes linefeed and carriage return to terminate the line.
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (...) 
#' {
#'     cat(format(Sys.time(), "%H-%M-%S"), ": ", ..., "  ...", sep = "")
#'     return(unclass(Sys.time()))
#'   }
#' 
#' 


#' @export 
#' @rdname Logging
BeginTimedLog <-
function(...){
# This command is used together with EndTimedLog(). Its return value has to be
# passed to EndTimedLog().
  cat(format(Sys.time(), "%H-%M-%S"), ": ", ... ,"  ...", sep="")
  return(unclass(Sys.time()))
}

#' @export 
#' @rdname Logging
LogLine <- function(...){
  cat(format(Sys.time(), "%H-%M-%S"), ": ", ... , "\r\n", sep="")
}

#' @export 
#' @rdname Logging
EndTimedLog <-
  function(log.start.seconds, ...){
    ### At the end of a block of code. Used together with MyLogBegin().
    cat(" done. (", 
        as.integer(unclass(Sys.time()) - log.start.seconds), 
        " secs)\r\n", sep="")
  }

#' @export 
#' @rdname Logging
UpdateLogPercent <-
  function(percent){
    if(UpdateLogPercent.percent!=as.integer(percent)){
      UpdateLogPercent.percent <<- as.integer(percent)
      percentStr<-paste(UpdateLogPercent.percent,"%",sep="")
      cat(rep("\b",nchar(percentStr)),percentStr,sep="")
    }
  }

