#' A suite of logging functions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @name Logging Functions
#' @aliases EndTimedLog
#' @aliases LogLine
#' @param log.obj Describe \code{log.obj} here
#' @return 
#' Function used for logging. Default is standard output. 
#' Includes linefeed and carriage return to terminate the line.
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2

#' @export
#' @examples
#' ### Calculating squares
#' log.obj <- BeginTimedLog("Calculating squares.")
#' n <- 1E3
#' for(i in 1:n){
#'   for(ii in 1:10000){
#'     i^2
#'   }
#'   log.obj <- UpdateLogPercent(log.obj, percent = 100 * i / n)
#' }
#' EndTimedLog(log.obj)


#' @export 
#' @rdname Logging
BeginTimedLog <-
function(...){
# This command is used together with EndTimedLog(). Its return value has to be
# passed to EndTimedLog().
  cat(format(Sys.time(), "%H-%M-%S"), ": ", ... ," ....", sep="")
  return(list(start.seconds=unclass(Sys.time())))
}

#' @export 
#' @rdname Logging
LogLine <- function(...){
  cat(format(Sys.time(), "%H-%M-%S"), ": ", ... , "\r\n", sep="")
}

#' @export 
#' @rdname Logging
EndTimedLog <-
  function(log.obj, ...){
    ### At the end of a block of code. Used together with MyLogBegin().
    cat(" done. (", 
        as.integer(unclass(Sys.time()) - log.obj$start.seconds), 
        " secs)\r\n", sep="")
  }

#' @export 
#' @rdname Logging
UpdateLogPercent <- function(log.obj, percent){
  if(!("percent.complete" %in% names(log.obj))){
    log.obj$percent.complete <- -1
  }
  if(floor(log.obj$percent.complete) < floor(percent)){
    cat(rep("\b", 4L),sprintf("%3.0f",floor(percent)), "%", sep="")
  }
  log.obj$percent.complete <- percent
  return(log.obj)
}


