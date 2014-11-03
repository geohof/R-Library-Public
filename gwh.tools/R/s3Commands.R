#' A suite of logging functions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @name S3 File Functions
#' @param \dots %% ~~Describe \code{\dots} here~~
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
#' @rdname s3Commands
s3.apply.put <-
  function(s3.path, object, FUN, options){
    tmp.file <- tempfile()
    FUN(object, tmp.file)
    s3.put(file = tmp.file, s3.path = s3.path, options)
    unlink(tmp.file)
  }


#' @export 
#' @rdname s3Commands
s3.fread <-
  function(s3.path)s3.get.apply(s3.path, fread)

#' @export 
#' @rdname s3Commands
s3.get.apply <-
  function(s3.path, FUN){
    tmp.file <- tempfile()
    s3.get(s3.path = s3.path, out.file = tmp.file)
    ans <- FUN(tmp.file)
    unlink(tmp.file)
    return(ans)
  }

#' @export 
#' @rdname s3Commands
s3.get <-
  function (s3.path, out.file="", options="") 
  {
    s3.cmd <- paste("s3cmd get", s3.path, out.file, options)
    if(out.file==""){
      out.file <- strsplit(x = file, split = "/")[[1]]
      out.file <- out.file[length(out.file)]
    }
    res <- system(s3.cmd, intern = TRUE)
    return(out.file)
  }

#' @export 
#' @rdname s3Commands
s3.put <-
  function (file, s3.path, options="") 
  {
    s3.cmd <- paste("s3cmd put", file, s3.path, options)
    res <- system(s3.cmd, intern = TRUE)
  }

#' @export 
#' @rdname s3Commands
s3.readRDS <-
  function(s3.path)s3.get.apply(s3.path, readRDS)



#' @export 
#' @rdname s3Commands
s3.saveRDS <-
  function(s3.path, object)
    s3.apply.put(s3.path, object, saveRDS, options = "-m binary/octet-stream")

#' @export 
#' @rdname s3Commands
s3.source <-
  function(s3.path){
    s3.get.apply(s3.path, source)
    return(invisible())
  }



