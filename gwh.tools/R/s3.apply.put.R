#' Apply a function and put to s3 bucket
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param s3.path %% ~~Describe \code{s3.path} here~~
#' @param object %% ~~Describe \code{object} here~~
#' @param FUN %% ~~Describe \code{FUN} here~~
#' @param options %% ~~Describe \code{options} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
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
#' function (s3.path, object, FUN, options) 
#' {
#'     tmp.file <- tempfile()
#'     FUN(object, tmp.file)
#'     s3.put(file = tmp.file, s3.path = s3.path, options)
#'     unlink(tmp.file)
#'   }
#' 
#' @export s3.apply.put
s3.apply.put <-
function(s3.path, object, FUN, options){
  tmp.file <- tempfile()
  FUN(object, tmp.file)
  s3.put(file = tmp.file, s3.path = s3.path, options)
  unlink(tmp.file)
}
