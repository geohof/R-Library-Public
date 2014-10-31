#' SaveRDS to s3
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param s3.path %% ~~Describe \code{s3.path} here~~
#' @param object %% ~~Describe \code{object} here~~
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
#' function (s3.path, object) 
#' s3.apply.put(s3.path, object, saveRDS, options = "-m binary/octet-stream")
#' 
#' @export s3.saveRDS
s3.saveRDS <-
function(s3.path, object)
  s3.apply.put(s3.path, object, saveRDS, options = "-m binary/octet-stream")
