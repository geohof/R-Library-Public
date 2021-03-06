#' A suite of logging functions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @name Insurance Risk
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
#' @rdname InsuranceRisk
GetOEP<-function(rate, loss, 
                 prob=1 / c(1000, 500, 250, 200, 100, 50, 25, 20, 10), 
                 method="Poisson"){
  num.loss <- length(loss)
  if (missing(rate)){
    rate <- rep(1 / num.loss, num.loss)
  }
  o<-order(loss, decreasing=TRUE)
  rate<-rate[o]
  loss<-loss[o]
  cum.rate <- cumsum(rate)
  if(method=="Poisson"){
    cum.prob <- 1 - exp(-cum.rate)
  }else if(method=="Bernoulli"){
    cum.prob <- cum.rate
  }    
  data.frame(
    prob=prob,
    rp=1 / prob,
    loss=approx(x=cum.prob, y=loss, xout=prob, yleft=max(loss),yright=0)$y)
}
