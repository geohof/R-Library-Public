GetObjectSizes <-
function(){
  tmp.list<-ls(envir=.GlobalEnv)
  tmp.size <- unlist(lapply(X=tmp.list, FUN=function(x)object.size(get(x))))/(2^30)
  names(tmp.size) <- tmp.list
  tmp.size <- tmp.size[order(tmp.size, decreasing=TRUE)]
return(tmp.size)
}
