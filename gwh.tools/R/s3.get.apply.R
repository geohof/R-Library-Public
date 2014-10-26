s3.get.apply <-
function(s3.path, FUN){
  tmp.file <- tempfile()
  s3.get(s3.path = s3.path, out.file = tmp.file)
  ans <- FUN(tmp.file)
  unlink(tmp.file)
  return(ans)
}
