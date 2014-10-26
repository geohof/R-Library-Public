s3.apply.put <-
function(s3.path, object, FUN, options){
  tmp.file <- tempfile()
  FUN(object, tmp.file)
  s3.put(file = tmp.file, s3.path = s3.path, options)
  unlink(tmp.file)
}
