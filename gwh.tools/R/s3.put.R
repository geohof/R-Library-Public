s3.put <-
function (file, s3.path, options="") 
{
  s3.cmd <- paste("s3cmd put", file, s3.path, options)
  res <- system(s3.cmd, intern = TRUE)
}
