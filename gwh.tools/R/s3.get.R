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
