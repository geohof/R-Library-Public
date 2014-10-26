s3.saveRDS <-
function(s3.path, object)
  s3.apply.put(s3.path, object, saveRDS, options = "-m binary/octet-stream")
