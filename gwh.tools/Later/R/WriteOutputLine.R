WriteOutputLine <-
function(...){
  writeLines(paste(...,sep=","),con=outputFile)
}
