MyWrite <-
function(...,myFile){
  write.csv(...=...,file=DataFile(myFile),row.names=FALSE)
}
