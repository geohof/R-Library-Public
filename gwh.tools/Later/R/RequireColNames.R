RequireColNames <-
function(x,colNames,myMessage="Error: Missing expected column names."){
  if(sum(!(colNames %in% colnames(x)))>0){
    stop(myMessage)
  }
}
