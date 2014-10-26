ExpectColNames <-
function(x,colNames,myMessage="Error: Column names not as expected"){
  if(sum(colnames(x) != colNames)>0){
    stop(myMessage)
  }
}
