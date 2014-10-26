TruncateHPCNames <-
function(myNames){
  return(sub("\\..*", "", myNames))
}
