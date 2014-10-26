AllocateTVaR <-
function(trialMatrix,returnPeriod){
  numTrials<-nrow(trialMatrix)
  numSamples<-ncol(trialMatrix)
  myOrder<-order(rowSums(trialMatrix),decreasing=TRUE)
  topTrials<-myOrder[1:(numTrials/returnPeriod)]
  return(colSums(trialMatrix[topTrials,])/(numTrials/returnPeriod))
  }
