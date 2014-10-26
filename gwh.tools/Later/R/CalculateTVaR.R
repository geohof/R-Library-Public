CalculateTVaR <-
function(trialVector,returnPeriod){
  numTrials<-length(trialVector)
  myOrder<-order(trialVector,decreasing=TRUE)
  topTrials<-myOrder[1:(numTrials/returnPeriod)]
  return(mean(trialVector[topTrials]))
  }
