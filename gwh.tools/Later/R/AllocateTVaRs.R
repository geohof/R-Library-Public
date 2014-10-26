AllocateTVaRs <-
function(trialMatrix,returnPeriod){
  numTVaRs <- length(Returnperiod)
  output<-matrix(0,numLayers,numTVaRs+2)
  colnames(output)<-(1:(numTVaRs+2))
  output[,1]<-LayerIds
  colnames(output)[1]<-"LayerId"
  output[,2]<-data.matrix(colSums(loss)/numTrials)
  colnames(output)[2]<-"EL"

  for(i in (1:numTVaRs)){
    tVaRReturnPeriod<-tVaRReturnPeriodVector[i]
    allocatedTVaR<-data.matrix(AllocateTVaR(trialMatrix=loss,
                                          returnPeriod=tVaRReturnPeriod))
    output[,i+2]<-allocatedTVaR
    colnames(output)[i+2]<-paste("TVaR",tVaRReturnPeriod,sep="")
    MyLogLine("Total TVaR",tVaRReturnPeriod,": ", sum(allocatedTVaR))
  }

  }
