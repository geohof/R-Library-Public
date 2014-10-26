ReRankSamples <-
function(Samples, RankTable, AlreadyInOrder=FALSE){
  NumSamples <- NCOL(Samples)
  SampleSize <- nrow(Samples)
  ## Order the Samples (if necessary)
  if(AlreadyInOrder){
    OrderedSamples <- Samples
  }else{
    OrderedSamples <- matrix(data=0, nrow=NumTrials, ncol=NumSamples) 
    for(i in 1:NumSamples){
      OrderedSamples[, i] <- Samples[order(Samples[, i]), i]   
    }
  }
  ## Rerank the samples
  CorrelatedSamples <- matrix(data=0, nrow=SampleSize, ncol=NumSamples)
  for(i in 1:NumSamples){
    CorrelatedSamples[, i] <- OrderedSamples[RankTable[, i], i]  
  }
  colnames(CorrelatedSamples) <- colnames(Samples)
  return(CorrelatedSamples)
}
