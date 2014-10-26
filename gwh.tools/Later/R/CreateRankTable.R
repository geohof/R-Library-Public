CreateRankTable <-
function(n=NumTrials, CorMat){
  NumSamples <- nrow(CorMat)
  Samples <- mvrnorm(n=n, mu=rep(0, NumSamples), Sigma=CorMat)
  ### Create Rank table:
  rankTable <- matrix(data=as.integer(0), nrow=n, ncol=NumSamples)
  colnames(rankTable) <- colnames(CorMat)
  for(i in 1:NumSamples){
    rankTable[, i] <- rank(Samples[, i], ties.method="random")
  }
  return(rankTable)
}
