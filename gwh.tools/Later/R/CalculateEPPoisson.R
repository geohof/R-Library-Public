CalculateEPPoisson <-
function(Rate, Loss, Probability){
  NumEvents=NROW(Loss)
  o<-order(Loss, decreasing=TRUE)
  Rate<-Rate[o]
  Loss<-Loss[o]
  if(Probability<Rate[1])
  {PML <- Loss[1]}
  else
  {i <- 2
   SumRate <- Rate[1]
   CumProb <- 1-exp(-SumRate)
   repeat
   {NewSumRate <- SumRate+Rate[i]
    NewCumProb <- 1-exp(-NewSumRate)
    if(Probability<NewCumProb)
    {PML <- Loss[i-1]+(Loss[i]-Loss[i-1])*(Probability-CumProb)/(NewCumProb-CumProb)
     break
    }
    SumRate <- NewSumRate
    CumProb <- NewCumProb
    i <- i+1
    if(i>NumEvents)
    {PML <- 0
     break
    } # if
   } # repeat
  } # else
  return(PML)
}
