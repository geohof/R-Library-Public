WriteTVaRELLayerAllocation <-
function(inputFileName,
                                     outputFileName,
                                     tVaRReturnPeriodVector){
  ### This function loads a By-Layer By-Trial Loss file.
  ### It writes a file that has TVar allocations to 
  ### layers and the EL for each layer.
  ## The input file is expected to have the following headers in any order:
  layerIdName<-"LayerId"
  trialName<-"TrialId"
  lossName<-"Loss"

  MyLogLine("Working directory: ", getwd())
  numTVaRs<-length(tVaRReturnPeriodVector)
  MyLogBegin("Loading losses from file ",inputFileName)
  dataTable<-read.csv(inputFileName,header=TRUE)
  ## If the column headers are not as expected (See above) 
  ## we will get an error below
  LayerIds<-unique(dataTable[,layerIdName])
  numLayers<-length(LayerIds)
  numTrials<-max(dataTable[,trialName])
  MyLogEnd()
  MyLogLine("Detected ", numTrials, 
            " Trials and ", numLayers,
            " Layers.")
  MyLogLine("Loaded mean loss: ", 
            sum(dataTable[,"Loss"])/numTrials)

  MyLogBegin("Reorganizing sparse loss data into dense matrix")
  loss<-matrix(data=0,nrow=numTrials,ncol=numLayers)
  colnames(loss)<-LayerIds

  for(i in 1:numLayers){
    whichLines<-which(dataTable[,layerIdName]==LayerIds[i])
    loss[dataTable[whichLines,trialName],i]<-dataTable[whichLines,"Loss"]
  }

  MyLogEnd()

  MyLogLine("Calculating EL and TVaRs:")
  ## We gather all results in a matrix names output
  ## Column 1 will contain layer Ids. 
  ## Column 2 will contain EL.
  ## The remaining columns will countain TVaRs.

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


  MyLogBegin("Writing results to file ", outputFileName)
  write.csv(output,file=outputFileName,row.names=FALSE)
  MyLogEnd()
}
