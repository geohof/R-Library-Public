CombineLayerAllocations <-
function(allocationFilenameList,
                                  allocationColNameList,
                                  outputFilename){
  numAllocationFiles<-length(allocationFilenameList)
  MyLogBegin("Loading data")
  
  dataTableList <- list()
  layerIds<-c()
  totalNumCols <- 0
  for(i in 1:numAllocationFiles){ 
    dataTableList[[i]]<-read.csv(allocationFilenameList[i],header=TRUE)
    layerIds <- unique(c(layerIds,dataTableList[[i]][,1]))
    totalNumCols<-totalNumCols+ncol(dataTableList[[i]])-1 #without LayerId
  }
  layerIds <- layerIds[order(layerIds)]
  numLayers<-length(layerIds)

  allAllocations<-matrix(0,nrow=numLayers,ncol=totalNumCols)
  colnames(allAllocations)<-1:totalNumCols
  totalNumCols <- 0
  for(i in 1:numAllocationFiles){ 
    numCols<-ncol(dataTableList[[i]])-1 #without LayerId
    allAllocations[match(dataTableList[[i]][,1],layerIds),
                   (totalNumCols+1):(totalNumCols+numCols)]<-
                     data.matrix(dataTableList[[i]][,2:(numCols+1)])
    colnames(allAllocations)[(totalNumCols+1):(totalNumCols+numCols)]<-
      paste(allocationColNameList[i],colnames(dataTableList[[i]])[2:(numCols+1)])
    totalNumCols<-totalNumCols+numCols
  }
  MyLogEnd()
  MyLogBegin("Writing data")
  
  
  output<-data.frame(layerIds,allAllocations)
  write.csv(output,file=outputFilename,row.names=FALSE)
  MyLogEnd()
}
