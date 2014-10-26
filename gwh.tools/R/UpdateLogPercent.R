UpdateLogPercent <-
function(percent){
	if(UpdateLogPercent.percent!=as.integer(percent)){
		UpdateLogPercent.percent <<- as.integer(percent)
    percentStr<-paste(UpdateLogPercent.percent,"%",sep="")
    cat(rep("\b",nchar(percentStr)),percentStr,sep="")
	}
}
