s3.data.path1 <- "s3://middleware-research/useq/"
s3.input.folder1 <- "EGH_FINAL_WLIQ_wFFE12"
s3.data.path2 <- "s3://middleware-research/useq/TEMP/"
s3.input.folder2 <- "3-EGHwVALIDwLIQwFFE-NODUPS"
working.directory <- "/mnt/d1/"
setwd(working.directory)

s3.input.path1 <- paste(s3.data.path1, s3.input.folder1, "_Meta/", sep="")
s3.output.path1 <- paste(s3.data.path1, s3.input.folder1, "_Meta/", sep="")
s3.input.path2 <- paste(s3.data.path2, s3.input.folder2, "_Meta/", sep="")
s3.output.path2 <- paste(s3.data.path2, s3.input.folder2, "_Meta/", sep="")

l1 <- s3.readRDS(s3.path = paste(s3.input.path1, "OutputFinal00000.RDS", sep=""))

l2 <- s3.readRDS(s3.path = paste(s3.input.path2, "OutputFinal00000.RDS", sep=""))

# names(l1) <- names(l2)

names(l1)
names(l2)

if(ncol(l1$grid.matrix)==ncol(l2$grid.matrix)){
	LogLine("Number of columns matches.")
}else{
	stop("Number of columns doesn't match.")
}
if(all(colnames(l1$grid.matrix)==colnames(l2$grid.matrix))){
	LogLine("Column names match.")
}else{
	stop("Column names don't match.")
}

if(nrow(l1$grid.matrix)==nrow(l2$grid.matrix)){
	LogLine("Number of rows matches.")
}else{
	stop("Number of rows doesn't match.")
}
if(all(rownames(l1$grid.matrix)==rownames(l2$grid.matrix))){
	LogLine("Row names match.")
}else{
	stop("Row names don't match.")
}

diff.mat.abs <- abs(l1$grid.matrix - l2$grid.matrix)

row.sums <- rowSums(diff.mat.abs)

LogLine("Number of rows: ", nrow(l1$grid.matrix))
LogLine("Number of rows that match: ", sum(row.sums < 1E-10))