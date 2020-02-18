# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

run_PCA <- function(countDF, targets, colData, method) {
  
  ## Create full DESeqDataSet object
  dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                                design = ~condition)
  dds <- DESeq(dds)
  #method
  if (method == "rlog") {
    normdata <- rlog(dds, blind=TRUE)
  } else if (method == "vst") {
    normdata <- varianceStabilizingTransformation(dds, blind = T)   
  }
  pcaData <- plotPCA(normdata, intgroup="condition", returnData=TRUE)
  percentVar <- round(100 * attr(pcaData, "percentVar"))
  Sample <- targets$Factor
  g <- ggplot(pcaData, aes(PC1, PC2, color=Sample)) +
    geom_point(size=3) +
    xlab(paste0("PC1: ",percentVar[1],"% variance")) +
    ylab(paste0("PC2: ",percentVar[2],"% variance")) + 
    coord_fixed() + ggtitle("Principal Component Analysis (PCA)")
  ggplotly(g)  
  
}
