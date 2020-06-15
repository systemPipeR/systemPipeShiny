## Another use of the transformed data is sample clustering. Here, we apply the dist function
## to the transformed count matrix to get sample-to-sample distances.

# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

run_MDS <- function(countDF, targets, colData, method) {
  ## Create full DESeqDataSet object
  dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                                design = ~condition)
  dds <- DESeq(dds)
  if (method == "rlog") {
    normdata <- rlog(dds, blind=TRUE)
  } else if (method == "vst") {
    normdata <- varianceStabilizingTransformation(dds, blind = T)   
  }
  ## transformation to a distance matrix
  d <- cor(assay(normdata), method = "spearman")
  distmat <- dist(1-d)
  ## perform MDS 
  mdsData <- data.frame(cmdscale(distmat)) 
  mds <- cbind(mdsData, as.data.frame(colData(dds)))
  Sample <- targets$Factor
  g <- ggplot(mds, aes(X1,X2,color=Sample)) + geom_point(size=3) + scale_y_reverse()  + ggtitle("Multidimensional Scaling (MDS)")
  ggplotly(g)
  
  
}

