# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

run_CLUST <- function(countDF, targets, colData, method) {
  ## Create full DESeqDataSet object
  dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                                design = ~condition)
  dds <- DESeq(dds)
  #method
  if (method == "rlog") {
    normdata <- rlog(dds, blind=TRUE)
  } else if (method == "vst") {
    normdata <- varianceStabilizingTransformation(dds, blind = T)   
  } else if (method == "raw") {
    normdata <- DESeqTransform(dds)
  }
  d <- cor(assay(normdata), method = "spearman")
  hc <- hclust(dist(1 - d))
  plot.phylo(as.phylo(hc), type = "p", edge.col = "blue", edge.width = 2, 
             show.node.label = TRUE, no.margin = TRUE)
  
}