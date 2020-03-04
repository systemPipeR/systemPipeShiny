# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)
# library("RColorBrewer")
# library(plotly)

run_HEAT <- function(countDF, targets, colData, method) {
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
  d <- cor(assay(normdata), method = "spearman")
  plot_ly(x = colnames(d), y = rownames(d), z = d, color="Greys", type = "heatmap")
}
