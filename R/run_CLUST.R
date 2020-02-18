# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

run_CLUST <- function(countDF, targets, colData) {
  ## Create full DESeqDataSet object
  dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                                design = ~condition)
  dds <- DESeq(dds)
  d <- cor(assay(rlog(dds)), method = "spearman")
  hc <- hclust(dist(1 - d))
  plot.phylo(as.phylo(hc), type = "p", edge.col = "blue", edge.width = 2, 
             show.node.label = TRUE, no.margin = TRUE)

}