# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)
# library("RColorBrewer")
# library(pheatmap)

run_HEAT <- function(countDF, targets, colData) {
## Create full DESeqDataSet object
dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                              design = ~condition)
dds <- DESeq(dds)
d <- cor(assay(rlog(dds)), method = "spearman")
pheatmap(d , color=brewer.pal(9,"RdPu"))
}
