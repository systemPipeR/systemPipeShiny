run_volcano <- function(DF, FDR, comparison) {
pval <- DF[, grep("_FDR$", colnames(DF)), drop=FALSE]
log2FC <- DF[, grep("_logFC$", colnames(DF)), drop=FALSE]

sample1 <- DF[, grep(comparison, colnames(DF)), drop=FALSE]
pval <- DF[, grep("_FDR$", colnames(DF)), drop=FALSE]

pval1 <- pval[,paste0(comparison,"_FDR")]
pvalT <- pval1 < FDR
sample1$threshold <- pvalT 
log2FoldChange <- sample1[,paste0(comparison,"_logFC")]

p <- ggplot(sample1) +
  geom_point(aes(x=log2FoldChange, y=-log10(pval1), colour=threshold)) +
  ggtitle(comparison) +
  xlab("log2 fold change") + 
  ylab("-log10 adjusted p-value") +
  #scale_y_continuous(limits = c(0,50)) +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.5), hjust = 0.5),
        axis.title = element_text(size = rel(1.25)))  
ggplotly(p)
}