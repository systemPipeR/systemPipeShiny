#################### Plots a Volcano Plot ######################

#' Plots  a Volcano Plot from an \code{edgeR} or \code{DESeq2} data frame.
#' @param DF \code{edgeR} or \code{DESeq2} data frame.
#' @param FDR False Discovery Rate cut off for filtering.
#' @param Fold Log Fold Change cut off for filtering.
#' @param Comparison \code{String} names of samples or conditions that are
#' compared. Example: "M1-A1"
#' @examples
#' ## Create DEG dataframe
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- readComp(file=targetspath, format="matrix", delim="-")
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#' edgeDF <- run_edgeR(countDF=countDF, targets=targets, cmp=cmp[[1]], independent=FALSE, mdsplot="")
#'
#' ## Create Volcano plot
#' run_volcano(DF = edgeDF,FDR = 10, Fold = 2, comparison = "M1-A1")
run_volcano <- function(DF, FDR, Fold, comparison) {
  pval <- DF[, grep("_FDR$", colnames(DF)), drop=FALSE]
  log2FC <- DF[, grep("_logFC$", colnames(DF)), drop=FALSE]
  sample1 <- DF[, grep(comparison, colnames(DF)), drop=FALSE]
  pval1 <- pval[,paste0(comparison,"_FDR")] ##DF of FDR's(p.adj) for sample comparison
  log2FoldChange <- sample1[,paste0(comparison,"_logFC")]
  Significance <- ifelse(pval1 < FDR & abs(as.numeric(log2FoldChange)) > Fold, "Significant", "Insignificant")
  sample1$Significance <- Significance ##adding column of significant/insignificant
  
  p <- ggplot(sample1) +
    geom_point(aes(x=log2FoldChange, y=-log10(as.numeric(pval1)), colour=Significance)) +
    geom_vline(xintercept = c(-Fold,Fold), linetype=2) + 
    geom_hline(yintercept = -log10(FDR), linetype=2)+
    ggtitle(comparison) +
    xlab("log2 fold change") + 
    ylab("-log10 adjusted p-value") +
    #scale_y_continuous(limits = c(0,50)) +
    theme(legend.position = "none",
          plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.title = element_text(size = rel(1.25)))
  ggplotly(p)
  
}
