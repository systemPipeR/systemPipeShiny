#################### Plot MDS from Count matrix ######################
# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

#' Plots Multidimensional Scaling based on pairwise distances between samples
#' using a count dataframe and a targets file.
#'
#' @param countDF Matrix object of Count data.
#' @param targets Object containing targets.
#' @param colData Dataframe containing metadata about each sample.
#' @param method Normalization method for plot. Options include "rlog", "vst".
#' @examples
#' ## Create targets file object
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' colData <- data.frame(row.names = targets$SampleName,
#'             condition = targets$Factor)
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#' ## Create MDS plot
#' run_MDS(countDF = countDF, targets = targets, colData = colData, method = "rlog")
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

