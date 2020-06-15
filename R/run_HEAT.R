#################### Plot Clustering Heat Map from Count matrix ######################

#' Plots a hierarchical clustering heat map using a count dataframe and a
#' targets file.
#' @param countDF Matrix of Count data.
#' @param targets targets data.frame
#' @param colData Dataframe containing metadata about each sample.
#' @param method Normalization method for plot. Options include "raw", "rlog",
#' "vst".
#' @examples
#' ## Create targets file object
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' colData <- data.frame(row.names = targets$SampleName,
#'             condition = targets$Factor)
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#' ## Create Heat map
#' run_HEAT(countDF = countDF, targets = targets, colData = colData, method = "raw")
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
  } else if (method == "raw") {
    normdata <- DESeqTransform(dds)
  }
  d <- cor(assay(normdata), method = "spearman")
  plot_ly(x = colnames(d), y = rownames(d), z = d, color="Greys", type = "heatmap")
}
