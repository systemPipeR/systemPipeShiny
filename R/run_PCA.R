#################### Plot PCA from Count matrix ######################

#' Plots Principal Component Analysis from a count dataframe and a targets file.
#' @param countDF Matrix of Count data.
#' @param targets targets data.frame
#' @param colData Dataframe containing metadata about each sample.
#' @param method Normalization method for plot. Options for PCA include "rlog", "raw", "vst".
#' @examples
#' ## Create targets file object
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' colData <- data.frame(row.names = targets$SampleName,
#'             condition = targets$Factor)
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#' ## Create PCA plot
#' run_PCA(countDF = countDF, targets = targets, colData = colData, method = "raw")
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
    } else if (method == "raw") {
        normdata <- DESeqTransform(dds)
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
