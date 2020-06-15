#################### Plot GLM-PCA from Count matrix ######################

#' Plots Generalized Principal Component Analysis performed on raw counts using a count dataframe and
#' a targets file.
#' @param countDF Matrix object of Count data.
#' @param targets Object containing targets.
#' @param colData Dataframe containing metadata about each sample.
#' @examples
#' ## Create targets file object
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' colData <- data.frame(row.names = targets$SampleName,
#'             condition = targets$Factor)
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#' ## Create GLM-PCA plot
#' run_GLM(countDF = countDF, targets = targets, colData = colData)
run_GLM <- function(countDF, targets, colData) {
    ## Create full DESeqDataSet object
    dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                                  design = ~condition)
    count_mat <- counts(dds)
    ##glmpca is performed on raw counts
    nozero <- count_mat[which(rowSums(count_mat) > 0),]  
    gpca <- glmpca(nozero, L=2)
    gpca.dat <- gpca$factors
    gpca.dat$condition <- dds$condition
    Sample <- targets$Factor
    g <- ggplot(gpca.dat, aes(x = dim1, y = dim2, color = Sample)) +   geom_point(size =2) + coord_fixed() + ggtitle("Generalized PCA (GLM-PCA)")
    ggplotly(g)
}
