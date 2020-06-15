#################### Plot t-SNE from Count matrix ######################

#' Plots t-Distributed Stochastic Neighbor Embedding between samples using a
#' count dataframe and a targets file.
#' @param countDF Matrix object of Count data.
#' @param targets Object containing targets.
#' @examples
#' ## Create targets file object
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' colData <- data.frame(row.names = targets$SampleName,
#'             condition = targets$Factor)
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#' ## Create t-SNE plot
#' run_TSNE(countDF, targets)
run_TSNE <- function(countDF, targets) {
countDF_uni <- t(unique(countDF)) # removes duplicates and transposes matrix
set.seed(42) #seeding for reproducibility
tsne_out <- Rtsne(countDF_uni,dims = 2,pca=T,perplexity=5,theta=0.0) # Run TSNE with perplexity = 5
targets <- data.frame(targets)
Sample <- targets$Factor
plotdata <- data.frame(tsne_x = tsne_out$Y[,1], tsne_y = tsne_out$Y[,2])
g <- ggplot(plotdata, aes(x = tsne_x, y = tsne_y, color = Sample)) + geom_point(size =2) + ggtitle("t-SNE")
ggplotly(g)
}
