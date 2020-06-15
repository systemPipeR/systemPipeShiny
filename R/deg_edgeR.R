##' Bar Plot of DEGs from a count matrix.
##' 
##' Plots a Bar Plot using \code{run_edgeR} to create a edgeR data frame of
##' differentially expressed genes.
##' 
##' @param countDF Matrix containing Count data.
##' @param targets targets \code{data.frame}
##' @param FDR False Discovery Rate cut off for filtering.
##' @param Fold Log Fold Change cut off for filtering.
##' @param cmpset Number pertaining to index of set of \code{cmp} matrix
##' desired.
##' @param cmp \code{character matrix} where comparisons are defined in two
##' columns. This matrix should be generated with \code{readComp()} from the
##' targets file. Values used for comparisons need to match those in the Factor
##' column of the targets file.
##' @param plot If plot = \code{TRUE}, then function will plot a bar plot. If
##' plot = \code{false}, function will return the edgeR \code{data frame}.
##' @examples
##' 
##' ## Create DEG dataframe
##' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
##' targets <- read.delim(targetspath, comment="#")
##' cmp <- readComp(file=targetspath, format="matrix", delim="-")
##' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
##' countDF <- read.delim(countfile, row.names=1)
##' 
##' ## Create plot
##' deg_edgeR(countDF, targets, Fold = 2, FDR = 10, cmpset = 1, cmp, plot = T)

deg_edgeR <- function(countDF, targets, Fold, FDR, cmpset, cmp, plot = T) {
    edgeDF <- systemPipeR::run_edgeR(countDF = countDF, targets = targets, cmp = cmp[[cmpset]], independent = FALSE, 
                                     mdsplot = "")
    DEG_list <-  systemPipeR::filterDEGs(degDF = edgeDF, filter = c(Fold = Fold, FDR = FDR), plot = F)
    if (plot == T) {
        df <- DEG_list$Summary
        filter <- c(Fold = Fold, FDR = FDR)
        ## plot
        mytitle <- paste("edgeR DEG Counts (", names(filter)[1], ": ", filter[1], " & " , names(filter)[2], ": ", filter[2], "%)", sep="")
        df_plot <- data.frame(Comparisons=rep(as.character(df$Comparisons), 2), Counts=c(df$Counts_Up, df$Counts_Down), Type=rep(c("Up", "Down"), each=length(df[,1])))
        p <- ggplot(df_plot, aes(Comparisons, Counts, fill = Type)) + geom_bar(position="stack", stat="identity") + coord_flip() + theme(axis.text.y=element_text(angle=0, hjust=1)) + ggtitle(mytitle)
        ggplotly(p)
    } else {return (edgeDF)}
}
