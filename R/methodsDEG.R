# All DEG reprocessing methods

#################### Bar Plot of DEGs from a count matrix ######################

#' Plots a Bar Plot using \code{run_DESeq2} to create a DESeq2 data frame of
#' differentially expressed genes.
#' @param countDF Matrix containing Count data.
#' @param targets targets \code{data.frame}
#' @param FDR False Discovery Rate cut off for filtering.
#' @param Fold Log Fold Change cut off for filtering.
#' @param cmpset Number pertaining to index of set of \code{cmp} matrix
#' desired.
#' @param cmp \code{character matrix} where comparisons are defined in two
#' columns. This matrix should be generated with \code{readComp()} from the
#' targets file. Values used for comparisons need to match those in the Factor
#' column of the targets file.
#' @param plot If plot = \code{TRUE}, then function will plot a bar plot. If
#' plot = \code{false}, function will return the DESeq2 \code{data frame}.
#' @noRd
#' @examples
#' ## Create DEG dataframe
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- readComp(file=targetspath, format="matrix", delim="-")
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#'
#' ## Create plot
#' deg_deseq2(countDF, targets, Fold = 2, FDR = 10, cmpset = 1, cmp, plot = T)
deg_deseq2 <- function(countDF, targets, Fold, FDR, cmpset, cmp, plot = T) {
  degseqDF <- systemPipeR::run_DESeq2(countDF = countDF, targets = targets, cmp = cmp[[cmpset]],
                         independent = FALSE)
  DEG_list <- systemPipeR::filterDEGs(degDF = degseqDF, filter = c(Fold = Fold, FDR = FDR), plot = F)
  if (plot == T){
    df <- DEG_list$Summary
    filter <- c(Fold = Fold, FDR = FDR)
    ## plot
    mytitle <- paste("DESeq2 DEG Counts (", names(filter)[1], ": ", filter[1], " & " , names(filter)[2], ": ", filter[2], "%)", sep="")
    df_plot <- data.frame(Comparisons=rep(as.character(df$Comparisons), 2), Counts=c(df$Counts_Up, df$Counts_Down), Type=rep(c("Up", "Down"), each=length(df[,1])))
    p <- ggplot(df_plot, aes(Comparisons, Counts, fill = Type)) + geom_bar(position="stack", stat="identity") + coord_flip() + theme(axis.text.y=element_text(angle=0, hjust=1)) + ggtitle(mytitle)
    plotly::ggplotly(p)
  } else {return(degseqDF)}
}

#################### Bar Plot of DEGs from a count matrix ######################
#' Plots a Bar Plot using \code{run_edgeR} to create a edgeR data frame of
#' differentially expressed genes.
#' @param countDF Matrix containing Count data.
#' @param targets targets \code{data.frame}
#' @param FDR False Discovery Rate cut off for filtering.
#' @param Fold Log Fold Change cut off for filtering.
#' @param cmpset Number pertaining to index of set of \code{cmp} matrix
#' desired.
#' @param cmp \code{character matrix} where comparisons are defined in two
#' columns. This matrix should be generated with \code{readComp()} from the
#' targets file. Values used for comparisons need to match those in the Factor
#' column of the targets file.
#' @param plot If plot = \code{TRUE}, then function will plot a bar plot. If
#' plot = \code{false}, function will return the edgeR \code{data frame}.
#' @noRd
#' @examples
#' ## Create DEG dataframe
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- readComp(file=targetspath, format="matrix", delim="-")
#' countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countDF <- read.delim(countfile, row.names=1)
#'
#' ## Create plot
#' deg_edgeR(countDF, targets, Fold = 2, FDR = 10, cmpset = 1, cmp, plot = T)
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
        plotly::ggplotly(p)
    } else {return (edgeDF)}
}

######### deg_venn

#' @noRd
deg_venn <- function(DF, FDR, Fold, cmp_list) {
    DEG_list <- systemPipeR::filterDEGs(degDF = DF, filter = c(Fold = Fold, FDR = FDR), plot = F)
    up <- DEG_list$Up
    down <- DEG_list$Down
    index.up <- which(names(DEG_list$Up) %in% cmp_list)
    index.down <- which(names(DEG_list$Down) %in% cmp_list)
    listup <- DEG_list$Up[index.up]
    listdown <- DEG_list$Down[index.down]
    vennsetup <- systemPipeR::overLapper(listup, type = "vennsets")
    vennsetdown <- systemPipeR::overLapper(listdown, type = "vennsets")
    systemPipeR::vennPlot(list(vennsetup, vennsetdown), mymain = "", mysub = "",
                          colmode = 2, ccol = c("blue", "red"))
}



load_count <- function(count_table, targets_file) {
    if (!(is.null(targets_file)) == T) {
        targets <- read.delim(targets_file, comment.char = "#")
        targets <- data.frame(targets)
        colData <- data.frame(row.names = targets$SampleName,
                              condition = targets$Factor)
    }
    countDF <- as.matrix(read.table(count_table))
    cmp <- readComp(file = targets_file, format = "matrix", delim = "-")
    list <- list(cmp = cmp, countDF = countDF, targets = targets, colData = colData)
    return(list)
}

##load_count("counttable.xls", "targets.txt")

#' @noRd
readComp <- function (file, format = "vector", delim = "-")
{
    if (!format %in% c("vector", "matrix"))
        stop("Argument format can only be assigned: vector or matrix!")
    if (class(file) == "SYSargs") {
        if (length(systemPipeR::targetsheader(file)) == 0)
            stop("Input has no targets header lines.")
        comp <- systemPipeR::targetsheader(file)
    }
    else if (class(file) == "SYSargs2") {
        if (length(systemPipeR::targetsheader(file)[[1]]) == 0)
            stop("Input has no targets header lines.")
        comp <- systemPipeR::targetsheader(file)[[1]]
    }
    else {
        comp <- readLines(file)
    }
    comp <- comp[grepl("<CMP>", comp)]
    comp <- gsub("#.*<CMP>| {1,}", "", comp)
    comp <- gsub("\t", "", comp)
    comp <- gsub("^\"|\"$", "", comp)
    comp <- strsplit(comp, ":|,")
    names(comp) <- lapply(seq(along = comp), function(x) comp[[x]][1])
    comp <- sapply(names(comp), function(x) comp[[x]][-1], simplify = FALSE)
    checkvalues <- unique(unlist(strsplit(unlist(comp), "-")))
    checkvalues <- checkvalues[checkvalues != "ALL"]
    if (class(file) == "SYSargs") {
        all <- unique(as.character(targetsin(file)$Factor))
    }
    else if (class(file) == "SYSargs2") {
        all <- unique(as.character(targets.as.df(targets(args_bam))$Factor))
    }
    else {
        all <- unique(as.character(read.delim(file, comment.char = "#")$Factor))
    }
    if (any(!checkvalues %in% all))
        stop(paste("The following samples are not present in Factor column of targets file:",
                   paste(checkvalues[!checkvalues %in% all], collapse = ", ")))
    allindex <- sapply(names(comp), function(x) any(grepl("ALL",
                                                          comp[[x]])))
    if (any(allindex))
        for (i in which(allindex)) comp[[i]] <- combn(all, m = 2,
                                                      FUN = paste, collapse = delim)
    if (format == "vector" & delim != "-")
        comp <- sapply(names(comp), function(x) gsub("-",
                                                     delim, comp[[x]]), simplify = FALSE)
    if (format == "vector")
        return(comp)
    if (format == "matrix")
        return(sapply(names(comp), function(x) do.call("rbind",
                                                       strsplit(comp[[x]], "-")), simplify = FALSE))
}

