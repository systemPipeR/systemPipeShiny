# All plotting functions for DEGs

#################### Plots a clustering dendrogram from Count matrix ######################
# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

#' Plots Hierarchical Clustering dendrogram between samples using a count
#' dataframe and a targets file.
#' @param countDF Matrix object of Count data.
#' @param targets targets data.frame
#' @param colData Dataframe containing metadata about each sample.
#' @param method Normalization method for plot. Options include "raw", "rlog",
#' "vst".
#' @noRd
# @examples
# ## Create targets file object
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# colData <- data.frame(row.names = targets$SampleName,
#             condition = targets$Factor)
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# ## Create Dendrogram plot
# run_CLUST(countDF = countDF, targets = targets, colData = colData, method = "raw")
run_CLUST <- function(countDF, targets, colData, method) {
    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = countDF, colData = colData,
                                  design = ~condition)
    dds <- DESeq2::DESeq(dds)
    #method
    if (method == "rlog") {
        normdata <- DESeq2::rlog(dds, blind=TRUE)
    } else if (method == "vst") {
        normdata <- DESeq2::varianceStabilizingTransformation(dds, blind = T)
    } else if (method == "raw") {
        normdata <- DESeq2::DESeqTransform(dds)
    }
    d <- cor(SummarizedExperiment::assay(normdata), method = "spearman")
    hc <- hclust(dist(1 - d))
    ape::plot.phylo(ape::as.phylo(hc), type = "p", edge.col = "blue", edge.width = 2,
               show.node.label = TRUE, no.margin = TRUE)

}


#################### Plot GLM-PCA from Count matrix ######################
# library(glmpca)
# library(DESeq2, quietly = TRUE)
# library(ggplot2)

#' Plots Generalized Principal Component Analysis performed on raw counts using a count dataframe and
#' a targets file.
#' @param countDF Matrix object of Count data.
#' @param targets Object containing targets.
#' @param colData Dataframe containing metadata about each sample.
#' @noRd
# @examples
# ## Create targets file object
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# colData <- data.frame(row.names = targets$SampleName,
#             condition = targets$Factor)
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# ## Create GLM-PCA plot
# run_GLM(countDF = countDF, targets = targets, colData = colData)
run_GLM <- function(countDF, targets, colData) {
    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = countDF, colData = colData,
                                  design = ~condition)
    count_mat <- DESeq2::counts(dds)
    ##glmpca is performed on raw counts
    nozero <- count_mat[which(rowSums(count_mat) > 0),]
    gpca <- glmpca::glmpca(nozero, L=2)
    gpca.dat <- gpca$factors
    gpca.dat$condition <- dds$condition
    Sample <- targets$Factor
    g <- ggplot(gpca.dat, aes(x = dim1, y = dim2, color = Sample)) +   geom_point(size =2) + coord_fixed() + ggtitle("Generalized PCA (GLM-PCA)")
    plotly::ggplotly(g)

}

#################### Plot Clustering Heat Map from Count matrix ######################
# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)
# library("RColorBrewer")
# library(plotly)

#' Plots a hierarchical clustering heat map using a count dataframe and a
#' targets file.
#' @param countDF Matrix of Count data.
#' @param targets targets data.frame
#' @param colData Dataframe containing metadata about each sample.
#' @param method Normalization method for plot. Options include "raw", "rlog",
#' "vst".
#' @noRd
# @examples
# ## Create targets file object
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# colData <- data.frame(row.names = targets$SampleName,
#             condition = targets$Factor)
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# ## Create Heat map
# run_HEAT(countDF = countDF, targets = targets, colData = colData, method = "raw")
run_HEAT <- function(countDF, targets, colData, method) {
    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = countDF, colData = colData,
                                  design = ~condition)
    dds <- DESeq2::DESeq(dds)
    #method
    if (method == "rlog") {
        normdata <- DESeq2::rlog(dds, blind=TRUE)
    } else if (method == "vst") {
        normdata <- DESeq2::varianceStabilizingTransformation(dds, blind = T)
    } else if (method == "raw") {
        normdata <- DESeq2::DESeqTransform(dds)
    }
    d <- cor(SummarizedExperiment::assay(normdata), method = "spearman")
    plotly::plot_ly(x = colnames(d), y = rownames(d), z = d, color="Greys", type = "heatmap")
}


# library(limma)



#' @noRd
run_LIMMA <- function(count_table, targetspath, approach, independent=TRUE, paired=NULL, mdsplot = "") {
    targets <- read.delim(targetspath, comment = "#")
    cmp <- readComp(file = targetspath, format = "matrix", delim = "-")
    cmp = cmp[[1]]

    countDFeBygpath <- count_table
    countDFeByg <- read.delim(countDFeBygpath, row.names = 1)
    countDF <- countDFeByg

    if(class(cmp) != "matrix" & length(cmp)==2) cmp <- t(as.matrix(cmp)) # If cmp is vector of length 2, convert it to matrix.
    samples <- as.character(targets$Factor); names(samples) <- paste(as.character(targets$SampleName), "", sep="")
    countDF <- countDF[, names(samples)]
    countDF[is.na(countDF)] <- 0
    DF <- data.frame(row.names=rownames(countDF))
    group <- as.character(samples)

    if(independent==TRUE) {
        loopv <- seq(along=cmp[,1])
    } else {
        loopv <- 1
    }

    for(j in loopv) {
        ## Filtering and normalization
        y <- DGEList(counts=countDF, group=group) # Constructs DGEList object
        if(independent == TRUE) {
            subset <- samples[samples %in% cmp[j,]]
            y <- y[, names(subset)]
            y$samples$group <- factor(as.character(y$samples$group))
        }
        keep <- DelayedArray::rowSums(cpm(y)>1) >= 2; y <- y[keep, ]
        y <- calcNormFactors(y)
        ## Design matrix
        if(length(paired)==0) {
            design <- model.matrix(~0+y$samples$group, data=y$samples)
            colnames(design) <- levels(y$samples$group)
        } else {
            if(length(paired)>0 & independent==FALSE) stop("When providing values under 'paired' also set independent=TRUE")
            Subject <- factor(paired[samples %in% cmp[j,]]) # corrected Jun 2014 (won't change results)
            Treat <- y$samples$group
            design <- model.matrix(~Subject+Treat)
            levels(design) <- levels(y$samples$group)
        }

        if(independent == TRUE) {
            mycomp <- paste(cmp[j,1], cmp[j,2], sep="-")
        } else {
            mycomp <- paste(cmp[,1], cmp[,2], sep="-")
        }
        if(length(paired)==0) contrasts <- makeContrasts(contrasts=mycomp, levels=design)


        if(approach == "limma-trend") { ## prior trend approach
            for(i in seq(along=mycomp)) {
                # deg <- as.data.frame(topTags(lrt, n=length(rownames(y))))
                logCPM <- cpm(y, log=TRUE, prior.count=3) ## counts are converted to logCPM values
                fit <- limma::lmFit(logCPM, design)
                fit <- limma::eBayes(fit, trend=TRUE)
                upordown <- Biostrings::summary(limma::decideTests(fit))
                ## all genes, p-value adjusted by fdr
                deg <- as.data.frame(topTable(fit, coef=ncol(design), number = length(rownames(y)), adjust = "fdr"), n=length(rownames(y)))
                colnames(deg) <- c("logFC", "AveExpr", "t", "pvalue", "FDR", "B")
                colnames(deg) <- paste(paste(mycomp[i], collapse="_"), colnames(deg), sep="_")
                DF <- cbind(DF, deg[rownames(DF),])
            }
            summary(decideTests(fit))

        }

        if(approach == "voom") { ## precision weights approach
            for(i in seq(along=mycomp)) {
                v <- voom(y, design, plot=TRUE) ## voom transformation
                fit <- lmFit(v, design)
                fit <- eBayes(fit)
                deg <- as.data.frame(limma::topTable(fit, coef=ncol(design), number = length(rownames(y))), n=length(rownames(y)))
                colnames(deg) <- c("logFC", "AveExpr", "t", "pvalue", "FDR", "B")
                colnames(deg) <- paste(paste(mycomp[i], collapse="_"), colnames(deg), sep="_")
                DF <- cbind(DF, deg[rownames(DF),])

            }
        }

    }
    return(DF)

    ##example: run_LIMMA("countDFeByg.xls","targets.txt","limma-trend")
    ##or run_LIMMA("countDFeByg.xls","targets.txt","voom")
}


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
#' @noRd
# @examples
# ## Create targets file object
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# colData <- data.frame(row.names = targets$SampleName,
#             condition = targets$Factor)
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# ## Create MDS plot
# run_MDS(countDF = countDF, targets = targets, colData = colData, method = "rlog")
run_MDS <- function(countDF, targets, colData, method) {
    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = countDF, colData = colData,
                                  design = ~condition)
    dds <- DESeq2::DESeq(dds)
    if (method == "rlog") {
        normdata <- DESeq2::rlog(dds, blind=TRUE)
    } else if (method == "vst") {
        normdata <- DESeq2::varianceStabilizingTransformation(dds, blind = T)
    }
    ## transformation to a distance matrix
    d <- cor(SummarizedExperiment::assay(normdata), method = "spearman")
    distmat <- dist(1-d)
    ## perform MDS
    mdsData <- data.frame(cmdscale(distmat))
    mds <- cbind(mdsData, as.data.frame(SummarizedExperiment::colData(dds)))
    Sample <- targets$Factor
    g <- ggplot(mds, aes(X1,X2,color=Sample)) + geom_point(size=3) + scale_y_reverse()  + ggtitle("Multidimensional Scaling (MDS)")
    plotly::ggplotly(g)


}


#################### Plot PCA from Count matrix ######################
# library(DESeq2, quietly = TRUE)
# library(ape, warn.conflicts = FALSE)

#' Plots Principal Component Analysis from a count dataframe and a targets file.
#' @param countDF Matrix of Count data.
#' @param targets targets data.frame
#' @param colData Dataframe containing metadata about each sample.
#' @param method Normalization method for plot. Options for PCA include "rlog", "raw", "vst".
#' @noRd
# @examples
# ## Create targets file object
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# colData <- data.frame(row.names = targets$SampleName,
#             condition = targets$Factor)
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# ## Create PCA plot
# run_PCA(countDF = countDF, targets = targets, colData = colData, method = "raw")
run_PCA <- function(countDF, targets, colData, method) {

    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = countDF, colData = colData,
                                  design = ~condition)
    dds <- DESeq2::DESeq(dds)
    #method
    if (method == "rlog") {
        normdata <- DESeq2::rlog(dds, blind=TRUE)
    } else if (method == "vst") {
        normdata <- DESeq2::varianceStabilizingTransformation(dds, blind = T)
    } else if (method == "raw") {
        normdata <- DESeq2::DESeqTransform(dds)
    }
    pcaData <- DESeq2::plotPCA(normdata, intgroup="condition", returnData=TRUE)
    percentVar <- round(100 * attr(pcaData, "percentVar"))
    Sample <- targets$Factor
    g <- ggplot(pcaData, aes(PC1, PC2, color=Sample)) +
        geom_point(size=3) +
        xlab(paste0("PC1: ",percentVar[1],"% variance")) +
        ylab(paste0("PC2: ",percentVar[2],"% variance")) +
        coord_fixed() + ggtitle("Principal Component Analysis (PCA)")
    plotly::ggplotly(g)
}


#################### Plot t-SNE from Count matrix ######################
## tSNE has two main parameters: the perplexity and learning rate
## tSNE is a stochastic algorithm which means running the method multiple times on the same dataset will result in different plots.
##To ensure reproducibility, we fix the
## "seed" of the random-number generator in the code below so that we always get the same plot
## tSNE faithfully represents local relationships
# library(Rtsne)
# library(ggplot2)

#' Plots t-Distributed Stochastic Neighbor Embedding between samples using a
#' count dataframe and a targets file.
#' @param countDF Matrix object of Count data.
#' @param targets Object containing targets.
#' @noRd
# @examples
# ## Create targets file object
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# colData <- data.frame(row.names = targets$SampleName,
#             condition = targets$Factor)
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# ## Create t-SNE plot
# run_TSNE(countDF, targets)
run_TSNE <- function(countDF, targets) {
    countDF_uni <- t(unique(countDF)) # removes duplicates and transposes matrix
    set.seed(42) #seeding for reproducibility
    tsne_out <- Rtsne::Rtsne(countDF_uni,dims = 2,pca=T,perplexity=5,theta=0.0) # Run TSNE with perplexity = 5
    targets <- data.frame(targets)
    Sample <- targets$Factor
    plotdata <- data.frame(tsne_x = tsne_out$Y[,1], tsne_y = tsne_out$Y[,2])
    g <- ggplot(plotdata, aes(x = tsne_x, y = tsne_y, color = Sample)) + geom_point(size =2) + ggtitle("t-SNE")
    plotly::ggplotly(g)
}


#################### Plots a Volcano Plot ######################
# library(ggplot2)

#' Plots  a Volcano Plot from an \code{edgeR} or \code{DESeq2} data frame.
#' @param DF \code{edgeR} or \code{DESeq2} data frame.
#' @param FDR False Discovery Rate cut off for filtering.
#' @param Fold Log Fold Change cut off for filtering.
#' @param Comparison \code{String} names of samples or conditions that are
#' compared. Example: "M1-A1"
#' @noRd
# @examples
# ## Create DEG dataframe
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# cmp <- readComp(file=targetspath, format="matrix", delim="-")
# countfile <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countDF <- read.delim(countfile, row.names=1)
# edgeDF <- run_edgeR(countDF=countDF, targets=targets, cmp=cmp[[1]], independent=FALSE, mdsplot="")
#
# ## Create Volcano plot
# run_volcano(DF = edgeDF,FDR = 10, Fold = 2, comparison = "M1-A1")
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
    plotly::ggplotly(p)

}
