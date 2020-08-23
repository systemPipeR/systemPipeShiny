################
## exploreDDS ##
################
#' @title exploreDDS
#' @description Convenience wrapper function to transform raw read counts using the 
#' [DESeq2::DESeq2-package()] package transformations methods. The input file
#'  has to contain all the genes, not just differentially expressed ones. 
#'
#' @param countMatrix `date.frame` or `matrix` containing raw read counts.
#' @param targets targets `data.frame`.
#' @param cmp `character matrix` where comparisons are defined in two columns. 
#' This matrix should be generated with the [systemPipeR::readComp()] function from the targets file. 
#' Values used for comparisons need to match those in the `Factor` column of the targets file. 
#' @param preFilter allows removing rows in which there are very few reads. 
#' Accepts a numeric value with the minimum of total reads to keep. Default is `NULL`.
#' @param transformationMethod a `character string` indicating which transformation
#'  method it will be used on the raw read counts. Supported methods include 
#'  `rlog` and `vst` using the `DESeq2` package or default `raw` 
#'  for no data transformation.
#'  
#' @details Note that the recommendation is to use the resulting transformed values in the 
#' `transformationMethod` argument only for visualization and clustering, 
#' not for differential expression analysis which needs raw counts. Users are 
#' strongly encouraged to consult the  [DESeq2::DESeq2-package()] vignette for more detailed 
#' information on this topic and how to properly run `DESeq2` on data sets 
#' with more complex experimental designs.
#' 
#' @references For more details on `DESeq2`, please consult the following 
#' page: \href{http://bioconductor.org/packages/release/bioc/html/DESeq2.html}{DESeq2}.
#' For more details on `targets` file definition, please consult the following 
#' page: \href{http://www.bioconductor.org/packages/release/bioc/vignettes/systemPipeR/inst/doc/systemPipeR.html#25_structure_of_targets_file}{systemPipeR}.
#' 
#' @author Daniela Cassol
#' 
#' @return returns an object of class [DESeq2::DESeqTransform()].
#' 
#' @examples
#' suppressPackageStartupMessages({library(systemPipeR)})
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Run
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="raw")
#' exploredds 
#' 
#' @export 
#' @importFrom DESeq2 DESeqDataSetFromMatrix counts DESeq rlog varianceStabilizingTransformation
exploreDDS <- function(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="raw") {
    ## A few validations ##
    if (!transformationMethod %in% c("raw", "rlog", "vst")) 
        stop("Supported methods include 'raw', 'rlog' and 'vst'")
    if(is.data.frame(countMatrix)){
        countMatrix <- as.matrix(countMatrix)
    } else if(is.matrix(countMatrix)){
        countMatrix <- countMatrix
    } else {
        stop("countMatrix needs to be assigned an object of class 'data.frame' OR 'matrix'")
    }
    if (!is.data.frame(targets)) stop("targets needs to be assignes an object of class 'data.frame'")
    if (all(class(cmp) != "matrix" & length(cmp)==2)) cmp <- t(as.matrix(cmp))
    ## Samples
    samples <- as.character(targets$Factor); names(samples) <- paste(as.character(targets$SampleName), "", sep="")
    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData=countMatrix,
                                          colData=data.frame(condition=samples), design = ~ condition)
    ## Pre-filtering
    if(!is.null(preFilter)){
        if (!is.numeric(preFilter)) stop ("'preFilter' needs to be numeric value.")
        keep <- rowSums(DESeq2::counts(dds)) >= preFilter
        dds <- dds[keep,]
    }
    ## Estimate of (i) size factors, (ii) dispersion, (iii) negative binomial GLM fitting and (iv) Wald statistics
    dds_deseq2 <- DESeq2::DESeq(dds, quiet=TRUE)
    ## Count data transformations
    if (transformationMethod == "rlog") {
        normdata <- DESeq2::rlog(dds_deseq2, blind=  TRUE)
    } else if (transformationMethod == "vst") {
        normdata <- DESeq2::varianceStabilizingTransformation(dds_deseq2, blind = TRUE)
    } else if (transformationMethod == "raw") {
        normdata <- dds
    }
    return(normdata)
}

####################
## exploreDDSplot ##
####################
#' @title exploreDDSplot
#' @description Scatterplot of transformed counts from two samples or grid of all samples
#'
#' @param countMatrix `date.frame` or `matrix` containing raw read counts
#' @param targets targets `data.frame`
#' @param cmp `character matrix` where comparisons are defined in two columns. 
#' This matrix should be generated with the [systemPipeR::readComp()] function from the targets file. 
#' Values used for comparisons need to match those in the `Factor` column of the targets file. 
#' @param preFilter allows removing rows in which there are very few reads. 
#' Accepts a numeric value with the minimum of total reads to keep. Default is `NULL`.
#' @param samples a `character vector` of two samples or `ALL` samples in the dataset.
#' Could be specified the `SampleName` column name of the targets file or the 
#' respective numeric values. Also, if set as `ALL`, a correlation matrix it will be plot.
#' @param scattermatrix if `samples` set as `ALL`, requires to assign `TRUE` to build 
#' a correlation matrix and plot the correlogram of all the samples.
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#'
#' @return returns an object of `ggplot2 plot`.

#' @examples
#' library(systemPipeR)
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, samples=c(3,4))
#' exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], samples=c("M1A", "M1B"), save = TRUE,
#'              filePlot = "transf_deseq2.pdf")
#' ## Plot Correlogram
#' exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, samples=c("M1A", "M1B"), scattermatrix=TRUE)
#' 
#' @export exploreDDSplot
#' @importFrom DESeq2 estimateSizeFactors counts
#' @importFrom dplyr bind_rows as_tibble mutate group_by do
#' @importFrom GGally ggpairs
#' @importFrom ggplot2 aes aes_string ggplot geom_hex coord_fixed facet_grid xlab ylab ggsave
#' @importFrom plotly plot_ly subplot
#' @importFrom SummarizedExperiment assay
exploreDDSplot <- function(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, samples, 
                           scattermatrix = FALSE, plotly = FALSE, savePlot=FALSE, filePlot=NULL) {
    ## Validations
    SampleName <- targets$SampleName; names(SampleName) <- targets$SampleName
    if(is.numeric(samples)){
        samples <- SampleName[samples]
        if(!all(samples %in% SampleName)) stop(paste("samples position can be assigned from the following options", paste0(1:length(SampleName), collapse=", "), sep = " "))
    } else if(is.character(samples)){
        if(all(samples=="ALL")){
            samples <- SampleName
            if(!scattermatrix=="TRUE") stop("'scattermatrix' argument needs to set as TRUE in the case of ALL the samples selected.")
        } else {
            samples <- SampleName[samples]
            if(!all(samples %in% SampleName)) stop(paste("samples names can be assigned from the following options", paste0((SampleName), collapse=", "), sep = " "))
        }
    }
    ## Calculate the data transformations
    suppressWarnings({
        vst <- exploreDDS(countMatrix, targets, cmp=cmp, preFilter=preFilter, transformationMethod="vst")
        rlog <- exploreDDS(countMatrix, targets, cmp=cmp, preFilter=preFilter, transformationMethod="rlog")
        dss <- exploreDDS(countMatrix, targets, cmp=cmp, preFilter=preFilter, transformationMethod="raw")
        dss <- DESeq2::estimateSizeFactors(dss)})
    ## create dataframe with transformed values
    transform_df <- dplyr::bind_rows(
        dplyr::as_tibble(log2(DESeq2::counts(dss, normalized = TRUE)[, samples] + 1)) %>%
            dplyr::mutate(transformation = "log2(x + 1)"),
        dplyr::as_tibble(SummarizedExperiment::assay(vst)[, samples]) %>% dplyr::mutate(transformation = "vst"),
        dplyr::as_tibble(SummarizedExperiment::assay(rlog)[, samples]) %>% dplyr::mutate(transformation = "rlog"))
    names <- colnames(transform_df)[1:2]
    lvl <- levels(factor(transform_df$transformation))
    ## plot
    if (scattermatrix==TRUE){
        plot <- GGally::ggpairs(transform_df, title="Scatterplot of transformed counts", ggplot2::aes_string(colour="transformation")) 
    } else {
        plot <- ggplot2::ggplot(transform_df, ggplot2::aes(x = .data[[names[1]]], y = .data[[names[2]]])) +
            ggplot2::geom_hex(bins = 80) +
            ggplot2::coord_fixed() + ggplot2::facet_grid( . ~transformation) +
            ggplot2::xlab(names[1]) + ggplot2::ylab(names[2])
    }
    if (savePlot == TRUE) {
        ggplot2::ggsave(filePlot, scale = 0.8)
    }
    ## Return
    if (plotly == TRUE) {
        plot <- transform_df %>%
            dplyr::group_by(transformation) %>%
            dplyr::do(p=plotly::plot_ly(., x = .data[[names[1]]], y = .data[[names[2]]], color = ~transformation, type = "scatter", 
                                        name= ~transformation, showlegend=TRUE, legendgroup = ~transformation)) %>% 
            plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
    }
    return(plot)
}

################
## hclustplot ##
################
#' @title Hierarchical Clustering Dendrogram (hclustplot)
#' @description This function computes the sample-wise correlation coefficients using the 
#' [stats::cor()] function from the transformed expression values. After transformation 
#' to a distance matrix, hierarchical clustering is performed with 
#' the [stats::hclust()] function, and the result is plotted as a dendrogram.
#' 
#' @param exploredds object of class [DESeq2::DESeqTransform()].
#' @param method a `character string` indicating which correlation coefficient is to be computed, 
#' based on the [stats::cor()] function. Options are: c("pearson" "kendall", "spearman").
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#'
#' @return returns an object of `ggplot` or `plotly` class.
#'
#' @examples
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="rlog")
#' hclustplot(exploredds, method = "spearman")
#' hclustplot(exploredds, method = "spearman", savePlot = TRUE, filePlot = "cor.pdf")
#' 
#' @export
#' @importFrom ape as.phylo
#' @importFrom ggplot2 coord_cartesian margin ggsave
#' @importFrom ggtree ggtree geom_tiplab theme_tree
#' @importFrom plotly ggplotly
#' @importFrom stats cor hclust dist
#' @importFrom SummarizedExperiment assay
hclustplot <- function(exploredds, method = "spearman", plotly = FALSE, savePlot = FALSE, filePlot = NULL) {
    ## Validations
    if (!class(exploredds) == "DESeqTransform") stop("'exploredds' needs to be assignes an object of class 'DESeqTransform'. For more information check 'help(exploreDDS)'.")
    ## cor() computes the correlation coefficient
    d <- stats::cor(SummarizedExperiment::assay(exploredds), method = method)
    ## Hierarchical cluster analysis
    hc <- stats::hclust(stats::dist(1 - d))
    ## plot phylogenetic trees
    plot <- ggtree::ggtree(ape::as.phylo(hc), color="blue") + ggtree::geom_tiplab() +
        ggplot2::coord_cartesian(clip = 'off') + ggtree::theme_tree(plot.margin=ggplot2::margin(6, 60, 6, 6))
    if (savePlot == TRUE){
        ggplot2::ggsave(filePlot, scale = 0.8)
    }
    ##Return
    if (plotly == TRUE){
        return(plotly::ggplotly(plot)) }
    return(plot)
}

################
## heatMaplot ##
################
#' @title Hierarchical Clustering HeatMap (heatMaplot)
#' @description This function performs hierarchical clustering on the transformed expression matrix 
#'  generated with the DESeq2 package. It uses, by default, a Pearson correlation-based distance
#' measure and complete linkage for cluster join.
#' 
#' @param exploredds object of class [DESeq2::DESeqTransform()].
#' @param clust sselect the data to apply the distance matrix computation. 
#' If `samples` selected, it will be applied the [stats::dist()] function to the
#'  transformed count matrix to get sample-to-sample distances. If `ind`, it is 
#'  necessary to provide the list of differentially expressed genes, 
#'  for the `exploredds` subset. 
#' @param DEGlist List of up or down regulated gene/transcript indentifiers meeting 
#' the chosen filter settings for all comparisons defined in data frames `pval` and `log2FC`.
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#' @param ... additional parameters for the [pheatmap::pheatmap()] function.
#' 
#' @return returns an object of `pheatmap` or `plotly` class.
#'
#' @examples
#' ### Load data 
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Samples plot
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="rlog")
#' heatMaplot(exploredds, clust="samples")
#' heatMaplot(exploredds, clust="samples", plotly = TRUE)
#' ## Individuals genes identified in DEG analysis
#' ### DEG analysis with `systemPipeR`
#' degseqDF <- systemPipeR::run_DESeq2(countDF = countMatrix, targets = targets, cmp = cmp[[1]], independent = FALSE)
#' DEG_list <- systemPipeR::filterDEGs(degDF = degseqDF, filter = c(Fold = 2, FDR = 10))
#' ### Plot
#' heatMaplot(exploredds, clust="ind", DEGlist = unique(as.character(unlist(DEG_list[[1]]))))
#' heatMaplot(exploredds, clust="ind", DEGlist = unique(as.character(unlist(DEG_list[[1]]))), plotly = TRUE)
#' 
#' @export
#' @importFrom ggplot2 ggsave
#' @importFrom pheatmap pheatmap
#' @importFrom plotly plot_ly
#' @importFrom stats dist
#' @importFrom SummarizedExperiment assay
heatMaplot <- function(exploredds, clust, DEGlist = NULL, plotly = FALSE, savePlot = FALSE, filePlot = NULL, ...) {
    ## Validations
    if (!class(exploredds) == "DESeqTransform") stop("'exploredds' needs to be assignes an object of class 'DESeqTransform'. For more information check 'help(exploreDDS)'")
    anno <- as.data.frame(exploredds$condition); colnames(anno) <- "Condition"
    ## sample-to-sample distances
    if (clust == "samples") {
        sampleDists <- stats::dist(t(SummarizedExperiment::assay(exploredds)))
        sampleDistMatrix <- as.matrix(sampleDists)
        rownames(anno) <- colnames(sampleDistMatrix)
        if (plotly == FALSE) {
            pheatPlot <- pheatmap::pheatmap(sampleDistMatrix, clustering_distance_rows = sampleDists,
                                            clustering_distance_cols = sampleDists, annotation_col = anno)
        } else if (plotly == TRUE) {
            plot <- plotly::plot_ly(x = colnames(sampleDistMatrix), y = rownames(sampleDistMatrix),
                                    z = sampleDistMatrix, type = "heatmap")
        }
    } else if (clust == "ind") {
        ## Hierarchical clustering on the transformed expression matrix subsetted by the DEGs identified in differential expression analysis.
        if (any(is.null(DEGlist) | !is.character(DEGlist))) stop("Provide a character vector with the gene names identified in differential expression analysis.")
        dist <- SummarizedExperiment::assay(exploredds)[DEGlist, ]
        rownames(anno) <- colnames(dist)
        if (plotly == FALSE) {
            pheatPlot <- pheatmap::pheatmap(dist, scale = "row", clustering_distance_rows = "correlation",
                                            clustering_distance_cols = "correlation", annotation_col = anno)
        } else if (plotly == TRUE) {
            plot <- plotly::plot_ly(x = colnames(dist), y = rownames(dist), z = dist, type = "heatmap")
        }
    } else {stop("Supported clust include 'samples' and 'ind'") }
    if (savePlot == TRUE) {
        ggplot2::ggsave(plot = pheatPlot, filename = filePlot)
    }
    ##Return
    if (plotly == TRUE) {
        return(plot) }
    return(pheatPlot)
}

#############
## PCAplot ##
#############
#' @title PCAplot
#' @description This function plots a Principal Component Analysis (PCA) from 
#' transformed expression matrix. This plot shows samples variation based on the 
#' expression values and identifies batch effects.
#'
#' @param exploredds object of class [DESeq2::DESeqTransform()].
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#'
#' @return returns an object of `ggplot` or `plotly` class.
#' 
#' @examples
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="rlog")
#' PCAplot(exploredds, plotly = FALSE)
#' PCAplot(exploredds, plotly = TRUE)
#' PCAplot(exploredds, save = TRUE, filePlot = "pca.pdf")
#' 
#' @export
#' @importFrom DESeq2 DESeqTransform plotPCA
#' @importFrom ggplot2 ggplot aes_string geom_point xlab ylab coord_fixed ggtitle ggsave
#' @importFrom plotly ggplotly
PCAplot <- function(exploredds, plotly = FALSE, savePlot = FALSE, filePlot = NULL) {
    ## Validations
    if (!class(exploredds) == "DESeqTransform") {
        warning("'exploredds' needs to be assignes an object of class 'DESeqTransform'.
    Here we are converting the object into a 'DESeqTransform'class for
    downstream analysis. For more information check 'help(exploreDDS)'")
        exploredds <- DESeq2::DESeqTransform(exploredds) }
    ## Plot
    pcaData <- DESeq2::plotPCA(exploredds, intgroup = "condition", returnData = TRUE)
    percentVar <- round(100 * attr(pcaData, "percentVar"))
    Sample <- exploredds$condition
    plot <- ggplot2::ggplot(pcaData, ggplot2::aes_string("PC1", "PC2", color = Sample)) +
        ggplot2::geom_point(size=3) +
        ggplot2::xlab(paste0("PC1: ",percentVar[1],"% variance")) +
        ggplot2::ylab(paste0("PC2: ",percentVar[2],"% variance")) +
        ggplot2::coord_fixed() + ggplot2::ggtitle("Principal Component Analysis (PCA)")
    ## Save plot
    if (savePlot == TRUE){
        ggplot2::ggsave(plot = plot, filename = filePlot)
    }
    ## Return
    if (plotly == TRUE){
        return(plotly::ggplotly(plot)) }
    return(plot)
}

#############
## GLMplot ##
#############
#' @title Dimension Reduction with GLMplot
#' @description This function computes and plots generalized principal components 
#' analysis for dimension reduction of count expression matrix.  
#' 
#' @param exploredds object of class [DESeq2::DESeqDataSet()], generated from `exploreDDS` function. 
#' Also, accepts the `date.frame` containing raw read counts.
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#' @param ... additional parameters for the [glmpca::glmpca()] function.
#'
#' @return returns an object of `ggplot` or `plotly` class.
#' 
#' @examples
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="raw")
#' GLMplot(exploredds, plotly = FALSE)
#' GLMplot(exploredds, plotly = FALSE, savePlot = TRUE, filePlot = "GML.pdf")
#' 
#' @export
#' @importFrom DESeq2 counts
#' @importFrom ggplot2 ggplot aes aes_string geom_point coord_fixed ggtitle ggsave
#' @importFrom glmpca glmpca
#' @importFrom plotly ggplotly
GLMplot <- function(exploredds, plotly = FALSE, savePlot = FALSE, filePlot = NULL, ...) {
    ## Add validation, need to be counts reads
    if (is.data.frame(exploredds)) {
        count_mat <- exploredds
    } else if (class(exploredds) == "DESeqDataSet") {
        count_mat <- DESeq2::counts(exploredds)
    } else if (!class(exploredds) == "DESeqDataSet") {
        stop("'exploredds' needs to be assignes an object of class 'DESeqDataSet'.
                For more information check 'help(exploreDDS)', and select the transformationMethod='raw'")
    }
    ##glmpca is performed on raw counts
    nozero <- count_mat[which(rowSums(count_mat) > 0),]
    gpca <- glmpca::glmpca(nozero, L=2, ...)
    gpca.dat <- gpca$factors
    gpca.dat$condition <- exploredds$condition
    Sample <- as.character(exploredds$condition)
    plot <- ggplot2::ggplot(gpca.dat, ggplot2::aes_string("dim1", "dim2")) +
        ggplot2::geom_point(size = 3, aes(color=Sample)) + ggplot2::coord_fixed() + 
        ggplot2::ggtitle("Generalized PCA (GLM-PCA)")
    ## Save plot
    if (savePlot == TRUE){
        ggplot2::ggsave(plot = plot, filename = filePlot)
    }
    ## Return
    if (plotly == TRUE){
        return(plotly::ggplotly(plot)) }
    return(plot)
}

#############
## MDSplot ##
#############
#' @title Multidimensional scaling with MDSplot
#' @description This function computes and plots multidimensional scaling 
#' analysis for dimension reduction of count expression matrix. Internally, it is
#' applied the [stats::dist()] function to the transformed count matrix to get sample-to-sample distances.
#' 
#' @param exploredds object of class [DESeq2::DESeqDataSet()], generated from `exploreDDS` function. 
#' @param method a `character string` indicating which correlation coefficient is to be computed, 
#' based on the [stats::cor()] function. Options are: c("pearson" "kendall", "spearman").
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#' 
#' @return returns an object of `ggplot` or `plotly` class.
#'
#' @examples
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="rlog")
#' MDSplot(exploredds, plotly = FALSE)
#' @export
#' @importFrom DESeq2 DESeqTransform
#' @importFrom ggplot2 ggplot aes_string geom_point scale_y_reverse ggtitle ggsave
#' @importFrom plotly ggplotly
#' @importFrom stats cor dist cmdscale
#' @importFrom SummarizedExperiment assay colData
MDSplot <- function(exploredds, method="spearman", plotly = FALSE, savePlot = FALSE, filePlot = NULL) {
    ## Add validation
    if (!class(exploredds) == "DESeqTransform") {
        warning("'exploredds' needs to be assignes an object of class 'DESeqTransform'.
    Here we are converting the object into a 'DESeqTransform'class for
    downstream analysis. For more information check 'help(exploreDDS)'")
        exploredds <- DESeq2::DESeqTransform(exploredds) }
    ## transformation to a distance matrix
    d <- stats::cor(SummarizedExperiment::assay(exploredds), method = method)
    distmat <- stats::dist(1 - d)
    ## perform MDS
    mdsData <- data.frame(stats::cmdscale(distmat))
    mds <- cbind(mdsData, as.data.frame(SummarizedExperiment::colData(exploredds)))
    Sample <- exploredds$condition
    ## plot
    plot <- ggplot2::ggplot(mds, ggplot2::aes_string("X1", "X2", color=Sample)) + 
        ggplot2::geom_point(size=3) + ggplot2::scale_y_reverse() + 
        ggplot2::ggtitle("Multidimensional Scaling (MDS)")
    ## Save plot
    if (savePlot == TRUE){
        ggplot2::ggsave(plot = plot, filename = filePlot)
    }
    ## Return
    if (plotly == TRUE){
        return(plotly::ggplotly(plot)) }
    return(plot)
}

###############
## tSNEplot ##
###############
#' @title t-Distributed Stochastic Neighbor embedding with tSNEplot
#' @description This function computes and plots t-Distributed Stochastic Neighbor 
#' embedding (t-SNE) analysis for unsupervised nonlinear dimensionality reduction
#' of count expression matrix. Internally, it is applied the [Rtsne::Rtsne()] 
#' function, using the exact t-SNE computing with `theta=0.0`.
#' 
#' @param countMatrix `date.frame` or `matrix` containing raw read counts.
#' @param targets targets `data.frame`.
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#' @param ... additional parameters for the [Rtsne::Rtsne()] function.
#' 
#' @return returns an object of `ggplot` or `plotly` class.
#'
#' @examples
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' set.seed(42) ## Set a seed if you want reproducible results
#' tSNEplot(countMatrix, targets, perplexity = 5)
#'
#' @export
#' @importFrom ggplot2 ggplot aes aes_string geom_point ggtitle ggsave
#' @importFrom plotly ggplotly
#' @importFrom Rtsne Rtsne
tSNEplot <- function(countMatrix, targets, plotly = FALSE, savePlot = FALSE, filePlot = NULL, ...) {
    ## Validations
    if(is.data.frame(countMatrix)) {
        countMatrix <- as.matrix(countMatrix)
    } else if(is.matrix(countMatrix)) {
        countMatrix <- countMatrix
    } else {
        stop("countMatrix needs to be assigned an object of class 'data.frame' OR 'matrix'")
    }
    if (!is.data.frame(targets)) stop("targets needs to be assignes an object of class 'data.frame'")
    ## data manipulation
    countDF_uni <- t(unique(countMatrix)) # removes duplicates and transpose matrix, samples perspective
    set.seed(42)
    tsne_out <- Rtsne::Rtsne(countDF_uni, dims = 2, theta = 0.0, ...)
    targets <- data.frame(targets)
    Sample <- targets$Factor
    plotdata <- data.frame(tsne_x = tsne_out$Y[,1], tsne_y = tsne_out$Y[,2])
    ## Plot
    plot <- ggplot2::ggplot(plotdata, ggplot2::aes_string(x = "tsne_x", y = "tsne_y")) + 
        ggplot2::geom_point(size = 3, aes(color = Sample)) + ggplot2::ggtitle("t-SNE")
    ## Save plot
    if (savePlot == TRUE) {
        ggplot2::ggsave(plot = plot, filename = filePlot)
    }
    ## Return
    if (plotly == TRUE) {
        return(plotly::ggplotly(plot)) }
    return(plot)
}

#############
## MAplot ##
#############

#' @title MAplot
#' @description This function plots log2 fold changes (y-axis) versus the mean 
#' of normalized counts (on the x-axis). Statistically significant features are colored.

#' @param exploredds object of class [DESeq2::DESeqDataSet()], generated from `exploreDDS` function. 
#' @param lfcShrink logiacal. If `TRUE` adds shrunken log2 fold changes (LFC) to the object.
#' @param padj.cutoff filter cutoffs for the p-value adjusted.
#' @param plotly logical: when `FALSE` (default), the `ggplot2` plot will be returned. 
#' `TRUE` option returns the `plotly` version of the plot. 
#' @param savePlot logical: when `FALSE` (default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' [ggplot2::ggsave()] function.
#'
#' @return returns an object of `ggplot` or `plotly` class.
#'
#' @examples
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="raw")
#' MAplot(exploredds, plotly = FALSE)
#' MAplot(exploredds, plotly = TRUE)
#' @export
#' @importFrom DESeq2 results DESeq lfcShrink
#' @importFrom ggplot2 ggplot aes aes_string geom_point scale_colour_manual scale_x_continuous geom_smooth ggsave
#' @importFrom plotly ggplotly
#' @importFrom stats setNames
MAplot <- function(exploredds, lfcShrink= FALSE, padj.cutoff = 0.05, plotly = FALSE, savePlot = FALSE, filePlot = NULL) {
    ## Add validation, need to be raw, counts
    if (!class(exploredds) == "DESeqDataSet") {
        stop("'exploredds' needs to be assignes an object of class 'DESeqDataSet'.
                For more information check 'help(exploreDDS)'")
    }
    ## lfcShrink
    if (lfcShrink == FALSE) {
        res <- DESeq2::results(DESeq2::DESeq(exploredds))
    } else if (lfcShrink == TRUE) {
        resLFC <- DESeq2::lfcShrink(DESeq2::DESeq(exploredds))
    }
    results <- as.data.frame(res)
    if (any(is.na(results$padj))) {
        print("removing NA from the results")
        results[is.na(results)] = 0.99
    }
    ## plot
    plot <- ggplot2::ggplot(results, ggplot2::aes_string(x = "baseMean", y = "log2FoldChange")) + 
        ggplot2::geom_point(ggplot2::aes(colour = padj < padj.cutoff), size = 0.5) +
        ggplot2::scale_colour_manual(name = paste0('padj < ', padj.cutoff), 
                                     values = stats::setNames(c('red','grey'), c(TRUE, FALSE))) +
        ggplot2::scale_x_continuous(trans = "log10", limits = c(0.1,300000)) + 
        ggplot2::geom_smooth(colour = "red")
    ## Save plot
    if (savePlot == TRUE) {
        ggplot2::ggsave(plot = plot, filename = filePlot)
    }
    ## Return
    if (plotly == TRUE) {
        return(suppressWarnings(suppressMessages(plotly::ggplotly(plot)))) }
    return(suppressWarnings(suppressMessages(print(plot))))
}

